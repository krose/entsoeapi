globalVariables(
  names = c(
    "doc_status",
    "TimeSeries.mRID",
    "ts_point_position",
    "ts_resolution",
    "eic_code",
    "eic_long_name",
    "eic_name"
  )
)


#' @title
#' instantiate a memory cache store for maximum 1 hour
#'
#' @param max_age Maximum age of files in cache before they are evicted,
#'                in seconds. Use Inf for no age limit.
#'
#' @importFrom cachem cache_mem
#'
#' @noRd
m <- cache_mem(max_age = .max_age)


#' @title
#' Extract a deeply nested element from a list by a sequence of keys
#'
#' @param .x A list or environment to index into.
#' @param ... A sequence of integer positions or character names specifying the
#'   path to the desired element. Traversal stops and returns `NULL` as soon as
#'   any intermediate node is `NULL`.
#'
#' @return The element at the end of the key path, or `NULL` if any step
#'   along the path evaluates to `NULL`.
#'
#' @noRd
pluck <- function(.x, ...) {
  if (!is.null(.x)) {
    for (key in list(...)) {
      .x <- .x[[key]]
    }
  }
  .x
}


#' @title
#' Wrap a function to capture errors instead of throwing
#'
#' @param .f A function to wrap.
#'
#' @return A new function with the similar (simplified) arguments as `.f`.
#'   When called, it always returns a named list with two elements: `result`
#'   (the return value of `.f`, or `otherwise` on error) and `error` (the
#'   condition object, or `NULL` on success).
#'
#' @noRd
safely <- function(.f) {
  function(...) {
    tryCatch(
      expr = list(result = .f(...), error = NULL),
      error = \(e) list(result = NULL, error = e)
    )
  }
}


#' @title
#' set tibble class without importing tibble
#'
#' @param df the input data.frame
#'
#' @return A [tibble::tibble()].
#'
#' @noRd
as_tbl <- function(df) {
  if (!is.data.frame(df)) df <- as.data.frame(x = df, stringsAsFactors = FALSE)
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}


#' @title
#' Retrieve a cached value or compute, cache, and return it
#'
#' @param key Character scalar. The cache key.
#' @param compute_fn A zero-argument function that produces the value.
#' @param label Character scalar. A human-readable label for CLI messages.
#' @param loud if TRUE emits CLI messages. Defaults to TRUE.
#'
#' @return The cached or freshly computed value associated with `key`.
#'
#' @importFrom cli cli_alert_info cli_h1
#'
#' @noRd
cache_get_or_compute <- function(key, compute_fn, label, loud = TRUE) {
  if (loud) cli_h1("public download")
  if (m$exists(key = key)) {
    if (loud) cli_alert_info("pulling {label} from cache")
    m$get(key = key)
  } else {
    if (loud) cli_alert_info("downloading {label} ...")
    val <- compute_fn()
    m$set(key = key, value = val)
    val
  }
}


#' @title
#' Assert that an EIC code is syntactically valid
#'
#' @description
#' Throws an informative error if `eic` is not a syntactically valid
#' Energy Identification Code (EIC). Validity requires all three conditions
#' to hold: the string must be exactly 16 characters long, the first 15
#' characters must belong to the EIC alphabet (`[A-Z0-9-]`), and the 16th
#' character must match the check character computed by the ENTSO-E
#' weighted-modulo-37 algorithm. Called for its side-effect; returns
#' invisibly on success.
#'
#' @param eic A length-one character string to assert.
#' @param var_name `[character(1)]` Name used for `eic` in error messages.
#'   Passed through to `checkmate` assertion functions.
#' @param null_ok `[logical(1)]` If `TRUE`, `NULL` is accepted without error.
#'
#' @return Invisibly returns `eic`. Called for its side-effect (assertion).
#'
#' @importFrom checkmate assert_string
#' @importFrom stringr str_sub str_split_1
#' @importFrom cli cli_abort
#'
#' @noRd
assert_eic <- function(eic, var_name = "eic", null_ok = FALSE) {
  if (is.null(eic) && isTRUE(null_ok)) return(invisible(eic))

  assert_string(
    x = eic,
    n.chars = 16L,
    pattern = "^[A-Z0-9-]*$",
    .var.name = var_name
  )

  eic_char_vector <- str_sub(string = eic, start = 1L, end = 15L) |>
    str_split_1(pattern = "")
  vals <- possible_eic_chars[eic_char_vector]
  total_sum <- sum(vals * 16L:2L)

  char_idx  <- (36L - (total_sum - 1L) %% 37L) + 1L
  exp_char <- names(possible_eic_chars)[char_idx]
  checksum_char <- str_sub(string = eic, start = 16L, end = 16L)

  if (checksum_char == exp_char) {
    invisible(eic)
  } else {
    cli_abort(c(
      "Invalid EIC checksum character in {.arg {var_name}}",
      "x" = "Expected {.val {exp_char}}, got {.val {checksum_char}}"
    ))
  }
}


#' @title
#' Organize list of strings into group
#'
#' @description
#' This function solves a connected components problem where vectors
#' are connected if they share at least one common string.
#' It returns with groups containing the indices of elements.
#'
#' @param vector_list A list of character vectors to group by shared strings.
#'
#' @return A list of integer vectors, each containing the indices of
#'   `vector_list` elements that belong to the same connected component.
#'
#' @noRd
grouping_by_common_strings <- function(vector_list) {
  n <- length(vector_list)

  if (n == 0L) {
    return(list())
  }
  if (n == 1L) {
    return(list(1L))
  }

  # Build an inverted index: string -> vector indices containing that string
  string_to_indices <- new.env(hash = TRUE)

  for (i in 1L:n) {
    unique_strings <- unique(vector_list[[i]])
    for (s in unique_strings) {
      if (exists(x = s, envir = string_to_indices)) {
        string_to_indices[[s]] <- c(string_to_indices[[s]], i)
      } else {
        string_to_indices[[s]] <- i
      }
    }
  }

  # Union-Find with path compression
  parent <- list2env(x = list(data = 1L:n), parent = emptyenv())

  find_root <- function(i) {
    if (parent$data[i] != i) {
      parent$data[i] <- find_root(i = parent$data[i])
    }
    parent$data[i]
  }

  union_sets <- function(i, j) {
    root_i <- find_root(i)
    root_j <- find_root(j)
    if (root_i != root_j) {
      parent$data[root_j] <- root_i
    }
  }

  # For each string, union all vectors that contain it
  for (s in ls(string_to_indices)) {
    indices <- string_to_indices[[s]]
    if (length(indices) > 1L) {
      for (k in 2L:length(indices)) {
        union_sets(i = indices[1L], j = indices[k])
      }
    }
  }

  # Normalize all parents
  for (i in 1L:n) {
    parent$data[i] <- find_root(i)
  }

  # Group indices by their root parent
  split(x = 1L:n, f = parent$data) |>
    unname()
}


#' @title
#' Calculate the Number of Children for a Given Nodeset
#'
#' @description
#' iterate through XML all children of a given XML nodeset,
#' and detect if they have children as well or not
#' and finally count the number of detected children
#'
#' @param nodeset An `xml_node` whose children are inspected.
#'
#' @return An integer scalar: the count of child nodes that themselves have
#'   at least one child.
#'
#' @importFrom xml2 xml_length xml_children
#'
#' @noRd
number_of_children <- function(nodeset) {
  sum(xml_length(xml_children(nodeset)) > 0L)
}


#' @title
#' Extract the Contents of XML nodesets
#'
#' @description
#' extract the content of the provided XML nodesets,
#' and compose a list of tibbles from them.
#' Used by the EIC helper (en_eic()) for flat nodesets.
#'
#' @param nodesets A list of `xml_node` objects to extract data from.
#' @param prefix Character scalar or `NULL`. An optional prefix prepended to
#'   column names in the resulting tibbles.
#'
#' @return A list of tibbles, one per nodeset element.
#'
#' @importFrom xml2 as_list xml_name
#' @importFrom stringr str_c
#' @importFrom cli cli_warn
#'
#' @noRd
extract_nodesets <- function(nodesets, prefix = NULL) {
  nodesets |>
    lapply(
      \(nodeset) {
        # convert the branches into a named vector
        named_vect <- nodeset |>
          as_list() |>
          unlist(recursive = TRUE)

        # convert the named_vect from NULL to NA if there is no value in it
        if (is.null(named_vect)) named_vect <- NA_character_

        # adjust element names
        names(named_vect) <- str_c(
          prefix,
          xml_name(nodeset),
          names(named_vect),
          sep = "."
        )

        # extract unique vector element names
        unique_names <- names(named_vect) |>
          unique()

        # compose a table from the elements
        vals <- split(x = named_vect, f = names(named_vect))[unique_names]
        max_len <- max(lengths(vals))
        non_conformable <- vals[
          lengths(vals) != max_len & max_len %% lengths(vals) != 0L
        ]
        if (length(non_conformable) > 0L) {
          cli_warn(
            "Some XML column lengths are just dividents of the \\
            maximum ({max_len}) column length; recycling with truncation."
          )
        }
        vals |>
          lapply(\(x) rep_len(x = x, length.out = max_len)) |>
          as_tbl()
      }
    )
}


#' @title
#' Recursively Convert an XML Node into Row Vectors
#'
#' @description
#' Walks the XML tree depth-first, collecting leaf text values directly
#' via xml_text() / xml_name() instead of as_list() + unlist().
#' Returns a list of named character vectors — one per output row.
#' Same-named siblings produce multiple rows (stacked); different-named
#' siblings produce columns (cross-joined). Duplicate leaf names within
#' a single row are collapsed with a "|" separator.
#'
#' This avoids per-node tibble creation entirely; a single tibble is
#' built at the end by rows_to_tbl().
#'
#' @param node An xml_node or length-1 xml_nodeset.
#' @param prefix Character scalar or NULL. Dot-separated path prefix
#'   prepended to all column names produced by this node's subtree.
#'
#' @return A list of named character vectors, each representing one row.
#'
#' @importFrom xml2 xml_children xml_length xml_name xml_text
#'
#' @noRd
node_to_rows <- function(node, prefix = NULL) {
  children <- xml_children(node)
  n_ch <- length(children)

  if (n_ch == 0L) {
    # terminal node: return its text as a single-element named vector
    nm <- if (is.null(prefix)) xml_name(node) else prefix
    val <- xml_text(node)
    if (length(val) == 0L || is.na(val)) val <- NA_character_
    row <- val
    names(row) <- nm
    return(list(row))
  }

  # classify children as leaf (no grandchildren) vs nested
  ch_names <- children |>
    vapply(FUN = xml_name, FUN.VALUE = character(1L))
  ch_has_gc <- xml_length(children) > 0L

  leaf_idx <- which(!ch_has_gc)
  nested_idx <- which(ch_has_gc)

  # --- Build the leaf part as a single named character vector ---
  leaf_row <- character(0L)
  if (length(leaf_idx) > 0L) {
    leaf_names <- leaf_idx |>
      vapply(
        FUN = \(i) {
          sub_children <- xml_children(children[[i]])
          if (length(sub_children) == 0L) {
            # simple text node
            paste(c(prefix, ch_names[i]), collapse = ".")
          } else {
            # leaf with internal structure (multiple text children)
            sub_nms <- sub_children |>
              vapply(FUN = xml_name, FUN.VALUE = character(1L))
            paste(c(prefix, ch_names[i]), collapse = ".") |>
              paste(sub_nms, sep = ".")
          }
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
      )
    leaf_vals <- leaf_idx |>
      vapply(
        FUN = \(i) {
          sub_children <- xml_children(children[[i]])
          if (length(sub_children) == 0L) {
            xml_text(children[[i]])
          } else {
            paste(
              sub_children |>
                vapply(FUN = xml_text, FUN.VALUE = character(1L)),
              collapse = "|"
            )
          }
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
      )
    names(leaf_vals) <- leaf_names

    # handle same-named leaves: group by name
    unique_leaf_names <- unique(leaf_names)
    if (length(unique_leaf_names) < length(leaf_names)) {
      # some leaves share a name — need to determine if they stack or collapse
      name_counts <- tabulate(match(leaf_names, unique_leaf_names))
      has_nested <- length(nested_idx) > 0L

      if (!has_nested && max(name_counts) > 1L) {
        # no nested siblings: same-named leaves produce multiple rows
        # group by name; each group becomes a column, rows = max group size
        groups <- split(leaf_vals, leaf_names)[unique_leaf_names]
        max_len <- lengths(groups) |> max()
        stacked_rows <- vector("list", max_len)
        for (r in seq_len(max_len)) {
          row <- groups |>
            vapply(
              FUN = \(g) if (r <= length(g)) g[[r]] else NA_character_,
              FUN.VALUE = character(1L)
            )
          names(row) <- unique_leaf_names
          stacked_rows[[r]] <- row
        }
        return(stacked_rows)
      } else {
        # nested siblings present or single-row collapse:
        # collapse duplicate names with "|"
        collapsed <- length(unique_leaf_names) |> character()
        names(collapsed) <- unique_leaf_names
        for (nm in unique_leaf_names) {
          collapsed[nm] <- paste(leaf_vals[leaf_names == nm], collapse = "|")
        }
        leaf_row <- collapsed
      }
    } else {
      leaf_row <- leaf_vals
    }
  }

  # --- Recurse into nested children ---
  nested_rows <- list()
  if (length(nested_idx) > 0L) {
    # group nested children by name
    nested_names <- ch_names[nested_idx]
    unique_nested <- unique(nested_names)

    group_row_lists <- vector("list", length(unique_nested))
    for (g in seq_along(unique_nested)) {
      gname <- unique_nested[g]
      g_idx <- nested_idx[nested_names == gname]

      # recurse into each member and collect rows
      member_rows <- vector("list", length(g_idx))
      for (m in seq_along(g_idx)) {
        new_prefix <- paste(c(prefix, gname), collapse = ".")
        member_rows[[m]] <- node_to_rows(
          node = children[[g_idx[m]]],
          prefix = new_prefix
        )
      }
      # same-named nested siblings: stack their rows
      group_row_lists[[g]] <- unlist(member_rows, recursive = FALSE)
    }

    if (length(unique_nested) == 1L) {
      nested_rows <- group_row_lists[[1L]]
    } else {
      # different-named nested groups: cross-join
      # start with first group, then cross with each subsequent group
      nested_rows <- group_row_lists[[1L]]
      for (g in 2L:length(group_row_lists)) {
        next_group <- group_row_lists[[g]]
        crossed <- vector("list", length(nested_rows) * length(next_group))
        k <- 1L
        for (i in seq_along(nested_rows)) {
          for (j in seq_along(next_group)) {
            crossed[[k]] <- c(nested_rows[[i]], next_group[[j]])
            k <- k + 1L
          }
        }
        nested_rows <- crossed
      }
    }
  }

  # --- Combine leaf + nested ---
  if (length(leaf_row) == 0L && length(nested_rows) == 0L) {
    return(list())
  }
  if (length(nested_rows) == 0L) {
    return(list(leaf_row))
  }
  if (length(leaf_row) == 0L) {
    return(nested_rows)
  }

  # prepend leaf columns to every nested row
  lapply(nested_rows, \(nr) c(leaf_row, nr))
}


#' @title
#' Convert a List of Named Character Vectors into a Tibble
#'
#' @description
#' Efficiently converts the row-vector output of node_to_rows() into
#' a single tibble. Uses a fast path (matrix construction) when all
#' rows have identical column names, and a slower fallback (column
#' accumulation with NA fill) when rows are heterogeneous.
#'
#' @param rows A list of named character vectors, as returned by
#'   node_to_rows().
#'
#' @return A tibble with one row per input vector and one column per
#'   unique name found across all vectors.
#'
#' @noRd
rows_to_tbl <- function(rows) {
  n <- length(rows)
  if (n == 0L) return(as_tbl(list()))
  if (n == 1L) {
    row <- rows[[1L]]
    df <- as.list(row)
    return(as_tbl(df))
  }

  # check if all rows have the same column set (fast path)
  col_names_first <- names(rows[[1L]])
  uniform <- TRUE
  for (i in 2L:n) {
    if (!identical(names(rows[[i]]), col_names_first)) {
      uniform <- FALSE
      break
    }
  }

  if (uniform) {
    # fast path: build a character matrix, then split into columns
    ncols <- length(col_names_first)
    mat <- matrix(
      data = unlist(rows, use.names = FALSE),
      nrow = n,
      ncol = ncols,
      byrow = TRUE
    )
    df <- vector("list", ncols)
    names(df) <- col_names_first
    for (j in seq_len(ncols)) {
      df[[j]] <- mat[, j]
    }
    return(as_tbl(df))
  }

  # slow path: collect all unique column names, fill with NA

  all_names <- lapply(rows, names) |> unlist(use.names = FALSE) |> unique()
  ncols <- length(all_names)
  # pre-allocate column vectors
  cols <- vector("list", ncols)
  names(cols) <- all_names
  for (j in seq_len(ncols)) {
    cols[[j]] <- rep(NA_character_, n)
  }
  # fill in values row by row
  for (i in seq_len(n)) {
    row <- rows[[i]]
    rnames <- names(row)
    for (k in seq_along(rnames)) {
      cols[[rnames[k]]][i] <- row[k]
    }
  }
  as_tbl(cols)
}


#' @title
#' Extract Data From an XML Document into Tabular Format
#'
#' @description
#' Mine data from all levels (leaf, twig, branch)
#' of an XML document and convert them to a tibble.
#' Uses node_to_rows() to recursively collect named character
#' vectors (one per output row) and rows_to_tbl() to efficiently
#' construct a single tibble at the end — avoiding per-node tibble
#' creation, as_list() + unlist() overhead, and thousands of
#' intermediate bind_rows()/bind_cols() calls.
#'
#' @param nodesets A list of `xml_node` objects representing the top-level
#'   children of an XML document.
#'
#' @return A tibble combining leaf, twig, and branch data extracted from
#'   `nodesets`.
#'
#' @importFrom xml2 xml_children xml_length xml_name xml_text
#' @importFrom dplyr bind_cols
#'
#' @noRd
extract_leaf_twig_branch <- function(nodesets) {
  # detect the number of children for each element
  children_of_nodes <- nodesets |>
    vapply(FUN = number_of_children, FUN.VALUE = integer(1L))

  # --- first level: leaf nodesets (no children) ---
  leaf_rows <- character(0L)
  if (any(children_of_nodes == 0L)) {
    leaf_ns <- nodesets[children_of_nodes == 0L]

    # expand each leaf nodeset into named character entries;
    # nodes with sub-elements (e.g. timeInterval with start/end)
    # become multiple named entries (one per sub-element)
    leaf_parts <- lapply(leaf_ns, \(ns) {
      nm <- xml_name(ns)
      ch <- xml_children(ns)
      if (length(ch) == 0L) {
        val <- xml_text(ns)
        names(val) <- nm
        val
      } else {
        sub_nms <- ch |>
          vapply(FUN = xml_name, FUN.VALUE = character(1L))
        sub_vals <- ch |>
          vapply(FUN = xml_text, FUN.VALUE = character(1L))
        names(sub_vals) <- paste(nm, sub_nms, sep = ".")
        sub_vals
      }
    })
    leaf_vec <- unlist(leaf_parts, use.names = TRUE)

    # handle duplicate names by collapsing with "|"
    if (anyDuplicated(names(leaf_vec)) > 0L) {
      unique_names <- names(leaf_vec) |> unique()
      collapsed <- length(unique_names) |> character()
      names(collapsed) <- unique_names
      for (nm in unique_names) {
        collapsed[nm] <- paste(leaf_vec[names(leaf_vec) == nm], collapse = "|")
      }
      leaf_rows <- collapsed
    } else {
      leaf_rows <- leaf_vec
    }
  }

  # --- second level: nested nodesets (have children) ---
  nested_all_rows <- list()
  if (any(children_of_nodes > 0L)) {
    nested_ns <- nodesets[children_of_nodes > 0L]
    all_rows <- vector("list", length(nested_ns))
    for (i in seq_along(nested_ns)) {
      ns <- nested_ns[[i]]
      all_rows[[i]] <- node_to_rows(
        node = ns,
        prefix = xml_name(ns)
      )
    }
    # same-named nested nodesets stack; different-named cross-join
    nested_names <- nested_ns |>
      vapply(FUN = xml_name, FUN.VALUE = character(1L))
    unique_nested <- unique(nested_names)

    if (length(unique_nested) == 1L) {
      nested_all_rows <- unlist(all_rows, recursive = FALSE)
    } else {
      group_row_lists <- vector("list", length(unique_nested))
      for (g in seq_along(unique_nested)) {
        g_idx <- which(nested_names == unique_nested[g])
        group_row_lists[[g]] <- unlist(all_rows[g_idx], recursive = FALSE)
      }
      # cross-join groups
      nested_all_rows <- group_row_lists[[1L]]
      for (g in 2L:length(group_row_lists)) {
        next_group <- group_row_lists[[g]]
        crossed <- vector(
          "list",
          length(nested_all_rows) * length(next_group)
        )
        k <- 1L
        for (ii in seq_along(nested_all_rows)) {
          for (jj in seq_along(next_group)) {
            crossed[[k]] <- c(nested_all_rows[[ii]], next_group[[jj]])
            k <- k + 1L
          }
        }
        nested_all_rows <- crossed
      }
    }
  }

  # --- combine leaf + nested into final tibble ---
  if (length(leaf_rows) == 0L && length(nested_all_rows) == 0L) {
    return(as_tbl(list()))
  }
  if (length(nested_all_rows) == 0L) {
    return(rows_to_tbl(list(leaf_rows)))
  }
  if (length(leaf_rows) == 0L) {
    return(rows_to_tbl(nested_all_rows))
  }

  # prepend leaf columns to every nested row, then build tibble
  combined <- lapply(nested_all_rows, \(nr) c(leaf_rows, nr))
  rows_to_tbl(combined)
}


#' @title
#' Converts Extracted Data into a Tidy or a Nested format
#'
#' @description
#' In tidy format each record has a calculated 'ts_point_dt_start' timestamp.
#' In nested format each submitted time unit record contains a nested table
#' with detailed data series
#'
#' @param tbl A tibble produced by `xml_to_table()` containing time-series
#'   columns (e.g. `ts_point_position`, `ts_resolution`).
#' @param tidy_output Logical. If `TRUE`, the result is returned in a flat,
#'   tidy format with a calculated `ts_point_dt_start` column. If `FALSE`
#'   (the default), time-series point columns are nested into a list-column.
#'
#' @return A tibble in either tidy (flat) or nested format.
#'
#' @importFrom stringr str_detect str_replace_all str_subset str_match_all
#'             str_replace
#' @importFrom dplyr mutate case_when group_by across all_of n ungroup
#'    full_join arrange everything distinct
#' @importFrom lubridate duration
#' @importFrom tidyr fill nest
#' @importFrom cli cli_abort
#'
#' @noRd
tidy_or_not <- function(tbl, tidy_output = FALSE) {
  # detect which columns have 'bid_ts_' prefix and build a rename map
  original_names <- names(tbl)
  bid_ts_mask <- str_detect(
    string = original_names,
    pattern = "^bid_ts_"
  )
  have_bid_ts_col <- any(bid_ts_mask)

  # convert the original 'bid_ts_' column names to 'ts_'
  if (have_bid_ts_col) {
    names(tbl) <- names(tbl) |>
      str_replace_all(
        pattern = "^bid_ts_",
        replacement = "ts_"
      )
  }

  # extract the ts_point_ column names
  ts_point_cols <- str_subset(
    string = names(tbl),
    pattern = "^ts_point_"
  )

  # extract the ts_reason_ column names
  ts_reason_cols <- str_subset(
    string = names(tbl),
    pattern = "^ts_reason_"
  )

  # if there is no ts_point_ column
  if (length(ts_point_cols) == 0L) {
    # convert the original 'bid_ts_' column names back
    if (have_bid_ts_col) {
      # the tbl may have gained/lost columns
      # match by the renamed (ts_) form
      names(tbl)[bid_ts_mask] <- original_names[bid_ts_mask]
    }
    return(tbl)
  }

  # extract curve type from tbl
  curve_type <- subset(
    x = tbl,
    select = str_match_all(
      string = names(tbl),
      pattern = ".*curve_type$"
    ) |>
      unlist()
  ) |>
    unlist() |>
    unique()

  # select the group by columns
  group_cols <- setdiff(
    x = names(tbl),
    y = c(ts_point_cols, ts_reason_cols)
  )

  # calculate 'by' values which will be used to calculate
  # the 'ts_point_dt_start' values
  tbl <- tbl |>
    mutate(
      by = case_when(
        ts_resolution == "PT4S"  ~ "4 sec",
        ts_resolution == "PT1M"  ~ "1 min",
        ts_resolution == "PT15M" ~ "15 mins",
        ts_resolution == "PT30M" ~ "30 mins",
        ts_resolution == "PT60M" ~ "1 hour",
        ts_resolution == "P1D"   ~ "1 DSTday",
        ts_resolution == "P7D"   ~ "7 DSTdays",
        ts_resolution == "P1M"   ~ "1 month",
        ts_resolution == "P3M"   ~ "3 months",
        ts_resolution == "P1Y"   ~ "1 year",
        .default = "n/a"
      )
    )

  # if curve_type is 'A01' or absent, do nothing; otherwise branch by type
  if (length(curve_type) == 0L || curve_type == "A01") {
    # no adjustment needed
  } else if (curve_type == "A03") {
    ts_resolution_requ_length <- ts_resolution_real_length <- ts_mrid <- NULL
    ts_resolution_ok <- ts_time_interval_start <- ts_time_interval_end <- NULL

    # calculate 'ts_resolution_requ_length', 'ts_resolution_real_length'
    # and 'ts_resolution_ok' values
    tbl <- tbl |>
      mutate(
        ts_resolution_requ_length =
          (max(ts_time_interval_end) - min(ts_time_interval_start)) /
          duration(by),
        ts_resolution_real_length = n(),
        ts_resolution_ok =
          ts_resolution_real_length == ts_resolution_requ_length,
        .by = all_of(group_cols)
      )

    # filter on those periods which have missing timeseries
    # data points (ts_point_position)
    tbl_adj <- subset(x = tbl, subset = !ts_resolution_ok)

    # check if there is any need to adjust the timeseries data points
    if (nrow(tbl_adj) > 0L) {
      # remove the to be adjusted rows from the base 'tbl'
      # or in other words stash the records with 'ok' resolution
      tbl <- subset(x = tbl, subset = ts_resolution_ok)

      # create a frame table to adjust the timeseries data points
      # vectorised expansion: rep() repeats each group's scalars by its length,
      # sequence() generates 1:n for each group — no row-wise iteration needed
      unique_combos <- subset(
        x = tbl_adj,
        select = c(
          ts_time_interval_start,
          ts_time_interval_end,
          ts_resolution_requ_length,
          ts_resolution,
          ts_mrid
        )
      ) |>
        distinct()
      lengths <- unique_combos$ts_resolution_requ_length
      frame_tbl <- data.frame(
        ts_time_interval_start = rep(
          unique_combos$ts_time_interval_start, lengths
        ),
        ts_time_interval_end = rep(unique_combos$ts_time_interval_end, lengths),
        ts_point_position = sequence(lengths),
        ts_resolution = rep(unique_combos$ts_resolution, lengths),
        ts_mrid = rep(unique_combos$ts_mrid, lengths),
        stringsAsFactors = FALSE
      ) |>
        as_tbl()

      # full join the adjusted timeseries data points with the frame table
      tbl_adj <- full_join(
        x = tbl_adj,
        y = frame_tbl,
        by = c(
          "ts_time_interval_start",
          "ts_time_interval_end",
          "ts_point_position",
          "ts_resolution",
          "ts_mrid"
        )
      )

      # fill the missing values with the last observation carry forward method
      group_cols_adj <- c("ts_resolution", "ts_mrid")
      tbl_adj <- tbl_adj |>
        group_by(across(all_of(group_cols_adj))) |>
        fill(everything()) |>
        ungroup()

      # append the adjusted timeseries data points to the 'ok' timeseries data
      tbl <- bind_rows(tbl, tbl_adj) |>
        arrange(
          ts_time_interval_start,
          ts_time_interval_end,
          ts_point_position
        )
    }
  } else {
    # hints: https://eepublicdownloads.entsoe.eu/clean-documents/EDI/
    # Library/cim_based/
    # Introduction_of_different_Timeseries_possibilities__curvetypes
    # __with_ENTSO-E_electronic_document_v1.4.pdf
    cli_abort("The curve type is not defined, but {curve_type}!")
  }

  # calculate the 'ts_point_dt_start' values accordingly
  tbl <- tbl |>
    subset(
      subset = !is.na(ts_time_interval_start) & !is.na(ts_point_position)
    ) |>
    mutate(
      ts_point_dt_start = seq.POSIXt(
        from = min(ts_time_interval_start),
        length.out = max(ts_point_position),
        by = unique(by),
      )[ts_point_position] |> # handle the unusual case of any missing period
        as.POSIXct(tz = "UTC"),
      .by = all_of(group_cols)
    )

  # if tidy output is needed, then
  if (isTRUE(tidy_output)) {
    # set the not_needed_cols
    not_needed_cols <- c(
      "ts_point_position", "by", "ts_resolution_requ_length",
      "ts_resolution_real_length", "ts_resolution_ok"
    )

    # remove the not needed columns
    not_needed_cols <- intersect(
      x = not_needed_cols,
      y = names(tbl)
    )
    tbl[not_needed_cols] <- list(NULL)
  } else {
    # set the not_needed_cols
    not_needed_cols <- c(
      "ts_point_dt_start", "by", "ts_resolution_requ_length",
      "ts_resolution_real_length", "ts_resolution_ok"
    )

    # remove the not needed columns
    not_needed_cols <- intersect(
      x = not_needed_cols,
      y = names(tbl)
    )
    tbl[not_needed_cols] <- list(NULL)

    # nest the timeseries data points
    tbl <- nest(
      tbl,
      ts_point = all_of(ts_point_cols)
    )
  }

  # convert the original 'bid_ts_' column names back
  if (have_bid_ts_col) {
    names(tbl) <- names(tbl) |>
      str_replace_all(
        pattern = "^ts_",
        replacement = "bid_ts_"
      )
  }

  # return
  tbl
}


#' @title
#' calculate offset URLs
#'
#' @param reason Character scalar. The reason text returned by the API that
#'   contains the allowed and requested document counts.
#' @param query_string Character scalar. The original API query string to
#'   augment with `offset` parameters.
#'
#' @return A character vector of query strings, each with an appended
#'   `&offset=` value covering all pages of the response.
#'
#' @importFrom stringr str_extract str_match str_remove_all
#' @importFrom cli cli_alert_info
#'
#' @noRd
calc_offset_urls <- function(reason, query_string) {
  # helper: try multiple patterns with capture groups, return first match
  extract_first_int <- function(text, patterns) {
    for (patt in patterns) {
      val <- str_match(string = text, pattern = patt)[, 2L]
      if (!is.na(val)) return(as.integer(val))
    }
    NA_integer_
  }

  # extract the number of the allowed and requested documents
  docs_allowed <- extract_first_int(
    text = reason,
    patterns = c(
      "allowed maximum \\(([0-9]{1,8})\\)",
      "allowed:\\s+([0-9]{1,8})"
    )
  )
  docs_requested <- extract_first_int(
    text = reason,
    patterns = c(
      "number of instances \\(([0-9]{1,8})\\)",
      "requested:\\s([0-9]{1,8})"
    )
  )

  # calculate how many offset round is needed
  all_offset_nr <- ceiling(docs_requested / docs_allowed)
  all_offset_seq <- (seq(all_offset_nr) - 1L) * docs_allowed

  # recompose offset URLs
  cli_alert_info("*** The request has been rephrased. ***")
  query_string <- query_string |>
    str_remove_all(pattern = "\\&offset=[0-9]+")
  paste0(query_string, "&offset=", all_offset_seq)
}


#' @title
#' read XML content from a zip compressed file
#'
#' @param temp_file_path Character scalar. Path to a temporary `.zip` file
#'   containing one or more XML files.
#'
#' @return A list of `xml_document` objects, one per XML file found inside the
#'   archive.
#'
#' @importFrom utils unzip
#' @importFrom xml2 read_xml
#' @importFrom cli cli_alert_success cli_abort
#'
#' @noRd
read_zipped_xml <- function(temp_file_path) {
  # safely decompress zip file into several files on disk
  unzip_safe <- safely(unzip)
  unzipped_files <- unzip_safe(
    zipfile = temp_file_path,
    overwrite = TRUE,
    exdir = dirname(temp_file_path)
  )

  # read the xml content from each the decompressed files
  if (is.null(unzipped_files$error)) {
    unzipped_files$result |>
      lapply(\(x) {
        xml_content <- read_xml(x)
        cli_alert_success("{x} has been read in")
        xml_content
      })
  } else {
    cli_abort(conditionMessage(unzipped_files$error))
  }
}


#' @title
#' call request against the ENTSO-E API and converts the response into xml
#'
#' @param api_scheme Character scalar. URL scheme (default: `"https://"`).
#' @param api_domain Character scalar. API host
#'   (default: `"web-api.tp.entsoe.eu/"`).
#' @param api_name Character scalar. API path prefix (default: `"api?"`).
#' @param query_string Character scalar. The query portion of the URL
#'   (everything after `api?`).
#' @param security_token Character scalar. The ENTSO-E API security token.
#'
#' @return An `xml_document`, or a list of `xml_document` objects when the
#'   response is a ZIP archive or when pagination via offset is triggered.
#'   Throws an error on failure.
#'
#' @importFrom checkmate assert_string
#' @importFrom cli cli_h1 cli_alert cli_alert_success cli_abort
#' @importFrom httr2 request req_method req_user_agent req_verbose req_timeout
#'   resp_status resp_content_type resp_body_raw resp_body_xml resp_body_html
#'   resp_body_json resp_status_desc
#' @importFrom xmlconvert xml_to_list
#' @importFrom stringr str_detect
#'
#' @noRd
api_req <- function(
  api_scheme = .api_scheme, # nolint: object_usage_linter.
  api_domain = .api_domain, # nolint: object_usage_linter.
  api_name = .api_name, # nolint: object_usage_linter.
  query_string = NULL,
  security_token = NULL
) {
  assert_string(query_string)
  assert_string(security_token)
  url <- paste0(
    api_scheme, api_domain, api_name, query_string, "&securityToken="
  )
  cli_h1("API call")
  cli_alert("{url}<...>")

  # retrieve data from the API
  req <- paste0(url, security_token) |>
    request() |>
    req_method(method = "GET") |>
    req_user_agent(string = user_agent_string) |>
    req_verbose(
      header_req = FALSE,
      header_resp = TRUE,
      body_req = FALSE,
      body_resp = FALSE
    ) |>
    req_timeout(seconds = .req_timeout) # nolint: object_usage_linter.
  resp <- req_perform_safe(req = req)

  if (is.null(x = resp$error)) {
    result_obj <- resp$result
    cli_alert_success("response has arrived")

    # if the get request is successful, then ...
    if (resp_status(resp = result_obj) == 200) {
      # retrieve content-type from response headers
      rhct <- resp_content_type(resp = result_obj)
      expt_zip <- c(
        "application/zip",
        "application/octet-stream"
      )
      expt_xml <- c(
        "text/xml",
        "application/xml"
      )

      # if the request is a zip file, then ...
      if (rhct %in% expt_zip) {
        # save raw data to disk from memory
        temp_file_path <- tempfile(fileext = ".zip")
        writeBin(
          object = resp_body_raw(resp = result_obj),
          con = temp_file_path
        )

        # read the xml content from each the decompressed files
        en_cont_list <- read_zipped_xml(temp_file_path = temp_file_path)

        # return with the xml content list
        en_cont_list
      } else if (rhct %in% expt_xml) {
        # read the xml content from the response and return
        result_obj |>
          resp_body_xml(encoding = "UTF-8")
      } else {
        cli_abort(
          "Not known response content-type: {result_obj$headers$`content-type`}"
        )
      }
    }
  } else {
    error_obj <- resp$error

    # retrieve content-type from response headers
    if (isTRUE(error_obj$status == 503)) cli_abort(error_obj$message)
    if (is.null(error_obj$resp)) cli_abort(error_obj$parent$message)
    rhct <- resp_content_type(resp = error_obj$resp)
    expt_html <- c("text/html")
    expt_xml <- c("text/xml", "application/xml")
    expt_json <- c("text/json", "application/json")

    if (rhct %in% expt_html) {
      # extract reason code and text
      response_reason_code <- resp_status(error_obj$resp)
      response_reason_text <- error_obj$resp |>
        resp_body_html(encoding = "utf-8") |>
        xml_to_list() |>
        pluck("body")

      sprintf("%s: %s", response_reason_code, response_reason_text) |>
        cli_abort()
    }

    if (rhct %in% expt_xml) {
      # extract reason from reason text
      response_reason <- error_obj$resp |>
        resp_body_xml(encoding = "utf-8") |>
        xml_to_list() |>
        pluck("Reason")

      if (!inherits(x = response_reason, what = "list") ||
            !identical(names(response_reason), c("code", "text"))) {
        cli_abort(
          paste(
            "{resp_status(error_obj$resp)}:",
            "{resp_status_desc(error_obj$resp)}"
          )
        )
      }

      if (response_reason$code == 999) {
        # check if query offsetting is forbidden
        offset_forbidden <- str_detect(
          string = query_string,
          pattern = sprintf(
            fmt = "(%s|%s|%s|%s|%s|%s)",
            "(?=.*documentType=A63)(?=.*businessType=A(46|85))",
            "(?=.*documentType=A65)(?=.*businessType=A85)",
            "(?=.*documentType=B09)(?=.*StorageType=archive)",
            "documentType=A91",
            "documentType=A92",
            "(?=.*documentType=A94)(?=.*auction.Type=A02)"
          )
        )

        # if offset usage is not forbidden, then ...
        if (isFALSE(offset_forbidden)) {
          # check if offset usage needed
          offset_needed <- str_detect(
            string = response_reason$text,
            pattern = "exceeds the allowed maximum"
          )

          # if offset usage needed and not forbidden, then ...
          if (isTRUE(offset_needed)) {
            # calculate offset URLs
            offset_query_strings <- calc_offset_urls(
              reason = response_reason$text,
              query_string = query_string
            )

            # recursively call the api_req() function itself
            en_cont_list <- offset_query_strings |>
              lapply(
                \(x) api_req(query_string = x, security_token = security_token)
              )

            return(en_cont_list)
          }
        }

        cli_abort(paste(response_reason, collapse = "\n"))
      } else {
        cli_abort("{response_reason$code}: {response_reason$text}")
      }
    }

    if (rhct %in% expt_json) {
      # extract reason from reason text
      response_reason <- error_obj$resp |>
        resp_body_json(encoding = "utf-8") |>
        pluck("uuAppErrorMap", "URI_FORMAT_ERROR")
      cli_abort(response_reason$message)
    }
  }
}


#' @title
#' safely call api_req() function
#'
#' @return A function wrapping `api_req()` via `safely()`, returning
#'   a list with `result` and `error` elements.
#'
#' @noRd
api_req_safe <- safely(api_req)


#' @title
#' safely call req_perform() function
#'
#' @return A function wrapping `httr2::req_perform()` via `safely()`,
#'   returning a list with `result` and `error` elements.
#'
#' @importFrom httr2 req_perform
#'
#' @noRd
req_perform_safe <- safely(req_perform)


#' @title
#' converts the given POSIXct or character timestamp into the acceptable format
#'
#' @param x A `POSIXct` value, a character string in a recognised date-time
#'   format, or `NULL`.
#'
#' @return A character scalar formatted as `"%Y%m%d%H%M"` (UTC), or `NULL`
#'   when `x` is `NULL`.
#'
#' @importFrom lubridate parse_date_time
#' @importFrom cli cli_abort cli_alert_warning
#'
#' @noRd
url_posixct_format <- function(x) {
  if (is.null(x)) return(NULL)

  if (inherits(x = x, what = "POSIXct")) {
    return(strftime(x = x, format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE))
  }

  if (inherits(x = x, what = "character")) {
    y <- parse_date_time(
      x = x,
      orders = c(
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d",
        "%Y.%m.%d %H:%M:%S",
        "%Y.%m.%d %H:%M",
        "%Y.%m.%d",
        "%Y%m%d%H%M%S",
        "%Y%m%d%H%M",
        "%Y%m%d"
      ),
      tz = "UTC",
      quiet = TRUE
    ) |>
      strftime(format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
    if (is.na(y)) {
      cli_abort(
        paste(
          "Only the class POSIXct or '%Y-%m-%d %H:%M:%S' formatted text",
          "are supported by the converter."
        )
      )
    }
    cli_alert_warning("The {x} value has been interpreted as UTC!")
    return(y)
  }

  cli_abort("The argument is not in an acceptable timestamp format!")
}


#' @title
#' downloads approved Energy Identification Codes
#'
#' @description
#' from ENTSO-E Transparency Platform under link "f"
#'
#' @param pd_scheme Character scalar. URL scheme for the public downloads site.
#' @param pd_domain Character scalar. Domain of the public downloads site.
#' @param pd_csv_eic Character scalar. Path segment for the CSV EIC endpoint.
#' @param f Character scalar. The CSV filename to download
#'   (e.g. `"X_eicCodes.csv"`).
#' @param sep Character scalar. The field separator used in the CSV file.
#'
#' @return A tibble of EIC codes parsed from the downloaded CSV file.
#'
#' @importFrom stringr str_replace_all
#' @importFrom utils read.table
#' @importFrom cli cli_abort
#'
#' @noRd
get_eiccodes <- function(
  pd_scheme = .pd_scheme,
  pd_domain = .pd_domain,
  pd_csv_eic = .pd_csv_eic,
  f = NA_character_,
  sep = ";"
) {
  # compose the complete url
  complete_url <- paste0(pd_scheme, pd_domain, pd_csv_eic, f)

  # reading input file into a character vector
  # and replacing erroneous semicolons to commas
  # unfortunately there is no general rule for that,
  # hence it must be set manually!!
  readlines_safe <- safely(readLines)
  content <- suppressWarnings(
    expr = readlines_safe(con = complete_url, encoding = "UTF-8")
  )
  if (is.null(content$error)) {
    lns <- content$result |>
      str_replace_all(c(
        "tutkimustehdas;\\sImatra" = "tutkimustehdas, Imatra",
        "; S\\.L\\.;"              = ", S.L.;",
        "\\$amp;"                  = "&"
      ))

    # reading lines as they would be a csv
    eiccodes <- read.table(
      text = lns,
      sep = sep,
      header = TRUE,
      na.strings = c("", "n / a", "n/a", "N/A", "-", "-------", "."),
      colClasses = "character",
      quote = '"',
      comment.char = ""
    )

    # trimming character columns
    eiccodes <- eiccodes |>
      lapply(\(x) trimws(x = enc2utf8(x = x), which = "both")) |>
      as_tbl()

    # replace empty character fields with NA_character_
    eiccodes[eiccodes == ""] <- NA_character_

    # rename columns to snakecase
    names(eiccodes) <- my_snakecase(eiccodes)

    # return
    eiccodes
  } else {
    cli_abort(content$error$message)
  }
}


#' @title
#' downloads all allocated Energy Identification Codes
#'
#' @description
#' from https://eepublicdownloads.blob.core.windows.net
#'
#' @param pd_scheme Character scalar. URL scheme for the public downloads site.
#' @param pd_domain Character scalar. Domain of the public downloads site.
#' @param pd_alloc_eic Character scalar. Path to the allocated EIC XML file.
#'
#' @return A tibble of all allocated EIC codes with snake_case column names,
#'   enriched with document-status definitions.
#'
#' @importFrom stats setNames
#' @importFrom httr2 request req_url_path_append req_method req_user_agent
#'   req_progress req_verbose req_timeout req_retry resp_body_raw
#' @importFrom xml2 as_xml_document xml_contents xml_children xml_name xml_text
#' @importFrom cli cli_alert_success cli_progress_bar cli_progress_update
#'   cli_abort
#' @importFrom dplyr bind_cols select matches bind_rows rename any_of left_join
#'   relocate
#'
#' @noRd
get_all_allocated_eic <- function(
    pd_scheme = .pd_scheme, # nolint: object_usage_linter
    pd_domain = .pd_domain, # nolint: object_usage_linter
    pd_alloc_eic = .pd_alloc_eic # nolint: object_usage_linter
) {
  # define those variables as NULL which are used under non-standard evaluation
  doc_status_value <- NULL

  # Access lazy-loaded datasets via :: to ensure they are found regardless of
  # whether the package is attached (e.g. during build_readme() callr subprocess
  # where only namespace-loading via :: occurs, not library()-attachment).
  message_types <- entsoeapi::message_types

  # retrieve data XML from the public downloads site
  req <- paste0(pd_scheme, pd_domain) |>
    request() |>
    req_url_path_append(pd_alloc_eic) |>
    req_method(method = "GET") |>
    req_user_agent(string = user_agent_string) |>
    req_progress() |>
    req_verbose(
      header_req = FALSE,
      header_resp = TRUE,
      body_req = FALSE,
      body_resp = FALSE
    ) |>
    req_timeout(seconds = 120) |>
    req_retry(max_tries = 3L, backoff = \(resp) 10)
  resp <- req_perform_safe(req = req)

  if (is.null(resp$error)) {
    cli_alert_success("response has arrived")

    # read the xml content from each the decompressed files
    en_cont <- resp$result |>
      resp_body_raw() |>
      rawToChar() |>
      as_xml_document()

    # convert XML to table
    result_tbl <- tryCatch(
      expr = {
        nodesets <- xml_contents(x = en_cont)

        # detect the number of children for each element
        children_of_nodes <- nodesets |>
          vapply(FUN = number_of_children, FUN.VALUE = integer(1L))

        # compose a sub table from the first level data
        first_level_tbl <- nodesets[children_of_nodes == 0L] |>
          extract_nodesets() |>
          bind_cols()

        # remove the not needed columns from the first_level_tbl
        not_needed_patt <- paste(
          "^(sender|receiver)_MarketParticipant\\.",
          "^mRID$|^type$",
          sep = "|"
        )
        first_level_tbl <- first_level_tbl |>
          select(!matches(match = not_needed_patt))

        # compose a sub table from the second level data
        second_level_length <- nodesets[children_of_nodes > 0L] |>
          length()
        prb_envir2 <- parent.frame()
        cli_progress_bar(
          name = "converting",
          total = second_level_length,
          .envir = prb_envir2
        )
        second_level_tbl <- nodesets[children_of_nodes > 0L] |>
          lapply(
            \(x) {
              cli_progress_update(.envir = prb_envir2)
              # extract names and values directly via xml2 (C-level)
              children <- xml_children(x)
              nms <- xml_name(children)
              vals <- xml_text(children)
              # collapse duplicated names
              if (anyDuplicated(nms)) {
                unique_nms <- unique(nms)
                vals <- unique_nms |>
                  vapply(
                    FUN = \(nm) paste(vals[nms == nm], collapse = " - "),
                    FUN.VALUE = character(1L)
                  )
                nms <- unique_nms
              }
              # return as named list (defer tibble creation to bind_rows)
              setNames(as.list(vals), nms)
            }
          ) |>
          Filter(f = length) |>
          bind_rows()

        # combine the first level and the second levels tables together
        bind_cols(first_level_tbl, second_level_tbl)
      },
      error = \(e) {
        cli_abort(
          "The XML document has an unexpected tree structure! {e}"
        )
      }
    )

    if (nrow(result_tbl) == 0L) {
      cli_abort("The XML document has an unexpected tree structure!")
    }

    # rename columns to snakecase
    result_tbl <- result_tbl |>
      rename(any_of(c(
        eic_code = "mRID",
        docStatusValue = "docStatus"
      )))
    names(result_tbl) <- my_snakecase(result_tbl)

    # replace empty character fields with NA_character_
    result_tbl[result_tbl == ""] <- NA_character_

    # add eic_code_doc_status definitions to codes
    result_tbl <- left_join(
      x = result_tbl,
      y = message_types |>
        subset(select = c("code", "title")) |>
        setNames(nm = c("doc_status_value", "doc_status")),
      by = "doc_status_value"
    ) |>
      relocate(doc_status, .after = doc_status_value)

    # return with the xml content list
    as_tbl(result_tbl)
  } else {
    cli_abort("{resp$error$message} {req$url}")
  }
}


#' @title
#' an own version of snakecase::to_snakecase() function
#'
#' @description
#' read and convert the column names of the provided data frame
#' into the required snakecase format
#'
#' @param tbl A data.frame whose column names are to be converted.
#'
#' @return A character vector of snake_case column names, with
#'   domain-specific adjustments applied.
#'
#' @importFrom checkmate assert_data_frame
#' @importFrom stringr str_replace_all
#' @importFrom snakecase to_snake_case
#'
#' @noRd
my_snakecase <- function(tbl) {
  assert_data_frame(x = tbl)
  names(tbl) |>
    str_replace_all(
      c(
        "mRID" = "mrid",
        "TimeSeries" = "ts",
        "^process" = "",
        "unavailability_Time_Period" = "unavailability",
        "ts.[pP]roduction_RegisteredResource.pSRType" = "ts.production",
        "ts.[pP]roduction_RegisteredResource" = "ts.production",
        "ts.[aA]sset_RegisteredResource.pSRType" = "ts.asset",
        "ts.[aA]sset_RegisteredResource" = "ts.asset",
        "[pP]owerSystemResources" = "psr",
        "eICCode" = "eicCode",
        "aCERCode" = "acerCode",
        "vATCode" = "vatCode",
        "eICParent" = "eic_parent",
        "eICResponsible" = "eicResponsible",
        "EICCode_MarketDocument" = "eicCode"
      )
    ) |>
    to_snake_case() |>
    str_replace_all(
      c(
        "psr_type_psr_type" = "psr_type",
        "asset_psr_type" = "psr_type",
        "_direction_direction" = "_direction",
        "eic_code_eic_code_" = "eic_code_",
        "_names_name" = "_name",
        "ts_mkt_psr_type_voltage_psr_" = "",
        "_(wind|solar)_power_feedin" = "",
        "ts_period_" = "ts_",
        "_quantity_quantity" = "_quantity",
        "_market_product_market_product" = "_market_product",
        "_attribute_instance_component" = "",
        "ts_type_market_agreement_type" = "market_agreement_type",
        "ts_contract_market_agreement_type" = "market_agreement_type",
        "ts_point_constraint_ts_" = "constraint_ts_",
        "ts_point_constraint_" = "",
        "ts_point_imbalance_" = "imbalance_",
        "ts_point_procurement_" = "procurement_",
        "ts_point_financial_" = "financial_",
        "financial_price_price_" = "financial_price_",
        "ts_monitored_registered_resource_" = "ts_monitored_",
        "_ptdf_domain_p_tdf_quantity" = "_ptdf_domain_quantity",
        "_flow_based_study_domain_flow_based_margin_quantity" =
          "_flow_based_study_domain_margin_quantity",
        "attribute_instance_component_attribute" =
          "instance_component_attribute",
        "last_request_date_and_or_time_date" = "last_request_date",
        "eic_responsible_market_participant_mrid" =
          "responsible_market_participant_mrid",
        "eic_code_market_participant_vat_code_name" =
          "market_participant_vat_code_name",
        "eic_code_market_participant_acer_code_name" =
          "market_participant_acer_code_name",
        "eic_parent_market_document_mrid" = "parent_market_document_mrid",
        "ts_quantity_measurement_unit_name" = "ts_quantity_measure_unit_name"
      )
    )
}


#' @title
#' Merge a lookup table onto a table by key column
#'
#' @param x A data.frame to enrich.
#' @param y A lookup data.frame containing source columns.
#' @param from_cols Character vector of length 2: the source column names in `y`
#'   (key column first, value column second).
#' @param to_cols Character vector of length 2: the target column names to use
#'   after renaming (key column first, value column second).
#'
#' @return A data.frame: `x` left-joined with the renamed subset of `y`.
#'
#' @importFrom dplyr left_join
#'
#' @noRd
lookup_merge <- function(x, y, from_cols, to_cols) {
  y <- y |>
    subset(select = from_cols)
  names(y) <- to_cols
  left_join(
    x = x,
    y = y,
    by = to_cols[[1L]],
    suffix = c("_x", "_y")
  )
}


#' @title
#' add type names to codes
#'
#' @param tbl A tibble containing type-code columns (e.g. `type`,
#'   `ts_business_type`) to enrich with human-readable definitions.
#'
#' @return The input tibble with additional `*_def` columns appended for
#'   each matched type-code column.
#'
#' @importFrom cli cli_alert_info
#'
#' @noRd
add_type_names <- function(tbl) {
  # Access lazy-loaded datasets via :: to ensure they are found regardless of
  # whether the package is attached (e.g. during build_readme() callr subprocess
  # where only namespace-loading via :: occurs, not library()-attachment).
  asset_types <- entsoeapi::asset_types
  auction_types <- entsoeapi::auction_types
  business_types <- entsoeapi::business_types
  contract_types <- entsoeapi::contract_types
  direction_types <- entsoeapi::direction_types
  energy_product_types <- entsoeapi::energy_product_types
  message_types <- entsoeapi::message_types
  process_types <- entsoeapi::process_types
  role_types <- entsoeapi::role_types
  price_category_types <- entsoeapi::price_category_types
  price_component_types <- entsoeapi::price_component_types

  # specification: col -> lookup table, definition column name
  type_specs <- list(
    list(
      col = "type",
      lookup = message_types,
      def = "type_def"
    ),
    list(
      col = "ts_business_type",
      lookup = business_types,
      def = "ts_business_type_def"
    ),
    list(
      col = "ts_mkt_psr_type",
      lookup = asset_types,
      def = "ts_mkt_psr_type_def"
    ),
    list(
      col = "ts_asset_psr_type",
      lookup = asset_types,
      def = "ts_asset_psr_type_def"
    ),
    list(
      col = "ts_production_psr_type",
      lookup = asset_types,
      def = "ts_production_psr_type_def"
    ),
    list(
      col = "process_type",
      lookup = process_types,
      def = "process_type_def"
    ),
    list(
      col = "ts_product",
      lookup = energy_product_types,
      def = "ts_product_def"
    ),
    list(
      col = "market_agreement_type",
      lookup = contract_types,
      def = "market_agreement_type_def"
    ),
    list(
      col = "ts_auction_type",
      lookup = auction_types,
      def = "ts_auction_type_def"
    ),
    list(
      col = "subject_market_participant_market_role_type",
      lookup = role_types,
      def = "subject_market_participant_market_role_type_def"
    ),
    list(
      col = "bid_ts_flow_direction",
      lookup = direction_types,
      def = "bid_ts_flow_direction_def"
    ),
    list(
      col = "financial_price_direction",
      lookup = direction_types,
      def = "financial_price_direction_def"
    ),
    list(
      col = "imbalance_price_category",
      lookup = price_category_types,
      def = "imbalance_price_category_def"
    ),
    list(
      col = "financial_price_descriptor_type",
      lookup = price_component_types,
      def = "financial_price_descriptor_type_def"
    )
  )

  tbl_names <- names(tbl)
  affected_cols <- character(0)

  for (spec in type_specs) {
    if (spec$col %in% tbl_names) {
      affected_cols <- c(affected_cols, spec$col)
      tbl <- lookup_merge(
        x = tbl,
        y = spec$lookup,
        from_cols = c("code", "title"),
        to_cols = c(spec$col, spec$def)
      )
    }
  }

  if (length(affected_cols) > 0L) {
    cli_alert_success("Additional type names have been added!")
  }

  tbl
}


#' @title
#' get & adjust resource_object_eic() table
#'
#' @return A tibble with columns `ts_registered_resource_mrid` and
#'   `ts_registered_resource_name`, derived from `resource_object_eic()` and
#'   cached in memory.
#'
#' @importFrom dplyr rename_with rename
#' @importFrom snakecase to_snake_case
#'
#' @noRd
get_resource_object_eic <- function() {
  # define those variables as NULL which are used under non-standard evaluation
  eic_code <- eic_long_name <- NULL

  cache_get_or_compute(
    key = "resource_object_eic_name_key",
    label = "resource_object_eic table",
    compute_fn = function() {
      resource_object_eic() |>
        subset(select = c("eic_code", "eic_long_name")) |>
        rename_with(to_snake_case) |>
        rename(
          ts_registered_resource_mrid = eic_code,
          ts_registered_resource_name = eic_long_name
        )
    }
  )
}


#' @title
#' add names to EIC codes
#'
#' @param tbl A tibble containing EIC mRID columns (e.g.
#'   `ts_in_domain_mrid`) to enrich with human-readable names.
#'
#' @return The input tibble with additional `*_name` columns appended for
#'   each matched EIC mRID column, or an empty `data.frame` when `tbl` is
#'   `NULL`.
#'
#' @importFrom dplyr select any_of
#' @importFrom cli cli_alert_success
#' @importFrom stringr str_subset str_replace
#'
#' @noRd
add_eic_names <- function(tbl) {
  if (is.null(tbl)) return(data.frame())

  affected_cols <- c()

  # special case: ts_registered_resource_mrid uses a different lookup table
  if ("ts_registered_resource_mrid" %in% names(tbl)) {
    resource_object_eic <- get_resource_object_eic()
    affected_cols <- c(affected_cols, "ts_registered_resource_mrid")
    tbl <- tbl |>
      select(!any_of("ts_registered_resource_name")) |>
      merge(
        y = resource_object_eic,
        by = "ts_registered_resource_mrid",
        all.x = TRUE
      )
  }

  # specification: eic_code column -> eic_name column
  eic_specs <- list(
    c("ts_bidding_zone_domain_mrid",
      "ts_bidding_zone_domain_name"),
    c("ts_in_bidding_zone_domain_mrid",
      "ts_in_bidding_zone_domain_name"),
    c("ts_out_bidding_zone_domain_mrid",
      "ts_out_bidding_zone_domain_name"),
    c("ts_in_domain_mrid",
      "ts_in_domain_name"),
    c("ts_out_domain_mrid",
      "ts_out_domain_name"),
    c("area_domain_mrid",
      "area_domain_name"),
    c("control_area_domain_mrid",
      "control_area_domain_name"),
    c("ts_acquiring_domain_mrid",
      "ts_acquiring_domain_name"),
    c("ts_connecting_domain_mrid",
      "ts_connecting_domain_name"),
    c("bid_ts_acquiring_domain_mrid",
      "bid_ts_acquiring_domain_name"),
    c("bid_ts_connecting_domain_mrid",
      "bid_ts_connecting_domain_name"),
    c("domain_mrid",
      "domain_name"),
    c("constraint_ts_monitored_ptdf_domain_mrid",
      "constraint_ts_monitored_ptdf_domain_name")
  )

  tbl_names <- names(tbl)

  specs_idx <- eic_specs |>
    vapply(FUN = \(es) pluck(es, 1L) %in% tbl_names, FUN.VALUE = TRUE) |>
    which()
  if (length(specs_idx) > 0L) {
    area_eic_name <- cache_get_or_compute(
      key = "area_eic_name_key",
      label = "area_eic_name table",
      compute_fn = \() {
        fetch_eic_csv(
          csv_file = "Y_eicCodes.csv",
          cache_key = "area_eic_df_key"
        ) |>
          subset(select = c("eic_code", "eic_long_name")) |>
          mutate(
            eic_name = str_c( # nolint: object_usage_linter.
              eic_long_name, # nolint: object_usage_linter.
              collapse = " - "
            ),
            .by = eic_code # nolint: object_usage_linter.
          ) |>
          select(eic_code, eic_name) # nolint: object_usage_linter.
      },
      loud = FALSE
    )

    for (spec in eic_specs[specs_idx]) {
      tbl <- lookup_merge(
        x = tbl,
        y = area_eic_name,
        from_cols = c("eic_code", "eic_name"),
        to_cols = spec
      )
    }
    cli_alert_success("Additional eic names have been added!")
  }

  tbl
}


#' @title
#' add definitions to codes
#'
#' @param tbl A tibble containing code columns (e.g. `doc_status_value`,
#'   `reason_code`) to enrich with human-readable definitions.
#'
#' @return The input tibble with additional definition columns appended for
#'   each matched code column, or an empty `data.frame` when `tbl` is `NULL`.
#'
#' @importFrom stringr str_subset str_replace
#' @importFrom tidyr unite
#' @importFrom dplyr all_of
#' @importFrom cli cli_alert_success
#'
#' @noRd
add_definitions <- function(tbl) {
  if (is.null(tbl)) return(data.frame())

  # pre-define lookup tables
  category_types <- entsoeapi::category_types
  direction_types <- entsoeapi::direction_types
  message_types <- entsoeapi::message_types
  reason_code_types <- entsoeapi::reason_code_types
  object_aggregation_types <- entsoeapi::object_aggregation_types

  # specification for simple definition merges (order matters for column order)
  def_specs <- list(
    list(
      col = "doc_status_value",
      lookup = message_types,
      def = "doc_status"
    ),
    list(
      col = "ts_auction_category",
      lookup = category_types,
      def = "ts_auction_category_def"
    ),
    list(
      col = "ts_flow_direction",
      lookup = direction_types,
      def = "ts_flow_direction_def"
    ),
    list(
      col = "ts_object_aggregation",
      lookup = object_aggregation_types,
      def = "ts_object_aggregation_def"
    )
  )

  tbl_names <- names(tbl)
  affected_cols <- c()
  specs_idx <- def_specs |>
    vapply(FUN = \(ds) pluck(ds, 1L) %in% tbl_names, FUN.VALUE = TRUE) |>
    which()

  if (length(specs_idx) > 0L) {
    for (spec in def_specs[specs_idx]) {
      affected_cols <- c(affected_cols, spec$col)
      tbl <- lookup_merge(
        x = tbl,
        y = spec$lookup,
        from_cols = c("code", "title"),
        to_cols = c(spec$col, spec$def)
      )
    }
  }

  # check existence of reason_code column
  merge_reason_codes <- function(tbl, prefix, affected_cols) {
    code_cols <- str_subset(
      string = names(tbl),
      pattern = paste0("^", prefix, "reason_code(|_[0-9])")
    )
    if (length(code_cols) == 0L) return(list(tbl = tbl, cols = affected_cols))
    for (rc_col in code_cols) {
      affected_cols <- c(affected_cols, rc_col)
      tbl <- lookup_merge(
        x = tbl,
        y = reason_code_types,
        from_cols = c("code", "title"),
        to_cols = c(
          rc_col,
          str_replace(string = rc_col, pattern = "_code", replacement = "_text")
        )
      )
    }
    text_cols <- str_subset(
      string = names(tbl),
      pattern = paste0("^", prefix, "reason_text(|_[0-9|_x|_y])")
    )
    if (length(text_cols) > 1L) {
      tbl <- tbl |>
        unite(
          col = !!paste0(prefix, "reason_text"),
          all_of(text_cols),
          sep = " - ",
          remove = TRUE,
          na.rm = TRUE
        ) |>
        unite(
          col = !!paste0(prefix, "reason_code"),
          all_of(code_cols),
          sep = " - ",
          remove = TRUE,
          na.rm = TRUE
        )
    }
    list(tbl = tbl, cols = affected_cols)
  }
  rc_result <- merge_reason_codes(
    tbl = tbl, prefix = "", affected_cols = affected_cols
  )
  tbl <- rc_result$tbl
  affected_cols <- rc_result$cols
  rc_result <- merge_reason_codes(
    tbl = tbl, prefix = "ts_", affected_cols = affected_cols
  )
  tbl <- rc_result$tbl
  affected_cols <- rc_result$cols

  if (length(affected_cols) > 0L) {
    cli_alert_success("Additional definitions have been added!")
  }

  tbl
}


#' @title
#' convert xml content to table new version
#'
#' @param xml_content An `xml_document` to convert.
#' @param tidy_output Logical. If `TRUE`, the result is returned in a flat,
#'   tidy format. If `FALSE` (the default), time-series point columns are
#'   nested.
#'
#' @return A tibble with snake_case column names, enriched with type
#'   definitions, EIC names, and additional definitions.
#'
#' @importFrom xml2 xml_contents
#' @importFrom cli cli_abort cli_progress_bar cli_progress_update
#' @importFrom stringr str_subset str_remove_all str_c str_detect
#' @importFrom dplyr select all_of mutate across matches arrange
#'
#' @noRd
xml_to_table <- function(xml_content, tidy_output = FALSE) {
  is_xml_document <- inherits(x = xml_content, what = "xml_document")
  if (isFALSE(is_xml_document)) {
    cli_abort("The 'xml_content' should be an xml document!")
  }

  # extract nodesets from the XML document and process
  result_tbl <- tryCatch(
    expr = xml_contents(xml_content) |> extract_leaf_twig_branch(),
    error = \(e) {
      cli_abort("The XML document has an unexpected tree structure! {e}")
    }
  )

  # merge the related date and time columns into datetime column
  for (pref in c("start_", "end_")) {
    date_col <- str_subset(
      string = names(result_tbl),
      pattern = paste0(pref, "DateAndOrTime\\.date")
    )
    time_col <- str_subset(
      string = names(result_tbl),
      pattern = paste0(pref, "DateAndOrTime\\.time")
    )
    if (length(date_col) == 1L && length(time_col) == 1L) {
      datetime_col <- date_col |>
        str_remove_all(pattern = "AndOr|\\.date$")
      result_tbl[[datetime_col]] <- str_c(
        result_tbl[[date_col]],
        result_tbl[[time_col]],
        sep = "T"
      )
      result_tbl <- result_tbl |>
        select(!all_of(c(date_col, time_col)))
    }
  }

  # convert datetime-like columns to POSIXct and numeric-like columns to numeric
  result_tbl <- result_tbl |>
    mutate(
      across(
        matches("[t|T]ime$|start$|end$"),
        ~ as.POSIXct(
          x = .x,
          tryFormats = c(
            "%Y-%m-%dT%H:%MZ", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%OSZ"
          ),
          tz = "UTC"
        )
      )
    ) |>
    mutate(
      across(
        matches(
          match = "number$|position$|quantity$|nominalP$|amount$",
          ignore.case = TRUE
        ),
        ~ as.numeric(x = .x)
      )
    )

  # if 'TimeSeries.mRID' can be converted to numeric then convert it
  if ("TimeSeries.mRID" %in% names(result_tbl)) {
    ts_mrid_is_num <- str_detect(
      string = result_tbl[["TimeSeries.mRID"]],
      pattern = "^[0-9]+$"
    ) |>
      all()
    if (ts_mrid_is_num) {
      result_tbl <- result_tbl |>
        mutate(
          TimeSeries.mRID = as.numeric(x = TimeSeries.mRID)
        )
    }
  }

  # rename columns to snakecase
  names(result_tbl) <- my_snakecase(tbl = result_tbl)

  # adjust the table according to tidy_output value
  result_tbl <- tidy_or_not(tbl = result_tbl, tidy_output = tidy_output)

  # add type names to codes
  result_tbl <- add_type_names(tbl = result_tbl)

  # add eic names to eic codes
  result_tbl <- add_eic_names(tbl = result_tbl)

  # add definitions to codes
  result_tbl <- add_definitions(tbl = result_tbl)

  # select and reorder columns
  needed_cols <- c(
    "ts_bidding_zone_domain_mrid",
    "ts_bidding_zone_domain_name",
    "ts_in_bidding_zone_domain_mrid",
    "ts_in_bidding_zone_domain_name",
    "ts_out_bidding_zone_domain_mrid",
    "ts_out_bidding_zone_domain_name",
    "domain_mrid",
    "domain_name",
    "area_domain_mrid",
    "area_domain_name",
    "control_area_domain_mrid",
    "control_area_domain_name",
    "ts_in_domain_mrid", "ts_in_domain_name",
    "ts_out_domain_mrid", "ts_out_domain_name",
    "ts_production_mrid", "ts_production_name",
    "ts_production_psr_mrid",
    "ts_production_psr_name",
    "ts_connecting_domain_mrid",
    "ts_connecting_domain_name",
    "ts_acquiring_domain_mrid",
    "ts_acquiring_domain_name",
    "bid_ts_connecting_domain_mrid",
    "bid_ts_connecting_domain_name",
    "bid_ts_acquiring_domain_mrid",
    "bid_ts_acquiring_domain_name",
    "bid_ts_mrid", "bid_ts_auction_mrid",
    "ts_product", "ts_product_def",
    "doc_status_value", "doc_status",
    "ts_mkt_psr_type_psr_mrid",
    "ts_mkt_psr_type_psr_name",
    "ts_registered_resource_mrid",
    "ts_registered_resource_name",
    "ts_asset_location_name",
    "ts_asset_mrid", "ts_asset_name",
    "ts_production_mrid", "ts_production_name",
    "ts_production_location_name",
    "subject_market_participant_market_role_type",
    "subject_market_participant_market_role_type_def",
    "type", "type_def",
    "process_type", "process_type_def",
    "market_agreement_type",
    "market_agreement_type_def",
    "ts_auction_mrid", "ts_auction_type",
    "ts_auction_type_def",
    "ts_auction_category",
    "ts_auction_category_def",
    "ts_object_aggregation",
    "ts_object_aggregation_def",
    "ts_flow_direction", "ts_flow_direction_def",
    "bid_ts_flow_direction", "bid_ts_flow_direction_def",
    "financial_price_direction", "financial_price_direction_def",
    "ts_business_type", "ts_business_type_def",
    "ts_mkt_psr_type", "ts_mkt_psr_type_def",
    "ts_asset_psr_type", "ts_asset_psr_type_def",
    "ts_psr_type", "ts_psr_type_def",
    "ts_production_psr_type",
    "ts_production_psr_type_def",
    "created_date_time",
    "reason_code", "reason_text",
    "ts_reason_code", "ts_reason_text",
    "revision_number",
    "time_period_time_interval_start",
    "time_period_time_interval_end",
    "unavailability_time_interval_start",
    "unavailability_time_interval_end",
    "reserve_bid_period_time_interval_start",
    "reserve_bid_period_time_interval_end",
    "ts_resolution", "bid_ts_resolution",
    "ts_available_period_resolution",
    "ts_time_interval_start",
    "ts_time_interval_end",
    "bid_ts_time_interval_start",
    "bid_ts_time_interval_end",
    "ts_mrid", "bid_ts_mrid", "bid_ts_auction_mrid",
    "ts_point", "bid_ts_point", "ts_point_dt_start",
    "bid_ts_point_dt_start",
    "ts_production_psr_nominal_p",
    "ts_point_quantity", "bid_ts_point_quantity",
    "ts_available_period_point_quantity",
    "ts_point_price", "ts_point_price_amount",
    "bid_ts_point_energy_price_amount",
    "imbalance_price_amount",
    "procurement_price_amount",
    "financial_price_amount",
    "ts_point_congestion_cost",
    "ts_currency_unit_name",
    "bid_ts_currency_unit_name",
    "ts_price_measure_unit_name",
    "bid_ts_price_measure_unit_name",
    "ts_quantity_measure_unit_name",
    "bid_ts_quantity_measure_unit_name",
    "high_voltage_limit",
    "imbalance_price_category",
    "imbalance_price_category_def",
    "financial_price_descriptor_type",
    "financial_price_descriptor_type_def",
    "ts_classification_sequence_position",
    "constraint_ts_monitored_ptdf_domain_mrid",
    "constraint_ts_monitored_ptdf_domain_name",
    "constraint_ts_monitored_ptdf_domain_quantity",
    "constraint_ts_monitored_flow_based_study_domain_margin_quantity"
  )
  needed_cols <- intersect(
    x = needed_cols,
    y = names(result_tbl)
  )

  # check if any columns left to keep
  if (length(needed_cols)) {
    # filter on the needed columns
    result_tbl <- result_tbl |>
      select(all_of(needed_cols))

    # reorder the rows
    sort_cols <- intersect(
      x = c(
        "created_date_time", "ts_mrid",
        "ts_business_type", "ts_mkt_psr_type",
        "ts_time_interval_start",
        "ts_point_dt_start"
      ),
      y = names(result_tbl)
    )
    result_tbl <- arrange(
      result_tbl,
      across(all_of(sort_cols))
    )

    # return
    result_tbl
  } else {
    cli_abort("There is no interesting column in the result table!")
  }
}


#' @title
#' extract the response from content list
#'
#' @param content A list with elements `result` and `error`, as returned by
#'   `api_req_safe()`.
#' @param tidy_output Logical. Passed through to `xml_to_table()`. If `TRUE`
#'   (the default), the result is in flat tidy format; if `FALSE`, time-series
#'   point columns are nested.
#' @param progress_bar_limit the minimum length of the XML list by which a
#'                           progress bar to be applied
#'
#' @return A tibble constructed from the XML content in `content$result`.
#'
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#'             cli_abort
#' @importFrom dplyr bind_rows
#'
#' @noRd
extract_response <- function(
  content,
  tidy_output = TRUE,
  progress_bar_limit = 10L
) {
  # guard: check format
  is_in_format <- inherits(x = content, what = "list") &&
    length(content) == 2L && all(names(content) == c("result", "error"))
  if (!is_in_format) {
    cli_abort("The content is not in the required list format!")
  }

  # guard: check for error
  # extract the possible failure reason
  reason <- content$error
  if (!is.null(reason)) {
    cli_abort(reason$message)
  }

  # single xml_document — convert directly
  if (!inherits(x = content$result, what = "list")) {
    return(
      xml_to_table(xml_content = content$result, tidy_output = tidy_output)
    )
  }

  response_length <- length(content$result)
  if (response_length >= progress_bar_limit) {
    prb_envir <- parent.frame()
    cli_progress_bar(
      name = "processing xml list",
      total = response_length,
      .envir = prb_envir
    )
  }

  # list of xml results — convert each element and bind
  result_tbl <- content$result |>
    lapply(
      \(x) {
        if (response_length >= progress_bar_limit) {
          cli_progress_update(.envir = prb_envir)
        }
        if (is.null(x)) return(NULL)
        if (inherits(x = x, what = "list")) {
          all_doc <- x |>
            vapply(FUN = inherits, what = "xml_document", FUN.VALUE = TRUE) |>
            all()
          if (!all_doc) return(NULL)
          x |>
            lapply(xml_to_table, tidy_output = tidy_output) |>
            Filter(f = length) |>
            bind_rows()
        } else {
          xml_to_table(xml_content = x, tidy_output = tidy_output)
        }
      }
    ) |>
    Filter(f = length) |>
    bind_rows()

  if (response_length >= progress_bar_limit) {
    cli_progress_done(.envir = prb_envir)
  }

  # return
  result_tbl
}


#' @title
#' Check if the ENTSO-E API provider is reachable
#'
#' @description
#' Sends a probe request to the ENTSO-E Transparency Platform API and returns
#' `TRUE` if the server responds with HTTP 401 Unauthorized (meaning the
#' endpoint is up but the dummy token was rejected). Returns `FALSE` when there
#' is no internet connection or when the server is unreachable. Primarily
#' intended as an `@examplesIf` guard in package documentation.
#'
#' @param api_scheme Character. URL scheme, default `"https://"`.
#' @param api_domain Character. API host, default `"web-api.tp.entsoe.eu/"`.
#' @param api_name   Character. API path prefix, default `"api?"`.
#'
#' @return A single logical value.
#'
#' @examples
#' there_is_provider()
#'
#' @importFrom httr2 request req_method req_user_agent req_timeout req_retry
#'   resp_status
#'
#' @export
there_is_provider <- function(
  api_scheme = .api_scheme, # nolint: object_usage_linter.
  api_domain = .api_domain, # nolint: object_usage_linter.
  api_name = .api_name # nolint: object_usage_linter.
) {
  req <- paste0(
    api_scheme, api_domain, api_name, "foo=bar&securityToken=baz"
  ) |>
    request() |>
    req_method(method = "GET") |>
    req_user_agent(string = user_agent_string) |>
    req_timeout(seconds = 10L) |>
    req_retry(max_tries = 1L)
  resp <- req_perform_safe(req)
  !is.null(resp$error$resp) && resp_status(resp$error$resp) == 401L
}
