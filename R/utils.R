#' @title
#' instantiate a memory cache store for maximum 1 hour
#'
#' @importFrom cachem cache_mem
#'
#' @noRd
m <- cachem::cache_mem(max_age = 3600)



utils::globalVariables(
  c(
    "allocation_mode_types",
    "analog_types",
    "asset_types",
    "auction_types",
    "business_types",
    "category_types",
    "classification_types",
    "coding_scheme_types",
    "contract_types",
    "coordinate_system_types",
    "currency_types",
    "curve_types",
    "direction_types",
    "eic_types",
    "energy_product_types",
    "fuel_types",
    "hvdc_mode_types",
    "indicator_types",
    "market_product_types",
    "message_types",
    "object_aggregation_types",
    "price_direction_types",
    "process_types",
    "quality_types",
    "reason_code_types",
    "rights_types",
    "role_types",
    "status_types",
    "tariff_types",
    "timeframe_types",
    "transmission_pair_eic_dict",
    "unit_multiplier",
    "unit_of_measure_types",
    "unit_symbol_types",
    "area_eic",
    "m",
    "ts_resolution",
    "ts_time_interval_start",
    "ts_point_position",
    "ts_resolution_requ_length",
    "ts_resolution_real_length",
    "ts_resolution_ok",
    "TimeSeries.mRID"
  )
)



#' @title
#' Organize list of strings into group
#'
#' @description
#' This function solves a connected components problem where vectors
#' are connected if they share at least one common string.
#' It returns with groups containing the indices of elements.
#'
#' @noRd
grouping_by_common_strings <- function(vector_list) {
  n <- length(vector_list)

  if (n == 0) return(list())
  if (n == 1) return(list(1L))

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
  parent <- 1L:n

  find_root <- function(i) {
    if (parent[i] != i) {
      parent[i] <<- find_root(parent[i])
    }
    parent[i]
  }

  union_sets <- function(i, j) {
    root_i <- find_root(i)
    root_j <- find_root(j)
    if (root_i != root_j) {
      parent[root_j] <<- root_i
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
    parent[i] <- find_root(i)
  }

  # Group indices by their root parent
  base::split(x = 1L:n, f = parent) |>
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
#' @noRd
number_of_children <- function(nodeset) {
  have_no_child <- purrr::map_lgl(
    xml2::xml_children(nodeset),
    ~xml2::xml_children(.x) |> unlist(recursive = FALSE) |> is.null()
  )
  sum(have_no_child == FALSE)
}



#' @title
#' Extract the Contents of XML nodesets
#'
#' @description
#' extract the content of the provided XML nodesets,
#' and compose a list of data.tables from them
#'
#' @noRd
extract_nodesets <- function(nodesets, prefix = NULL) {
  purrr::map(
    nodesets,
    \(nodeset) {
      # convert the branches into a named vector
      named_vect <- nodeset |>
        xml2::as_list() |>
        unlist(recursive = TRUE)

      # convert the named_vect from NULL to NA if there is no value in it
      if (is.null(named_vect)) named_vect <- NA_character_

      # adjust element names
      names(named_vect) <- stringr::str_c(
        prefix,
        xml2::xml_name(nodeset),
        names(named_vect),
        sep = "."
      )

      # extract unique vector element names
      unique_names <- names(named_vect) |>
        unique()

      # compose a table from the elements and
      # adjust column names accordingly
      purrr::map(
        unique_names,
        ~named_vect[names(named_vect) == .x]
      ) |>
        data.table::as.data.table() |>
        stats::setNames(nm = unique_names)
    }
  )
}



#' @title
#' Extract Data From an XML Document into Tabular Format
#'
#' @description
#' Mine data from all levels (leaf,twig, branch)
#' of an XML document and convert them to a tibble
#'
#' @noRd
extract_leaf_twig_branch <- function(nodesets) {

  # compose a sub table from first level data
  children_of_nodes <- purrr::map_int(nodesets, number_of_children)
  first_level_tbl <- nodesets[children_of_nodes == 0L] |>
    extract_nodesets() |>
    data.table::as.data.table()

  second_level_tbl <- nodesets[children_of_nodes > 0L] |>
    purrr::map(
      \(scnd_ns) {
        # define an empty list
        compound_tbls <- list()

        # extract child nodesets
        my_xml_paths <- xml2::xml_contents(scnd_ns) |>
          xml2::xml_path()
        child_nodesets <- purrr::map(
          my_xml_paths,
          ~xml2::xml_find_all(x = scnd_ns, xpath = .x)
        )

        # convert the childless child nodes into a table
        ch_children_of_nodes <- purrr::map_int(
          child_nodesets,
          number_of_children
        )
        compound_tbls[[1L]] <- extract_nodesets(
          nodesets = child_nodesets[ch_children_of_nodes == 0L],
          prefix = xml2::xml_name(scnd_ns)
        ) |>
          data.table::as.data.table()

        # convert the grandchild nodes into a table
        nodeset_tbls <- extract_nodesets(
          nodesets = child_nodesets[ch_children_of_nodes > 0L],
          prefix = xml2::xml_name(scnd_ns)
        )
        nodeset_groups <- purrr::map(nodeset_tbls, names) |>
          grouping_by_common_strings()
        if (length(nodeset_groups) == 1L) {
          compound_tbls[[2L]] <- nodeset_tbls |>
            data.table::rbindlist(use.names = TRUE, fill = TRUE)
        } else {
          compound_tbls[[2L]] <- seq_along(nodeset_groups) |>
            purrr::map(
              ~nodeset_tbls[nodeset_groups[[.x]]] |>
                data.table::rbindlist(use.names = TRUE, fill = TRUE)
            ) |>
            dplyr::bind_cols()
        }

        # column-wise append the tables
        compound_tbl <- compound_tbls |>
          purrr::compact() |>
          dplyr::bind_cols()

        compound_tbl

      }
    ) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE)

  list(first_level_tbl, second_level_tbl) |>
    purrr::compact() |>
    dplyr::bind_cols() |>
    tibble::tibble()
}


#' @title
#' Converts Extracted Data into a Tidy or a Nested format
#'
#' @description
#' In tidy format each record has a calculated 'ts_point_dt_start' timestamp.
#' In nested format each submitted time unit record contains a nested table
#' with detailed data series
#'
#' @noRd
tidy_or_not <- function(tbl, tidy_output = FALSE) {

  # detect if there is any 'bid_ts_' column names
  bid_ts_cols <- stringr::str_subset(
    string = names(tbl),
    pattern = "^bid_ts_"
  )

  # convert the original 'bid_ts_' column names to 'ts_'
  if (length(bid_ts_cols) > 0L) {
    names(tbl) <- names(tbl) |>
      stringr::str_replace_all(
        pattern = "^bid_ts_",
        replacement = "ts_"
      )
  }

  # extract the ts_point_ column names
  ts_point_cols <- stringr::str_subset(
    string = names(tbl),
    pattern = "^ts_point_"
  )

  # extract the ts_reason_ column names
  ts_reason_cols <- stringr::str_subset(
    string = names(tbl),
    pattern = "^ts_reason_"
  )

  # if there is no ts_point_ column
  if (length(ts_point_cols) == 0L) {
    # convert the original 'bid_ts_' column names back
    if (length(bid_ts_cols) > 0L) {
      names(tbl) <- names(tbl) |>
        stringr::str_replace_all(
          pattern = "^ts_",
          replacement = "bid_ts_"
        )
    }
    return(tbl)
  }

  # extract curve type from tbl
  curve_type <- base::subset(
    x = tbl,
    select = stringr::str_match_all(
      string = names(tbl),
      pattern = ".*curve_type$"
    ) |>
      unlist()
  ) |>
    unlist() |>
    unique()

  # select the group by columns
  group_cols <- base::setdiff(
    x = names(tbl),
    y = c(ts_point_cols, ts_reason_cols)
  )

  # calculate 'by' values which will be used to calculate
  # the 'ts_point_dt_start' values
  tbl <- tbl |>
    dplyr::mutate(
      by = data.table::fcase(
        ts_resolution == "PT4S", "4 sec",
        ts_resolution == "PT1M", "1 min",
        ts_resolution == "PT15M", "15 mins",
        ts_resolution == "PT30M", "30 mins",
        ts_resolution == "PT60M", "1 hour",
        ts_resolution == "P1D", "1 DSTday",
        ts_resolution == "P7D", "7 DSTdays",
        ts_resolution == "P1M", "1 month",
        ts_resolution == "P1Y", "1 year",
        default = "n/a"
      )
    )

  # if curve_type not defined or 'A01', then
  if (is.null(curve_type) || curve_type == "A01") {

    # do nothing in this case
    Sys.sleep(time = 0)

    # if curve_type is 'A03', then
  } else if (curve_type == "A03") {
    ts_resolution_requ_length <- ts_resolution_real_length <- ts_mrid <- NULL
    ts_resolution_ok <- ts_time_interval_start <- ts_time_interval_end <- NULL

    # calculate 'ts_resolution_requ_length', 'ts_resolution_real_length'
    # and 'ts_resolution_ok' values
    tbl <- tbl |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
      dplyr::mutate(
        ts_resolution_requ_length =
          (max(ts_time_interval_end) - min(ts_time_interval_start)) /
          lubridate::duration(by),
        ts_resolution_real_length = dplyr::n(),
        ts_resolution_ok =
          ts_resolution_real_length == ts_resolution_requ_length
      ) |>
      dplyr::ungroup()

    # filter on those periods which have missing timeseries
    # data points (ts_point_position)
    tbl_adj <- base::subset(x = tbl, subset = !ts_resolution_ok)

    # check if there is any need to adjust the timeseries data points
    if (nrow(tbl_adj) > 0L) {

      # remove the to be adjusted rows from the base 'tbl'
      # or in other words stash the records with 'ok' resolution
      tbl <- base::subset(x = tbl, subset = ts_resolution_ok)

      # create a frame table to adjust the timeseries data points
      frame_tbl <- base::subset(
        x = tbl_adj,
        select = c(
          ts_time_interval_start,
          ts_time_interval_end,
          ts_resolution_requ_length,
          ts_resolution,
          ts_mrid
        )
      ) |>
        unique() |>
        purrr::pmap(
          ~tibble::tibble(
            ts_time_interval_start = ..1,
            ts_time_interval_end = ..2,
            ts_point_position = seq.int(from = 1, to = ..3),
            ts_resolution = ..4,
            ts_mrid = ..5
          )
        ) |>
        data.table::rbindlist(use.names = TRUE, fill = TRUE)

      # full join the adjusted timeseries data points with the frame table
      tbl_adj <- data.table::merge.data.table(
        x = tbl_adj,
        y = frame_tbl,
        by = c(
          "ts_time_interval_start",
          "ts_time_interval_end",
          "ts_point_position",
          "ts_resolution",
          "ts_mrid"
        ),
        all = TRUE
      ) |>
        data.table::as.data.table()

      # fill the missing values with the last observation carry forward method
      group_cols <- c("ts_resolution", "ts_mrid")
      tbl_adj <- tbl_adj |>
        dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
        tidyr::fill(dplyr::everything()) |>
        dplyr::ungroup()

      # append the adjusted timeseries data points to the 'ok' timeseries data
      tbl <- list(tbl, tbl_adj) |>
        data.table::rbindlist(use.names = TRUE, fill = TRUE)
      data.table::setorderv(
        x = tbl,
        cols = c(
          "ts_time_interval_start",
          "ts_time_interval_end",
          "ts_point_position"
        )
      )

    }

  } else {

    # hints: https://eepublicdownloads.entsoe.eu/clean-documents/EDI/
    # Library/cim_based/
    # Introduction_of_different_Timeseries_possibilities__curvetypes
    # __with_ENTSO-E_electronic_document_v1.4.pdf
    stop(
      sprintf("The curve type is not defined, but %s!", curve_type),
      call. = FALSE
    )

  }

  # calculate the 'ts_point_dt_start' values accordingly
  tbl <- tbl |>
    base::subset(
      subset = !is.na(ts_time_interval_start) & !is.na(ts_point_position)
    ) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
    dplyr::mutate(
      ts_point_dt_start = seq.POSIXt(
        from = min(ts_time_interval_start),
        length.out = max(ts_point_position),
        by = unique(by),
      )[ts_point_position] |>  # handle the unusual case of any missing period
        as.POSIXct(tz = "UTC")
    ) |>
    dplyr::ungroup()

  # if tidy output is needed, then
  if (tidy_output == TRUE) {

    # set the not_needed_cols
    not_needed_cols <- c(
      "ts_point_position", "by", "ts_resolution_requ_length",
      "ts_resolution_real_length", "ts_resolution_ok"
    )

    # remove the not needed columns
    not_needed_cols <- base::intersect(
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
    not_needed_cols <- base::intersect(
      x = not_needed_cols,
      y = names(tbl)
    )
    tbl[not_needed_cols] <- list(NULL)

    # nest the timeseries data points
    tbl <- tidyr::nest(
      tbl,
      ts_point = tidyselect::all_of(ts_point_cols)
    )

  }

  # convert the original 'bid_ts_' column names back
  if (length(bid_ts_cols) > 0L) {
    names(tbl) <- names(tbl) |>
      stringr::str_replace_all(
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
#' @noRd
calc_offset_urls <- function(reason, query_string) {
  # extract the number of the allowed documents
  docs_allowed <- stringr::str_extract(
    string = reason,
    pattern = "allowed maximum \\([0-9]{1,8}\\)"
  ) |>
    stringr::str_extract(pattern = "[0-9]{1,8}") |>
    as.integer()
  if (is.na(docs_allowed)) {
    docs_allowed <- stringr::str_extract(
      string = reason,
      pattern = "allowed:\\s+[0-9]{1,8}"
    ) |>
      stringr::str_extract(pattern = "[0-9]{1,8}") |>
      as.integer()
  }

  # extract the number of the requested documents
  docs_requested <- stringr::str_extract(
    string = reason,
    pattern = "number of instances \\([0-9]{1,8}\\)"
  ) |>
    stringr::str_extract(pattern = "[0-9]{1,8}") |>
    as.integer()
  if (is.na(docs_requested)) {
    docs_requested <- stringr::str_extract(
      string = reason,
      pattern = "requested:\\s[0-9]{1,8}"
    ) |>
      stringr::str_extract(pattern = "[0-9]{1,8}") |>
      as.integer()
  }

  # calculate how many offset round is needed
  all_offset_nr <- docs_requested %/% docs_allowed +
    ceiling(docs_requested %% docs_allowed / docs_allowed)
  all_offset_seq <- (seq(all_offset_nr) - 1L) * docs_allowed

  # recompose offset URLs
  message("*** The request has been rephrased. ***")
  query_string <- query_string |>
    gsub(pattern = "\\&offset=[0-9]+", replacement = "")
  paste0(query_string, "&offset=", all_offset_seq)
}



#' @title
#' read XML content from a zip compressed file
#'
#' @noRd
read_zipped_xml <- function(temp_file_path) {
  # safely decompress zip file into several files on disk
  unzip_safe <- purrr::safely(utils::unzip)
  unzipped_files <- unzip_safe(
    zipfile = temp_file_path,
    overwrite = TRUE,
    exdir = fs::path_dir(temp_file_path)
  )

  # read the xml content from each the decompressed files
  en_cont_list <- unzipped_files$result |>
    purrr::map(~{
      xml_content <- xml2::read_xml(.x)
      message(.x, " has read in")
      return(xml_content)
    })

  # return with the xml content list
  en_cont_list

}



#' @title
#' call request against the ENTSO-E API and converts the response into xml
#'
#' @noRd
api_req <- function(
  api_scheme = "https://",
  api_domain = "web-api.tp.entsoe.eu/",
  api_name = "api?",
  query_string = NULL,
  security_token = NULL
) {
  if (is.null(query_string)) {
    stop("The argument 'query_string' is missing!", call. = FALSE)
  }
  if (is.null(security_token)) {
    stop(
      "The argument 'security_token' is not provided!",
      call. = FALSE
    )
  } else {
    # add the canonical API prefix and suffix to the request url
    url <- paste0(
      api_scheme, api_domain, api_name, query_string, "&securityToken="
    )
    cat(url, "<...>\n", sep = "")
  }

  # retrieve data from the API
  req <- httr2::request(base_url = paste0(url, security_token)) |>
    httr2::req_method(method = "GET") |>
    httr2::req_verbose(
      header_req = FALSE,
      header_resp = TRUE,
      body_req = FALSE,
      body_resp = FALSE
    ) |>
    httr2::req_timeout(seconds = 60)
  resp <- "No response."
  resp <- req_perform_safe(req = req)

  if (is.null(x = resp$error)) {
    result_obj <- resp$result
    message("response has arrived")

    # if the get request is successful, then ...
    if (httr2::resp_status(resp = result_obj) == 200) {

      # retrieve content-type from response headers
      rhct <- httr2::resp_content_type(resp = result_obj)
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
          object = httr2::resp_body_raw(resp = result_obj),
          con = temp_file_path
        )

        # read the xml content from each the decompressed files
        en_cont_list <- read_zipped_xml(temp_file_path)

        # return with the xml content list
        en_cont_list

      } else if (rhct %in% expt_xml) {

        # read the xml content from the response and return
        result_obj |>
          httr2::resp_body_xml(encoding = "UTF-8")

      } else {

        stop(
          sprintf("Not known response content-type: %s",
                  result_obj$headers$`content-type`),
          call. = FALSE
        )

      }
    }

  } else {
    error_obj <- resp$error

    # retrieve content-type from response headers
    if (is.null(error_obj$resp)) stop(error_obj$parent$message, call. = FALSE)
    rhct <- httr2::resp_content_type(resp = error_obj$resp)
    expt_html <- c("text/html;charset=UTF-8")
    expt_xml <- c("text/xml", "application/xml")
    expt_json <- c("text/xml", "application/json")

    if (rhct %in% expt_html) {
      # extract reason code and text
      response_reason_code <- httr2::resp_status(error_obj$resp)
      response_reason_text <- error_obj$resp |>
        httr2::resp_body_html(encoding = "utf-8") |>
        xmlconvert::xml_to_list() |>
        purrr::pluck("body")

      stop(
        sprintf("%s: %s", response_reason_code, response_reason_text),
        call. = FALSE
      )
    }

    if (rhct %in% expt_xml) {
      # extract reason from reason text
      response_reason <- error_obj$resp |>
        httr2::resp_body_xml(encoding = "utf-8") |>
        xmlconvert::xml_to_list() |>
        purrr::pluck("Reason")

      if (!all(names(response_reason) == c("code", "text"))) {
        stop(
          sprintf("%s: %s",
                  httr2::resp_status(error_obj$resp),
                  httr2::resp_status_desc(error_obj$resp)),
          call. = FALSE
        )
      }

      if (response_reason$code == 999) {
        # check if query offsetting is forbidden
        offset_forbidden <- stringr::str_detect(
          string = query_string,
          pattern = sprintf(
            fmt = "(%s|%s|%s|%s|%s)",
            "(?=.*documentType=A63)(?=.*businessType=A(46|85))",
            "(?=.*documentType=A65)(?=.*businessType=A85)",
            "(?=.*documentType=B09)(?=.*StorageType=archive)",
            "documentType=A91",
            "documentType=A92"
          )
        )

        # if offset usage is not forbidden, then ...
        if (isFALSE(offset_forbidden)) {
          # check if offset usage needed
          offset_needed <- stringr::str_detect(
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
              purrr::map(
                ~api_req(
                  query_string = .x,
                  security_token = security_token
                )
              )

            return(en_cont_list)
          }
        }

        stop(paste(response_reason, collapse = "\n"), call. = FALSE)
      } else {
        stop(
          sprintf("%s: %s", response_reason$code, response_reason$text),
          call. = FALSE
        )
      }
    }

    if (rhct %in% expt_json) {
      # extract reason from reason text
      response_reason <- error_obj$resp |>
        httr2::resp_body_json(encoding = "utf-8") |>
        purrr::pluck("uuAppErrorMap", "URI_FORMAT_ERROR")
      stop(response_reason$message, call. = FALSE)
    }

  }
}



#' @title
#' safely call api_req() function
#'
#' @noRd
api_req_safe <- purrr::safely(api_req)



#' @title
#' safely call req_perform() function
#'
#' @noRd
req_perform_safe <- purrr::safely(httr2::req_perform)



#' @title
#' converts the given POSIXct or character timestamp into the acceptable format
#'
#' @noRd
url_posixct_format <- function(x) {
  if (is.null(x)) {
    y <- NULL
  } else if (inherits(x = x, what = "POSIXct")) {
    y <- strftime(x = x, format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
  } else if (inherits(x = x, what = "character")) {
    y <- lubridate::parse_date_time(x      = x,
                                    orders = c("%Y-%m-%d %H:%M:%S",
                                               "%Y-%m-%d %H:%M",
                                               "%Y-%m-%d",
                                               "%Y.%m.%d %H:%M:%S",
                                               "%Y.%m.%d %H:%M",
                                               "%Y.%m.%d",
                                               "%Y%m%d%H%M%S",
                                               "%Y%m%d%H%M",
                                               "%Y%m%d"),
                                    tz     = "UTC",
                                    quiet  = TRUE) |>
      strftime(format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
    if (is.na(y)) {
      stop(
        paste(
          "Only the class POSIXct or '%Y-%m-%d %H:%M:%S' formatted text",
          "are supported by the converter."
        ),
        call. = FALSE
      )
    } else {
      warning(
        "The ", x, " value has interpreted as UTC!",
        call. = FALSE
      )
    }
  } else {
    stop(
      "The argument is not in an acceptable timestamp format!",
      call. = FALSE
    )
  }

  y
}



#' @title
#' downloads approved Energy Identification Codes
#'
#' @description
#' from ENTSO-E Transparency Platform under link "f"
#'
#' @noRd
get_eiccodes <- function(
  base_url = "https://eepublicdownloads.blob.core.windows.net/cio-lio/csv/",
  f = NA_character_
) {
  # compose the complete url
  complete_url <- paste0(base_url, f)

  # reading input file into a character vector
  # and replacing erroneous semicolons to commas
  # unfortunately there is no general rule for that,
  # hence it must be set manually!!
  readlines_safe <- purrr::safely(readLines, quiet = TRUE)
  content <- suppressWarnings(
    expr = readlines_safe(
      con = complete_url, encoding = "UTF-8"
    )
  )
  if (is.null(content$error)) {
    lns <- content$result |>
      stringr::str_replace_all(pattern     = "tutkimustehdas;\\sImatra",
                               replacement = "tutkimustehdas, Imatra") |>
      stringr::str_replace_all(pattern     = "; S\\.L\\.;",
                               replacement = ", S.L.;") |>
      stringr::str_replace_all(pattern     = "\\$amp;",
                               replacement = "&")

    # reading lines as they would be a csv
    eiccodes <- data.table::fread(
      text       = lns,
      sep        = ";",
      na.strings = c("", "n / a", "n/a", "N/A", "-", "-------", "."),
      encoding   = "UTF-8"
    )

    # trimming character columns
    eiccodes <- eiccodes |>
      purrr::map(~{
        if (is.character(.x)) {
          utf8::utf8_encode(x = .x) |>
            trimws(which = "both")
        } else {
          .x
        }
      }) |>
      tibble::as_tibble()

    # return
    eiccodes

  } else {

    stop(
      sprintf("cannot open the connection to '%s'!", complete_url),
      call. = FALSE
    )

  }
}



#' @title
#' unpack an xml section into a tabular row
#'
#' @noRd
unpack_xml <- function(section, parent_name = NULL) {
  result_vector <- xml2::as_list(section) |>
    unlist(recursive = TRUE)
  if (is.null(result_vector)) {
    tbl <- tibble::tibble()
  } else {
    names(result_vector) <- stringr::str_c(parent_name,
                                           xml2::xml_name(section),
                                           names(result_vector),
                                           sep = ".")
    tbl <- tibble::as_tibble_row(result_vector)
  }
  # return
  tbl
}



#' @title
#' an own version of snakecase::to_snakecase() function
#'
#' @description
#' read and convert the column names of the provided data frame
#' into the required snakecase format
#'
#' @noRd
my_snakecase <- function(tbl) {
  if (isFALSE(is.data.frame(tbl))) {
    stop(
      "The provided argument is not a valid data frame!",
      call. = FALSE
    )
  }
  names(tbl) |>
    stringr::str_replace_all(
      pattern = "mRID",
      replacement = "mrid"
    ) |>
    stringr::str_replace_all(
      pattern = "TimeSeries",
      replacement = "ts"
    ) |>
    stringr::str_remove(
      pattern = "^process"
    ) |>
    stringr::str_replace_all(
      pattern = "unavailability_Time_Period",
      replacement = "unavailability"
    ) |>
    stringr::str_replace_all(
      pattern = "ts.[p|P]roduction_RegisteredResource.pSRType",
      replacement = "ts.production"
    ) |>
    stringr::str_replace_all(
      pattern = "ts.[p|P]roduction_RegisteredResource",
      replacement = "ts.production"
    ) |>
    stringr::str_replace_all(
      pattern = "ts.[a|A]sset_RegisteredResource.pSRType",
      replacement = "ts.asset"
    ) |>
    stringr::str_replace_all(
      pattern = "ts.[a|A]sset_RegisteredResource",
      replacement = "ts.asset"
    ) |>
    stringr::str_replace_all(
      pattern = "[p|P]owerSystemResources",
      replacement = "psr"
    ) |>
    stringr::str_replace_all(
      pattern = "eICCode",
      replacement = "eicCode"
    ) |>
    stringr::str_replace_all(
      pattern = "aCERCode",
      replacement = "acerCode"
    ) |>
    stringr::str_replace_all(
      pattern = "vATCode",
      replacement = "vatCode"
    ) |>
    stringr::str_replace_all(
      pattern = "eICParent",
      replacement = "eic_parent"
    ) |>
    stringr::str_replace_all(
      pattern = "eICResponsible",
      replacement = "eicResponsible"
    ) |>
    stringr::str_replace_all(
      pattern = "EICCode_MarketDocument",
      replacement = "eicCode"
    ) |>
    stringr::str_replace_all(
      pattern = "eICResponsible",
      replacement = "eicResponsible"
    ) |>
    snakecase::to_snake_case() |>
    stringr::str_replace_all(
      pattern = "psr_type_psr_type",
      replacement = "psr_type"
    ) |>
    stringr::str_replace_all(
      pattern = "asset_psr_type",
      replacement = "psr_type"
    ) |>
    stringr::str_replace_all(
      pattern = "_direction_direction",
      replacement = "_direction"
    ) |>
    stringr::str_replace_all(
      pattern = "eic_code_eic_code_",
      replacement = "eic_code_"
    ) |>
    stringr::str_replace_all(
      pattern = "_names_name",
      replacement = "_name"
    ) |>
    stringr::str_remove(
      pattern = "ts_mkt_psr_type_voltage_psr_"
    ) |>
    stringr::str_remove_all(
      pattern = "_(wind|solar)_power_feedin"
    ) |>
    stringr::str_replace_all(
      pattern = "ts_period_",
      replacement = "ts_"
    ) |>
    stringr::str_replace_all(
      pattern = "_quantity_quantity",
      replacement = "_quantity"
    ) |>
    stringr::str_replace_all(
      pattern = "_market_product_market_product",
      replacement = "_market_product"
    ) |>
    stringr::str_replace_all(
      pattern = "_attribute_instance_component",
      replacement = ""
    ) |>
    stringr::str_replace_all(
      pattern = "ts_point_constraint_ts_",
      replacement = "constraint_ts_"
    ) |>
    stringr::str_remove(
      pattern = "ts_point_constraint_"
    ) |>
    stringr::str_replace_all(
      pattern = "ts_monitored_registered_resource_",
      replacement = "ts_monitored_"
    ) |>
    stringr::str_replace_all(
      pattern = "_ptdf_domain_p_tdf_quantity",
      replacement = "_ptdf_domain_quantity"
    ) |>
    stringr::str_replace_all(
      pattern = "_flow_based_study_domain_flow_based_margin_quantity",
      replacement = "_flow_based_study_domain_margin_quantity"
    )
}



#' @title
#' create a specific merge function which adds the needed definitions
#'
#' @noRd
def_merge <- function(x, y, code_name, definition_name) {
  x <- x |>
    data.table::data.table()
  y <- y |>
    subset(select = c("code", "title")) |>
    data.table::data.table()
  names(y) <- c(code_name, definition_name)
  data.table::merge.data.table(
    x = x,
    y = y,
    by = code_name,
    suffixes = c("_x", "_y"),
    all.x = TRUE
  )
}



#' @title
#' create a specific merge function which adds the EIC names
#'
#' @noRd
eic_name_merge <- function(x, y, eic_code_name, eic_name_name) {
  y <- y |>
    subset(select = c("eic_code", "eic_name")) |>
    data.table::data.table()
  names(y) <- c(eic_code_name, eic_name_name)
  data.table::merge.data.table(
    x = x,
    y = y,
    by = eic_code_name,
    all.x = TRUE
  )
}



#' @title
#' add type names to codes
#'
#' @noRd
add_type_names <- function(tbl) {
  # pre-define some built-in tables to avoid non-standard evaluation issues
  # within the current function
  asset_types <- entsoeapi::asset_types
  business_types <- entsoeapi::business_types
  contract_types <- entsoeapi::contract_types
  message_types <- entsoeapi::message_types
  process_types <- entsoeapi::process_types
  role_types <- entsoeapi::role_types
  direction_types <- entsoeapi::direction_types
  energy_product_types <- entsoeapi::energy_product_types

  # convert tbl to data.table in order to join faster
  tbl <- data.table::data.table(tbl)

  # define an empty vector to collect those column names
  # which will get definitions by add_type_names() function
  affected_cols <- c()

  # add type definitions to codes
  if ("type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "type")
    tbl <- def_merge(
      x = tbl,
      y = message_types,
      code_name = "type",
      definition_name = "type_def"
    )
  }
  if ("ts_business_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_business_type")
    tbl <- def_merge(
      x = tbl,
      y = business_types,
      code_name = "ts_business_type",
      definition_name = "ts_business_type_def"
    )
  }
  if ("ts_mkt_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_mkt_psr_type")
    tbl <- def_merge(
      x = tbl,
      y = asset_types,
      code_name = "ts_mkt_psr_type",
      definition_name = "ts_mkt_psr_type_def"
    )
  }
  if ("ts_asset_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_asset_psr_type")
    tbl <- def_merge(
      x = tbl,
      y = asset_types,
      code_name = "ts_asset_psr_type",
      definition_name = "ts_asset_psr_type_def"
    )
  }
  if ("ts_production_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_production_psr_type")
    tbl <- def_merge(
      x = tbl,
      y = asset_types,
      code_name = "ts_production_psr_type",
      definition_name = "ts_production_psr_type_def"
    )
  }
  if ("process_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "process_type")
    tbl <- def_merge(
      x = tbl,
      y = process_types,
      code_name = "process_type",
      definition_name = "process_type_def"
    )
  }
  if ("ts_product" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_product")
    tbl <- def_merge(
      x = tbl,
      y = energy_product_types,
      code_name = "ts_product",
      definition_name = "ts_product_def"
    )
  }
  if ("ts_contract_market_agreement_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_contract_market_agreement_type")
    tbl <- def_merge(
      x = tbl,
      y = contract_types,
      code_name = "ts_contract_market_agreement_type",
      definition_name = "ts_contract_market_agreement_type_def"
    )
  }
  if ("ts_auction_type" %in% names(tbl)) {
    auction_types <- entsoeapi::auction_types
    affected_cols <- c(affected_cols, "ts_auction_type")
    tbl <- def_merge(
      x = tbl,
      y = auction_types,
      code_name = "ts_auction_type",
      definition_name = "ts_auction_type_def"
    )
  }
  if ("subject_market_participant_market_role_type" %in% names(tbl)) {
    affected_cols <- c(
      affected_cols,
      "subject_market_participant_market_role_type"
    )
    tbl <- def_merge(
      x = tbl,
      y = role_types,
      code_name = "subject_market_participant_market_role_type",
      definition_name = "subject_market_participant_market_role_type_def"
    )
  }
  if ("bid_ts_flow_direction" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "bid_ts_flow_direction")
    tbl <- def_merge(
      x = tbl,
      y = direction_types,
      code_name = "bid_ts_flow_direction",
      definition_name = "bid_ts_flow_direction_def"
    )
  }
  if (length(affected_cols) == 0L) {
    warning("No additional type names added!")
  }

  tbl
}



#' @title
#' get & adjust area_eic() table
#'
#' @description
#' download area_eic() table & convert to data.table
#' in order to join faster
#'
#' @noRd
get_area_eic_name <- function() {
  # define those variables as NULL which are used under non-standard evaluation
  eic_code <- eic_long_name <- eic_name <- NULL

  # compose a local 'download and transform' function
  get_data <- function() {
    area_eic() |>
      subset(select = c("EicCode", "EicLongName")) |>
      dplyr::rename_with(snakecase::to_snake_case) |>
      dplyr::group_by(eic_code) |>
      dplyr::mutate(
        eic_name = stringr::str_c(
          eic_long_name,
          collapse = " - "
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(eic_code, eic_name) |>
      data.table::data.table()
  }

  # check if there is any cached value of 'area_eic_name'
  aen_cache_key <- "area_eic_name_key"
  if (m$exists(key = aen_cache_key)) {

    # recall area_eic_name values
    area_eic_name <- m$get(
      key = aen_cache_key,
      missing = get_data()
    )

  } else {
    # download area_eic() table & convert to data.table
    # in order to join faster
    area_eic_name <- get_data()

    # cache aen_dt as aen_cache_key
    m$set(key = aen_cache_key, value = area_eic_name)
  }

  area_eic_name
}



#' @title
#' get & adjust resource_object_eic() table
#'
#' @description
#' download resource_object_eic() table & convert to data.table
#' in order to join faster
#'
#' @noRd
get_resource_object_eic <- function(
  roe_cache_key = "resource_object_eic_name_key"
) {
  # define those variables as NULL which are used under non-standard evaluation
  eic_code <- eic_long_name <- NULL

  # compose a local 'download and transform' function
  get_data <- function() {
    resource_object_eic() |>
      subset(select = c("EicCode", "EicLongName")) |>
      dplyr::rename_with(snakecase::to_snake_case) |>
      dplyr::rename(
        ts_registered_resource_mrid = eic_code,
        ts_registered_resource_name = eic_long_name
      ) |>
      data.table::data.table()
  }


  # check if there is any cached value of 'area_eic_name'
  if (m$exists(key = roe_cache_key)) {

    # recall resource_object_eic_name values
    resource_object_eic <- m$get(
      key = roe_cache_key,
      missing = get_data()
    )

  } else {
    # download resource_object_eic() table & convert to data.table
    # in order to join faster
    resource_object_eic <- get_data()

    # cache roe_dt as cache_key
    m$set(key = roe_cache_key, value = resource_object_eic)
  }

  # return
  resource_object_eic
}



#' @title
#' add names to EIC codes
#'
#' @noRd
add_eic_names <- function(tbl) {

  # convert tbl to data.table in order to join faster
  tbl <- data.table::data.table(tbl)

  # download & convert area_eic() table to data.table
  # in order to join faster
  area_eic_name <- get_area_eic_name()

  # define an empty vector to collect those EIC column names
  # which will get definitions by add_eic_names() function
  affected_cols <- c()

  # add names to eic codes
  if ("ts_registered_resource_mrid" %in% names(tbl)) {

    # download & convert resource_object_eic() table to data.table
    # in order to join faster
    resource_object_eic <- get_resource_object_eic()

    affected_cols <- c(affected_cols, "ts_registered_resource_mrid")
    tbl <- tbl |>
      dplyr::select(!tidyselect::any_of("ts_registered_resource_name")) |>
      merge(y = resource_object_eic,
            by = "ts_registered_resource_mrid",
            all.x = TRUE)
  }
  if ("ts_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_bidding_zone_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_bidding_zone_domain_mrid",
        eic_name_name = "ts_bidding_zone_domain_name"
      )
  }
  if ("ts_in_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_in_bidding_zone_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_in_bidding_zone_domain_mrid",
        eic_name_name = "ts_in_bidding_zone_domain_name"
      )
  }
  if ("ts_out_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_out_bidding_zone_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_out_bidding_zone_domain_mrid",
        eic_name_name = "ts_out_bidding_zone_domain_name"
      )
  }
  if ("ts_in_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_in_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_in_domain_mrid",
        eic_name_name = "ts_in_domain_name"
      )
  }
  if ("ts_out_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_out_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_out_domain_mrid",
        eic_name_name = "ts_out_domain_name"
      )
  }
  if ("area_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "area_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "area_domain_mrid",
        eic_name_name = "area_domain_name"
      )
  }
  if ("control_area_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "control_area_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "control_area_domain_mrid",
        eic_name_name = "control_area_domain_name"
      )
  }
  if ("ts_acquiring_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_acquiring_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_acquiring_domain_mrid",
        eic_name_name = "ts_acquiring_domain_name"
      )
  }
  if ("ts_connecting_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_connecting_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_connecting_domain_mrid",
        eic_name_name = "ts_connecting_domain_name"
      )
  }
  if ("bid_ts_acquiring_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "bid_ts_acquiring_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "bid_ts_acquiring_domain_mrid",
        eic_name_name = "bid_ts_acquiring_domain_name"
      )
  }
  if ("bid_ts_connecting_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "bid_ts_connecting_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "bid_ts_connecting_domain_mrid",
        eic_name_name = "bid_ts_connecting_domain_name"
      )
  }
  if ("domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "domain_mrid",
        eic_name_name = "domain_name"
      )
  }
  if ("constraint_ts_monitored_ptdf_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(
      affected_cols, "constraint_ts_monitored_ptdf_domain_mrid"
    )
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "constraint_ts_monitored_ptdf_domain_mrid",
        eic_name_name = "constraint_ts_monitored_ptdf_domain_name"
      )
  }
  if (length(affected_cols) == 0L) {
    warning("No additional eic names added!")
  }

  tbl
}



#' @title
#' add definitions to codes
#'
#' @noRd
add_definitions <- function(tbl) {
  # convert tbl to data.table in order to join faster
  tbl <- data.table::data.table(tbl)

  # define an empty vector to collect those column names
  # which will get definitions by add_definitions() function
  affected_cols <- c()

  # add definitions to codes
  if ("doc_status_value" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "doc_status_value")
    tbl <- def_merge(
      x = tbl,
      y = message_types,
      code_name = "doc_status_value",
      definition_name = "doc_status"
    )
  }
  if ("ts_auction_category" %in% names(tbl)) {
    category_types <- entsoeapi::category_types
    affected_cols <- c(affected_cols, "ts_auction_category")
    tbl <- def_merge(
      x = tbl,
      y = category_types,
      code_name = "ts_auction_category",
      definition_name = "ts_auction_category_def"
    )
  }
  if ("ts_flow_direction" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_flow_direction")
    tbl <- def_merge(
      x = tbl,
      y = direction_types,
      code_name = "ts_flow_direction",
      definition_name = "ts_flow_direction_def"
    )
  }
  rc_cols <- stringr::str_subset(
    string = names(tbl),
    pattern = "^reason_code(|_[0-9])"
  )
  if (length(rc_cols) > 0) {
    reason_code_types <- entsoeapi::reason_code_types
    for (rc_col in rc_cols) {
      affected_cols <- c(affected_cols, rc_col)
      tbl <- def_merge(
        x = tbl,
        y = reason_code_types,
        code_name = rc_col,
        definition_name = stringr::str_replace(
          string = rc_col,
          pattern = "_code",
          replacement = "_text"
        )
      )
    }
    rt_cols <- stringr::str_subset(
      string = names(tbl),
      pattern = "^reason_text(|_[0-9|_x|_y])"
    )
    if (length(rt_cols) > 1) {
      tbl <- tbl |>
        tidyr::unite(
          col = "reason_text",
          dplyr::all_of(rt_cols),
          sep = " - ",
          remove = TRUE,
          na.rm = TRUE
        ) |>
        tidyr::unite(
          col = "reason_code",
          dplyr::all_of(rc_cols),
          sep = " - ",
          remove = TRUE,
          na.rm = TRUE
        ) |>
        data.table::data.table()
    }
  }
  trc_cols <- stringr::str_subset(
    string = names(tbl),
    pattern = "^ts_reason_code(|_[0-9])"
  )
  if (length(trc_cols) > 0) {
    reason_code_types <- entsoeapi::reason_code_types
    for (trc_col in trc_cols) {
      affected_cols <- c(affected_cols, trc_col)
      tbl <- def_merge(
        x = tbl,
        y = reason_code_types,
        code_name = trc_col,
        definition_name = stringr::str_replace(
          string = trc_col,
          pattern = "_code",
          replacement = "_text"
        )
      )
    }
    trt_cols <- stringr::str_subset(
      string = names(tbl),
      pattern = "^ts_reason_text(|_[0-9|_x|_y])"
    )
    if (length(trt_cols) > 1) {
      tbl <- tbl |>
        tidyr::unite(
          col = "ts_reason_text",
          dplyr::all_of(trt_cols),
          sep = " - ",
          remove = TRUE,
          na.rm = TRUE
        ) |>
        tidyr::unite(
          col = "ts_reason_code",
          dplyr::all_of(trc_cols),
          sep = " - ",
          remove = TRUE,
          na.rm = TRUE
        ) |>
        data.table::data.table()
    }
  }
  if ("ts_object_aggregation" %in% names(tbl)) {
    object_aggregation_types <- entsoeapi::object_aggregation_types
    affected_cols <- c(affected_cols, "ts_object_aggregation")
    tbl <- def_merge(
      x = tbl,
      y = object_aggregation_types,
      code_name = "ts_object_aggregation",
      definition_name = "ts_object_aggregation_def"
    )
  }
  if (length(affected_cols) == 0L) {
    warning("No additional definitions added!")
  }

  tbl
}



#' @title
#' convert xml content to table new version
#'
#' @noRd
xml_to_table <- function(xml_content, tidy_output = FALSE) {
  is_xml_document <- inherits(x = xml_content, what = "xml_document")
  if (isFALSE(is_xml_document)) {
    stop(
      "The 'xml_content' should be an xml document!",
      call. = FALSE
    )
  }

  # extract nodesets from the XML document and process
  result_tbl <- tryCatch(
    expr = xml2::xml_contents(xml_content) |> extract_leaf_twig_branch(),
    error = \(e) {
      stop(
        sprintf("The XML document has an unexpected tree structure!\n%s", e),
        call. = FALSE
      )
    }
  )

  # merge the related date and time columns into datetime column
  for (pref in c("start_", "end_")) {
    date_col <- stringr::str_subset(
      string = names(result_tbl),
      pattern = paste0(pref, "DateAndOrTime\\.date")
    )
    time_col <- stringr::str_subset(
      string = names(result_tbl),
      pattern = paste0(pref, "DateAndOrTime\\.time")
    )
    if (length(date_col) == 1L && length(time_col) == 1L) {
      datetime_col <- date_col |>
        stringr::str_remove_all(pattern = "AndOr|\\.date$")
      result_tbl[[datetime_col]] <- stringr::str_c(
        result_tbl[[date_col]],
        result_tbl[[time_col]],
        sep = "T"
      )
      result_tbl <- result_tbl |>
        dplyr::select(!tidyselect::all_of(c(date_col, time_col)))
    }
  }

  # convert datetime-like columns to POSIXct and numeric-like columns to numeric
  result_tbl <- result_tbl |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("[t|T]ime$|start$|end$"),
        ~as.POSIXct(
          x = .x,
          tryFormats = c(
            "%Y-%m-%dT%H:%MZ", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%OSZ"
          ),
          tz = "UTC"
        )
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches(
          match = "number$|position$|quantity$|nominalP$|amount$",
          ignore.case = TRUE
        ),
        ~as.numeric(x = .x)
      )
    )

  # if 'TimeSeries.mRID' can be converted to numeric then convert it
  ts_mrid_is_in <- "TimeSeries.mRID" %in% names(result_tbl)
  if (ts_mrid_is_in) {
    ts_mrid_is_num <- stringr::str_detect(
      string = result_tbl[["TimeSeries.mRID"]],
      pattern = "^[0-9]+$"
    ) |>
      all()
    if (ts_mrid_is_num) {
      result_tbl <- result_tbl |>
        dplyr::mutate(
          TimeSeries.mRID = as.numeric(x = TimeSeries.mRID)
        )
    }
  }

  # rename columns to snakecase
  names(result_tbl) <- my_snakecase(tbl = result_tbl)

  # adjust the table according to tidy_output value
  result_tbl <- tidy_or_not(
    tbl = result_tbl,
    tidy_output = tidy_output
  )

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
    "type", "type_def", "process_type",
    "process_type_def",
    "ts_contract_market_agreement_type",
    "ts_contract_market_agreement_type_def",
    "ts_auction_mrid", "ts_auction_type",
    "ts_auction_type_def",
    "ts_auction_category",
    "ts_auction_category_def",
    "ts_object_aggregation",
    "ts_object_aggregation_def",
    "ts_flow_direction", "ts_flow_direction_def",
    "bid_ts_flow_direction", "bid_ts_flow_direction_def",
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
    "ts_point_congestion_cost",
    "ts_currency_unit_name",
    "bid_ts_currency_unit_name",
    "ts_price_measure_unit_name",
    "bid_ts_price_measure_unit_name",
    "ts_quantity_measure_unit_name",
    "bid_ts_quantity_measure_unit_name",
    "high_voltage_limit",
    "ts_classification_sequence_position",
    "constraint_ts_monitored_ptdf_domain_mrid",
    "constraint_ts_monitored_ptdf_domain_name",
    "constraint_ts_monitored_ptdf_domain_quantity",
    "constraint_ts_monitored_flow_based_study_domain_margin_quantity"
  )
  needed_cols <- base::intersect(x = needed_cols,
                                 y = names(result_tbl))

  # check if any columns left to keep
  if (length(needed_cols)) {
    # filter on the needed columns
    result_tbl <- result_tbl |>
      dplyr::select(tidyselect::all_of(needed_cols))

    # reorder the rows
    sort_cols <- base::intersect(x = c("created_date_time", "ts_mrid",
                                       "ts_business_type", "ts_mkt_psr_type",
                                       "ts_time_interval_start",
                                       "ts_point_dt_start"),
                                 y = names(result_tbl))
    result_dtbl <- data.table::as.data.table(result_tbl)
    data.table::setkeyv(x = result_dtbl, cols = sort_cols)

    # convert the result to tibble
    result_tbl <- tibble::as_tibble(result_dtbl)

    # return
    result_tbl

  } else {
    stop(
      "There is no interesting column in the result table!",
      call. = FALSE
    )
  }

}



#' @title
#' extract the response from content list
#'
#' @noRd
extract_response <- function(content, tidy_output = TRUE) {

  # check if the content is in the required list format
  is_in_format <- is.list(content) &&
    length(content) == 2L &&
    all(names(content) == c("result", "error"))
  if (is_in_format) {

    # extract the possible failure reason
    reason <- content$error

    # if valid content got
    if (is.null(reason)) {

      # check if the response is list
      result_is_list <- inherits(x = content$result, what = "list")

      # if the response is list, then convert the XML elements
      # to table in a loop and append them to a single table
      if (result_is_list) {
        # convert XMLs to one table
        response_length <- length(content$result)
        result_tbl <- content$result |>
          purrr::imap(
                      \(x, idx) {
                        times <- response_length - idx + 1L
                        if (times > 1L) {
                          message(
                            idx, " ", rep(x = "<", times = times)
                          )
                        }
                        if (is.null(x)) {
                          NULL
                        } else {
                          if (inherits(x = x, what = "list")) {
                            all_doc <- purrr::map_lgl(
                              x, inherits, what = "xml_document"
                            ) |>
                              all()
                            if (all_doc) {
                              purrr::map(
                                x, xml_to_table, tidy_output = tidy_output
                              ) |>
                                purrr::compact() |>
                                data.table::rbindlist(
                                  use.names = TRUE,
                                  fill = TRUE
                                )
                            } else {
                              NULL
                            }
                          } else {
                            xml_to_table(
                              xml_content = x,
                              tidy_output = tidy_output
                            )
                          }
                        }
                      }) |>
          purrr::compact() |>
          data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
          tibble::as_tibble()
      } else {
        # convert XML to table
        result_tbl <- xml_to_table(
          xml_content = content$result,
          tidy_output = tidy_output
        )
      }

      # return
      result_tbl

    } else {

      stop(reason$message, call. = FALSE)

    }
  } else {

    stop(
      "The content is not in the required list format!",
      call. = FALSE
    )

  }
}
