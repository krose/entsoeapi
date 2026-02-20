"
This scripts downloads the actual code lists XML from the preset link and
converts them to tables and places those in the data/ folder.
"


# set the latest code list URL (use AI for search)
url <- "https://www.entsoe.eu/Documents/EDI/Library/CodelistV93.7z"

# set the working files and folders
tmp <- tempfile(fileext = ".7z")
out_main_dir <- fs::path_wd("data-raw")

# ── 1. Download and extract the archive ──────────────────────────────────────
# Build and perform the request
httr2::request(base_url = url) |>
  httr2::req_headers(Authorization = Sys.getenv("ENTSOE_PAT")) |>
  httr2::req_perform(path = tmp)  # streams directly to disk

# Extract the archive
archive::archive_extract(archive = tmp, dir = out_main_dir)


out_dir <- fs::dir_ls(
  path = out_main_dir,
  type = "directory",
  glob = "*Codelist*"
)

# ── 2. Locate the XSD file ───────────────────────────────────────────────────
xsd_path <- fs::dir_ls(
  path = out_main_dir,
  recurse = TRUE,
  type = "file",
  regexp = "codelists\\.xsd$"
) |>
  purrr::pluck(1L)
message("Using: ", xsd_path)

# ── 3. Parse the XSD ─────────────────────────────────────────────────────────
doc <- xml2::read_xml(x = xsd_path)

# XSD uses the xs: namespace
ns <- c(xs = "http://www.w3.org/2001/XMLSchema")

# ── 4. Extract every simpleType that contains enumerations ───────────────────
simple_types <- xml2::xml_find_all(
  x = doc,
  xpath = ".//xs:simpleType[@name]",
  ns = ns
)

parse_simple_type <- function(node) {
  enum_nodes <- xml2::xml_find_all(
    x = node,
    xpath = ".//xs:enumeration",
    ns = ns
  )

  # skip non-enum types
  if (length(enum_nodes) == 0L) return(NULL)

  data.table::data.table(
    list_name = xml2::xml_attr(x = node, attr = "name") |>
      stringr::str_remove(
        pattern = "Standard"
      ) |>
      snakecase::to_snake_case() |>
      stringr::str_replace(
        pattern = "type_list",
        replacement = "types"
      ) |>
      stringr::str_replace(
        pattern = "unit_symbol",
        replacement = "unit_symbol_types"
      ) |>
      stringr::str_replace(
        pattern = "tarif_type_types",
        replacement = "tariff_types"
      ),
    code = xml2::xml_attr(x = enum_nodes, attr = "value"),
    title = enum_nodes |>
      purrr::map_chr(
        \(enum_node) {
          n <- xml2::xml_find_first(
            x = enum_node,
            xpath = ".//*[local-name()='Title']",
            ns = ns
          )
          if (inherits(x = n, what = "xml_missing")) {
            NA_character_
          } else {
            xml2::xml_text(x = n)
          }
        }
      ),
    description = purrr::map_chr(
      enum_nodes,
      \(enum_node) {
        n <- xml2::xml_find_first(
          x = enum_node,
          xpath = ".//*[local-name()='Definition']",
          ns = ns
        )
        if (inherits(x = n, what = "xml_missing")) {
          NA_character_
        } else {
          xml2::xml_text(x = n)
        }
      }
    ) |>
      purrr::modify(gsub, pattern = "\\n", replacement = " ") |>
      purrr::modify_if(is.character, trimws, which = "both") |>
      purrr::discard(~collapse::allNA(.x))
  )
}

# ── 5. Combine into one master table ─────────────────────────────────────────
code_lists_combined <- simple_types |>
  purrr::map(parse_simple_type) |>
  purrr::compact() |>
  data.table::rbindlist(use.names = TRUE, fill = TRUE)

# ── 6. Split into a named list of individual tables ──────────────────────────
code_lists <- code_lists_combined |>
  split(by = "list_name", keep.by = FALSE)

# ── 7. Inspect ───────────────────────────────────────────────────────────────
# number of code list tables
length(code_lists)

# the names of the code list tables
names(code_lists) |> sort()

# ── 8. Save onto disk ────────────────────────────────────────────────────────
list2env(code_lists, envir = globalenv())
syms <- rlang::syms(names(code_lists))
do.call(
  usethis::use_data,
  c(lapply(names(code_lists), as.name), list(overwrite = TRUE))
)
