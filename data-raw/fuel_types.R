# read raw tsv, trim the character columns and remove empty columns
fuel_types <- fs::path("data-raw",
                       "fuel_types",
                       ext = "tsv") |>
  data.table::fread(encoding = "UTF-8") |>
  purrr::modify_if(is.character, trimws, which = "both") |>
  purrr::discard(~is.na(.x) |> all())

# save the package data in the correct format
usethis::use_data(fuel_types, overwrite = TRUE)
