# read raw tsv, trim the character columns and remove empty columns
energy_product_types <- fs::path("data-raw",
                                 "energy_product_types",
                                 ext = "tsv") |>
  data.table::fread(encoding = "UTF-8") |>
  dplyr::mutate(Code = as.character(Code)) |>
  purrr::modify_if(is.character, trimws, which = "both") |>
  purrr::discard(~is.na(.x) |> all())

# save the package data in the correct format
usethis::use_data(energy_product_types, overwrite = TRUE)
