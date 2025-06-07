# read raw tsv, trim the character columns and remove empty columns
market_product_types <- fs::path("data-raw",
                                 "market_product_types",
                                 ext = "tsv") |>
  data.table::fread(encoding = "UTF-8") |>
  purrr::modify_if(is.character, trimws, which = "both") |>
  purrr::discard(~is.na(.x) |> all())

# save the package data in the correct format
usethis::use_data(market_product_types, overwrite = TRUE)
