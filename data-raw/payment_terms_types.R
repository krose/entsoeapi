# read raw tsv, trim the character columns and remove empty columns
payment_terms_types <- fs::path("data-raw",
                                "payment_terms_types",
                                ext = "tsv") |>
  data.table::fread(encoding = "UTF-8") |>
  purrr::modify_if(is.character, trimws, which = "both") |>
  purrr::discard(~is.na(.x) |> all())

# save the package data in the correct format
usethis::use_data(payment_terms_types, overwrite = TRUE)
