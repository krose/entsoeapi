# read raw tsv, trim the character columns and remove empty columns
transmission_pair_eic_dict <- fs::path("data-raw",
                                       "transmission_pair_eic_dict",
                                       ext = "tsv") |>
  data.table::fread(encoding = "UTF-8", fill = TRUE) |>
  purrr::modify_if(is.character, trimws, which = "both") |>
  purrr::discard(~is.na(.x) |> all())

# save the package data in the correct format
usethis::use_data(transmission_pair_eic_dict, overwrite = TRUE)
