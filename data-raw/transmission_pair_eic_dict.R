# read raw tsv, trim the character columns and remove empty columns
transmission_pair_eic_dict <- fs::path("data-raw",
  "transmission_pair_eic_dict",
  ext = "tsv"
) |>
  utils::read.delim(encoding = "UTF-8", fill = TRUE) |>
  purrr::modify_if(is.character, trimws, which = "both") |>
  purrr::discard(~ all(is.na(.x))) |>
  tibble::as_tibble()

# save the package data in the correct format
usethis::use_data(transmission_pair_eic_dict, overwrite = TRUE)
