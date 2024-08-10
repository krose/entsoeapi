fs::dir_ls(
  path = "data-raw",
  regexp = "\\/.+_types\\.R|transmission_pair_eic_dict\\.R"
) |>
  purrr::walk(source)
