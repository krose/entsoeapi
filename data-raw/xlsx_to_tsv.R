df_list <- rio::import_list(file = "data-raw/entso-e-code-list-v92-tables.xlsx")

purrr::walk2(
  df_list,
  names(df_list),
  function(df, sheet_name) {
    if (endsWith(x = sheet_name, suffix = "Type")) {
      file_path <- paste0("data-raw/", snakecase::to_snake_case(sheet_name), "s.tsv")
    } else {
      file_path <- paste0("data-raw/", snakecase::to_snake_case(sheet_name), ".tsv")
    }
    dt <- purrr::map(df, gsub, pattern = "\\n", replacement = "") |>
      data.table::as.data.table()
    data.table::fwrite(
      x = dt,
      file = file_path,
      sep = "\t",
      na = "",
      quote = FALSE
    )
  }
)
