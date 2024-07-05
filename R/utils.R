

api_req <- function(url) {

  message(url, " ...")
  req <- httr::GET(url)

  if (httr::status_code(req) != "200") {
    stop(httr::content(req, encoding = "UTF-8"))
  }
  en_cont <- httr::content(req, encoding = "UTF-8")
  message("downloaded")

  return(en_cont)
}


api_req_safe <- function(..., otherwise = NULL, quiet = TRUE) {

  return(purrr:::capture_error(api_req(...), otherwise = otherwise, quiet = quiet))

}


url_posixct_format <- function(x) {

  if (any(class(x) == "POSIXct")) {
    y <- strftime(x = x, format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
  } else {
    y <- lubridate::parse_date_time(x      = x,
                                    orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
                                               "%Y.%m.%d %H:%M:%S", "%Y.%m.%d %H:%M", "%Y.%m.%d",
                                               "%Y%m%d%H%M%S",      "%Y%m%d%H%M",     "%Y%m%d"),
                                    tz     = "UTC",
                                    quiet  = TRUE) |>
      strftime(format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
    if (is.na(y)) {
      stop("Only the class POSIXct or '%Y-%m-%d %H:%M:%S' formatted text are supported by the converter.")
    } else {
      warning("The ", x, " value interpreted as UTC.", call. = FALSE)
    }
  }

  return(y)

}


dt_helper <- function(tz_start, tz_resolution, tz_position) {

  # turn 'tz_start' to POSIXct if it is not such yet
  if (!lubridate::is.POSIXct(tz_start)) {
    tz_start <- as.POSIXct(x = tz_start,
                           tryFormats = c("%Y-%m-%dT%H:%MZ",
                                          "%Y-%m-%dT%H:%M:%SZ",
                                          "%Y-%m-%d %H:%M",
                                          "%Y-%m-%d %H:%M:%S"),
                           tz = "UTC")
  }

  # calculate 'add_time' value
  tz_pos_prev <- as.numeric(tz_position) - 1
  add_time <- dplyr::case_when(
    tz_resolution == "PT1M" ~ lubridate::minutes(x = tz_pos_prev),
    tz_resolution == "PT15M" ~ lubridate::minutes(x = tz_pos_prev * 15),
    tz_resolution == "PT30M" ~ lubridate::minutes(x = tz_pos_prev * 30),
    tz_resolution == "PT60M" ~ lubridate::hours(x = tz_pos_prev),
    tz_resolution == "P1D" ~ lubridate::days(x = tz_pos_prev),
    tz_resolution == "P1Y" ~ lubridate::years(x = tz_pos_prev)
  )

  if (is.null(add_time)) {
    stop("The provided 'tz_resolution' value is not supported.",
         "\nPlease use 'PT1M', 'PT15M', 'PT30M', 'PT60M' or 'P1D'.")
  } else {
    dt <- tz_start + add_time
    return(dt)
  }

}

dt_seq_helper <- function(from, to, seq_resolution = "PT60M", qty) {

  ## calculate "by" value from "seq_resolution" value
  by <- dplyr::case_when(seq_resolution == "PT1M" ~ "1 min",
                         seq_resolution == "PT15M" ~ "15 mins",
                         seq_resolution == "PT30M" ~ "30 mins",
                         seq_resolution == "PT60M" ~ "1 hour",
                         seq_resolution == "P1D" ~ "1 day",
                         seq_resolution == "P1Y" ~ "1 year",
                         .default = "n/a")

  ## compose a tibble from the expanded periods and the provided quantity
  if (by == "n/a") {
    stop("The provided 'seq_resolution' value is not supported.",
         "\nPlease use 'PT1M', 'PT15M', 'PT30M', 'PT60M' or 'P1D'.")
  } else {
    dts <- seq(from = from, to = to, by = by) |>
      lubridate::floor_date(unit = by)
    if (length(dts) == length(qty) + 1) {
      dts <- head(dts, -1)
    }
    dt_tbl <- tibble::tibble(
      dt = dts,
      qty = qty
    )
  }

  return(dt_tbl)

}


get_eiccodes <- function(f) {

  message("\ndownloading ", f, " file ...")

  ## reading input file into a character vector
  ## and replacing erroneous semicolons to commas
  ## unfortunately there is no general rule for that hence it must be set manually!!
  lns <- readLines(con = f, encoding = "UTF-8") |>
    stringr::str_replace_all(pattern     = "tutkimustehdas;\\sImatra",
                             replacement = "tutkimustehdas, Imatra") |>
    stringr::str_replace_all(pattern     = "; S\\.L\\.;",
                             replacement = ", S.L.;") |>
    stringr::str_replace_all(pattern     = "\\$amp;",
                             replacement = "&")

  ## looking for those lines (elements) which end are not
  ## according to the general rules
  clps_ind <- grep(x       = lns,
                   pattern = ";type$|;X$|;Y$|;Z$|;T$|;V$|;W$|;A$",
                   perl    = TRUE,
                   invert  = TRUE)

  ## if there are being collapsible elements
  if (length(x = clps_ind) > 0L) {

    ## iterating related elements thru from the last till the first element
    for (i in rev(clps_ind)) {
      ## collapsing related line (element) with its subsequent neighbor
      lns[i] <- paste0(lns[i], lns[i + 1L], collapse = "")
    }

    ## removing subsequent neighbors (after collapse)
    lns <- lns[-(clps_ind + 1L)]

  }

  ## reading lines as they would be a csv
  eiccodes <- data.table::fread(text       = lns,
                                sep        = ";",
                                na.strings = c("", "n / a", "n/a", "N/A", "-", "-------", "."),
                                encoding   = "UTF-8")

  ## trimming character columns
  eiccodes <- lapply(X   = names(eiccodes),
                     FUN = function(col) {
                       if (is.character(eiccodes[[col]])) {
                         data.table::set(x     = eiccodes,
                                         j     = col,
                                         value = trimws(x     = eiccodes[[col]],
                                                        which = "both"))
                         }
                       })

  return(eiccodes)

}


tm_quantity_helper <- function(x, patt) {

  sections <- x$Publication_MarketDocument[names(x$Publication_MarketDocument) == "TimeSeries"]
  tbl <- sections |>
    purrr::map(~timeseries_extract_quantity(section = .x, patt = patt)) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  return(tbl)

}


timeseries_extract_quantity <- function(section, patt) {

  value <- purrr::map(section$Period, patt) |>
    unlist() |>
    as.numeric()
  resolution <- section$Period$resolution[[1]]
  position <- purrr::map(x$Period, "position") |>
    unlist() |>
    as.integer()
  dt <- dt_helper(tz_start = strptime(x = x$Period$timeInterval$start[[ 1L ]],
                                      format = "%Y-%m-%dT%H:%MZ", tz = "UTC") |>
                    as.POSIXct(tz = "UTC"),
                  tz_resolution = resolution,
                  tz_position = position)
  tbl <- tibble::tibble(dt, value, resolution)

  return(tbl)

}


unpack_xml <- function(section, parent_name = NULL) {
    result_vector <- xml2::as_list(section) |> unlist()
    names(result_vector) <- stringr::str_c(parent_name,
                                           xml2::xml_name(section),
                                           names(result_vector),
                                           sep = ".")
    tbl <- tibble::as_tibble_row(result_vector)
    return(tbl)
}
