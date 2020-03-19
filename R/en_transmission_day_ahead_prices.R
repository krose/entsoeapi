
#' Get Day ahead prices from Entsoe
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' dk1 <- en_transmission_day_ahead_prices(eic = "10YDK-1--------W",
#'                                         period_start = lubridate::ymd("2019-10-27", tz = "CET"),
#'                                         period_end = lubridate::ymd_hm("2019-10-27 23:00", tz = "CET"))
#' dk1 <- en_transmission_day_ahead_prices(eic = "10YDK-1--------W",
#'                                         period_start = lubridate::ymd("2019-03-31", tz = "CET"),
#'                                         period_end = lubridate::ymd_hm("2019-03-31 23:00", tz = "CET"))
#'
en_transmission_day_ahead_prices <- function(eic, period_start, period_end, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A44",
    "&in_Domain=", eic,
    "&out_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req(url)

  en_cont <- xml2::as_list(en_cont)

  en_cont <- tm_day_ahead_helper(en_cont)

  en_cont
}


tm_day_ahead_helper <- function(x){

  x <- x$Publication_MarketDocument[names(x$Publication_MarketDocument) == "TimeSeries"]

  x <- purrr::map(x, ~timeseries_extract_day_ahead(.x))
  x <- dplyr::bind_rows(x)

  x
}

timeseries_extract_day_ahead <- function(x){
  dt <- lubridate::ymd_hm(x$Period$timeInterval$start[[1]], tz = "UTC")
  position <- as.integer(unlist(purrr::map(x$Period, "position")))
  price <- unlist(purrr::map(x$Period, "price.amount"))

  dt <- dt_helper(tz_start = dt, tz_resolution = x$Period$resolution[[1]], tz_position = position)

  tibble::tibble(dt, price)

}

