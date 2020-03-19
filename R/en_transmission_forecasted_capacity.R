#' Get Day ahead prices from Entsoe
#'
#' @param eic_in Energy Identification Code
#' @param eic_out Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  en_transmission_forecasted_capacity(eic_in = "10YCZ-CEPS-----N", eic_out = "10YSK-SEPS-----K", period_start = lubridate::ymd("2019-11-01", tz = "CET"), period_end = lubridate::ymd("2019-12-01", tz = "CET"))
#'  en_transmission_forecasted_capacity(eic_in = "10YDK-1--------W", eic_out = "10Y1001A1001A82H", period_start = lubridate::ymd("2019-11-01", tz = "CET"), period_end = lubridate::ymd("2019-12-01", tz = "CET"))
#'
en_transmission_forecasted_capacity <- function(eic_in, eic_out, period_start, period_end, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  if(length(eic_in) > 1 | length(eic_out) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A61",
    "&contract_MarketAgreement.Type=A01",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req(url)

  en_cont <- xml2::as_list(en_cont)

  en_cont <- tm_quantity_helper(en_cont)

  en_cont
}


tm_quantity_helper <- function(x){

  x <- x$Publication_MarketDocument[names(x$Publication_MarketDocument) == "TimeSeries"]

  x <- purrr::map(x, ~timeseries_extract_forecast_amount(.x))
  x <- dplyr::bind_rows(x)

  x
}


timeseries_extract_forecast_amount <- function(x){
  dt <- lubridate::ymd_hm(x$Period$timeInterval$start[[1]], tz = "UTC")
  position <- as.integer(unlist(purrr::map(x$Period, "position")))
  quantity <- unlist(purrr::map(x$Period, "quantity"))

  dt <- dt_helper(tz_start = dt, tz_resolution = x$Period$resolution[[1]], tz_position = position)

  tibble::tibble(dt, quantity)
}
