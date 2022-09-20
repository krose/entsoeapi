
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
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  en_transmission_day_ahead_prices(eic = "10YCZ-CEPS-----N", period_start = lubridate::ymd("2019-11-01", tz = "CET"), period_end = lubridate::ymd("2019-12-01", tz = "CET"))
#'  en_transmission_day_ahead_prices(eic = "10YDK-1--------W", period_start = lubridate::ymd("2019-11-01", tz = "CET"), period_end = lubridate::ymd("2019-12-01", tz = "CET"))
#'
en_transmission_day_ahead_prices <- function(eic, period_start, period_end, in_domain = NULL, out_domain = NULL, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(in_domain)){
    in_domain <- eic
  }
  if(is.null(out_domain)){
    out_domain <- eic
  }

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A44",
    "&in_Domain=", in_domain,
    "&out_Domain=", out_domain,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req(url)

  en_cont <- xml2::as_list(en_cont)

  en_currency <- en_cont$Publication_MarketDocument$TimeSeries$currency_Unit.name[[1]]
  en_price_unit <- en_cont$Publication_MarketDocument$TimeSeries$price_Measure_Unit.name[[1]]

  en_cont <- tm_price_helper(en_cont)

  en_cont$price <- as.numeric(en_cont$price)
  en_cont$currency_unit <- en_currency
  en_cont$price_unit <- en_price_unit
  en_cont$in_domain_mrid <- in_domain
  en_cont$out_domain_mrid <- out_domain

  en_cont <- en_cont[, c("in_domain_mrid", "out_domain_mrid", "currency_unit", "price_unit", "dt", "price", "mrid")]

  en_cont
}


