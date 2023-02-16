
#' Get day-ahead prices from Entsoe
#'
#' @param eic Energy Identification Code of the related domain
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
en_transmission_day_ahead_prices <- function(eic, period_start, period_end, security_token = Sys.getenv("ENTSOE_PAT")){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A44",
    "&in_Domain=", eic,
    "&out_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req(url)
  en_cont <- xml2::as_list(en_cont)

  en_currency <- en_cont$Publication_MarketDocument$TimeSeries$currency_Unit.name[[1]]
  en_price_unit <- en_cont$Publication_MarketDocument$TimeSeries$price_Measure_Unit.name[[1]]

  en_cont <- tm_quantity_helper(en_cont, "price.amount")

  colnames(en_cont)[names(en_cont) == "value"] <- "price"
  en_cont$currency_unit <- en_currency
  en_cont$price_unit <- en_price_unit
  en_cont$in_domain_mrid <- eic
  en_cont$out_domain_mrid <- eic

  en_cont <- en_cont %>%
    dplyr::select(tidyselect::any_of(c("in_domain_mrid", "out_domain_mrid", "currency_unit", "price_unit", "dt", "price", "resolution")))

  en_cont
}


