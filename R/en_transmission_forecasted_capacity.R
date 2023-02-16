#' Get day-ahead forecasted transfer capacities from Entsoe
#'
#' @param eic_in Energy Identification Code of in domain
#' @param eic_out Energy Identification Code of out domain
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
en_transmission_forecasted_capacity <- function(eic_in, eic_out, period_start, period_end, security_token = Sys.getenv("ENTSOE_PAT")){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(eic_in) > 1 | length(eic_out) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
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
  en_measure_unit <- en_cont$Publication_MarketDocument$TimeSeries$quantity_Measure_Unit.name[[1]]

  en_cont <- tm_quantity_helper(en_cont, "quantity")
  colnames(en_cont)[names(en_cont) == "value"] <- "quantity"

  en_cont$measure_unit <- en_measure_unit
  en_cont$in_domain_mrid <- eic_in
  en_cont$out_domain_mrid <- eic_out

  en_cont <- en_cont %>%
    dplyr::select(tidyselect::any_of(c("in_domain_mrid", "out_domain_mrid", "measure_unit", "dt", "quantity", "resolution")))

  en_cont
}


#' Get scheduled day-ahead commercial exchanges in aggregated form between bidding zones per direction and market time unit from Entsoe.
#'
#' @param eic_in Energy Identification Code of in domain
#' @param eic_out Energy Identification Code of out domain
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
#'  en_transmission_day_ahead_commercial_schedules(eic_in = "10YCZ-CEPS-----N", eic_out = "10YSK-SEPS-----K", period_start = lubridate::ymd("2019-11-01", tz = "CET"), period_end = lubridate::ymd("2019-12-01", tz = "CET"))
#'  en_transmission_day_ahead_commercial_schedules(eic_in = "10YDK-1--------W", eic_out = "10Y1001A1001A82H", period_start = lubridate::ymd("2019-11-01", tz = "CET"), period_end = lubridate::ymd("2019-12-01", tz = "CET"))
#'
en_transmission_day_ahead_commercial_schedules <- function(eic_in, eic_out, period_start, period_end, security_token = Sys.getenv("ENTSOE_PAT")){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(eic_in) > 1 | length(eic_out) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A09",
    "&contract_MarketAgreement.Type=A01",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req(url)
  en_cont <- xml2::as_list(en_cont)

  en_measure_unit <- en_cont$Publication_MarketDocument$TimeSeries$quantity_Measure_Unit.name[[1]]

  en_cont <- tm_quantity_helper(en_cont, "quantity")
  colnames(en_cont)[names(en_cont) == "value"] <- "quantity"

  en_cont$measure_unit <- en_measure_unit
  en_cont$in_domain_mrid <- eic_in
  en_cont$out_domain_mrid <- eic_out

  en_cont <- en_cont %>%
    dplyr::select(tidyselect::any_of(c("in_domain_mrid", "out_domain_mrid", "measure_unit", "dt", "quantity", "resolution")))

  en_cont
}


