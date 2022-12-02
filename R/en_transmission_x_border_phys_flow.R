#' Get cross-border physical flow values between control areas / bidding zones /countries from Entsoe.
#' Unlike Web GUI, API responds not netted values since data is requested per direction.
#'
#' @param eic_in Energy Identification Code of in domain
#' @param eic_out Energy Identification Code of out domain
#' @param period_start POSIXct
#' @param period_end POSIXct Minimum time interval in query response is an MTU period, but 1 year range limit applies.
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  en_transmission_x_border_phys_flow(eic_in = "10Y1001A1001A83F", eic_out = "10YCZ-CEPS-----N", period_start = lubridate::ymd("2020-01-01", tz = "CET"), period_end = lubridate::ymd("2020-01-02", tz = "CET"))
#'  en_transmission_x_border_phys_flow(eic_in = "10YCZ-CEPS-----N", eic_out = "10Y1001A1001A83F", period_start = lubridate::ymd("2020-01-01", tz = "CET"), period_end = lubridate::ymd("2020-01-02", tz = "CET"))
#'
en_transmission_x_border_phys_flow <- function(eic_in, eic_out, period_start, period_end, security_token = NULL){

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
    "?documentType=A11",
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

  en_cont <- en_cont[, c("in_domain_mrid", "out_domain_mrid", "measure_unit", "dt", "quantity")]

  en_cont
}
