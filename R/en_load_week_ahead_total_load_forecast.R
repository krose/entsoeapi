

#' Get week ahead total load forecast from Entsoe
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
#' en_load_week_ahead_total_load_forecast(eic = "10Y1001A1001A82H",
#'                                        period_start = lubridate::ymd("2019-11-01", tz = "CET"),
#'                                        period_end = lubridate::ymd_hms("2019-11-30 23:00:00", tz = "CET"))
#'
#'
en_load_week_ahead_total_load_forecast <- function(eic, period_start, period_end, security_token = NULL){

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
    "?documentType=A65",
    "&processType=A31",
    "&outBiddingZone_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req(url)

  en_cont <- xml2::as_list(en_cont)

  en_unit <- unlist(en_cont$GL_MarketDocument$TimeSeries$quantity_Measure_Unit.name)
  en_out_bidding_zone <- unlist(en_cont$GL_MarketDocument$TimeSeries$outBiddingZone_Domain.mRID)

  en_cont <- load_actual_total_load_helper(en_cont)
  en_cont$unit <- en_unit
  en_cont$out_bidding_zone <- en_out_bidding_zone

  en_cont
}
