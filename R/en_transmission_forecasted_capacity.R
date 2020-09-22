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

  en_cont <- en_cont[, c("in_domain_mrid", "out_domain_mrid", "currency_unit", "price_unit", "dt", "price")]

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

tm_price_helper <- function(x){

  x <- x$Publication_MarketDocument[names(x$Publication_MarketDocument) == "TimeSeries"]

  x <- purrr::map(x, ~timeseries_extract_price_amount(.x))
  x <- dplyr::bind_rows(x)

  x
}

timeseries_extract_price_amount <- function(x){
  dt <- lubridate::ymd_hm(x$Period$timeInterval$start[[1]], tz = "UTC")
  position <- as.integer(unlist(purrr::map(x$Period, "position")))
  price <- unlist(purrr::map(x$Period, "price.amount"))

  dt <- dt_helper(tz_start = dt, tz_resolution = x$Period$resolution[[1]], tz_position = position)

  tibble::tibble(dt, price)
}


#' Get the available areas.
#'
#' @export
#'
#'
en_transmission_day_ahead_prices_eic <- function(){
  trans_df <- structure(list(AreaCode = c("10YCS-SERBIATSOV", "10YHU-MAVIR----U",
                              "10YSK-SEPS-----K", "10YRO-TEL------P", "10YES-REE------0", "10Y1001A1001A46L",
                              "10YDK-1--------W", "10YNO-1--------2", "10YFI-1--------U", "10Y1001A1001A45N",
                              "10Y1001A1001A44P", "10YNO-3--------J", "10YLV-1001A00074", "10YDK-2--------M",
                              "10YNO-2--------T", "10Y1001A1001A47J", "10Y1001A1001A48H", "10YNO-4--------9",
                              "10Y1001A1001A39I", "10YLT-1001A0008Q", "10YPT-REN------W", "10YSI-ELES-----O",
                              "10Y1001A1001A82H", "10YFR-RTE------C", "10YAT-APG------L", "10YBE----------2",
                              "10YNL----------L", "10YGB----------A", "10YCA-BULGARIA-R", "10YHR-HEP------M",
                              "10YPL-AREA-----S", "10YGR-HTSO-----Y", "10Y1001A1001A70O", "10Y1001A1001A73I",
                              "10Y1001A1001A71M", "10Y1001A1001A788", "10Y1001A1001A77A", "10Y1001A1001A75E",
                              "10Y1001A1001A893", "10Y1001A1001A74G", "10Y1001A1001A885", "10YCH-SWISSGRIDZ",
                              "10YCZ-CEPS-----N", "10Y1001A1001A59C"), AreaTypeCode = c("BZN",
                                                                                        "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                                                                                        "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                                                                                        "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                                                                                        "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                                                                                        "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN"), AreaName = c("EMS BZ",
                                                                                                                                                       "MAVIR BZ", "SEPS BZ", "Transelectrica BZ", "REE BZ", "SE3 BZ",
                                                                                                                                                       "DK1  BZ", "NO1 BZ", "Fingrid BZ", "SE2 BZ", "SE1 BZ", "NO3 BZ",
                                                                                                                                                       "AST BZ", "DK2 BZ", "NO2 BZ", "SE4 BZ", "NO5 BZ", "NO4 BZ", "Elering BZ",
                                                                                                                                                       "Litgrid BZ", "REN BZ", "ELES BZ", "DE-LU BZ", "RTE BZ", "APG BZ",
                                                                                                                                                       "Elia BZ", "TenneT NL BZ", "National Grid BZ", "ESO BZ", "HOPS BZ",
                                                                                                                                                       "PSE SA BZ", "IPTO BZ", "IT-Centre-North BZ", "IT-North BZ",
                                                                                                                                                       "IT-Centre-South BZ", "IT-South BZ", "IT-Rossano BZ", "IT-Sicily BZ",
                                                                                                                                                       "Italy_Sacodc", "IT-Sardinia BZ", "Italy_Saco_AC", "swissgrid BZ",
                                                                                                                                                       "CEPS BZ", "Ireland - (SEM) BZ")), class = c("tbl_df", "tbl",
                                                                                                                                                                                                    "data.frame"), row.names = c(NA, -44L))

  trans_df
}
