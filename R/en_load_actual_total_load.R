
#' Get total load from Entsoe
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
#' en_load_actual_total_load(eic = "10Y1001A1001A82H", period_start = lubridate::ymd("2019-03-31", tz = "CET"),
#'                           period_end = lubridate::ymd_hm("2019-03-31 23:00", tz = "CET"))
#'
#' # German average daily load.
#' en_load_actual_total_load(eic = "10Y1001A1001A83F", period_start = lubridate::ymd(as.character(Sys.Date() - 30), tz = "UTC"),
#'                           period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC")) %>%
#'   mutate(dt = lubridate::floor_date(dt, "hours")) %>%
#'   group_by(dt) %>%
#'   summarise(quantity = mean(quantity)) %>%
#'   ungroup() %>%
#'   mutate(dt = lubridate::floor_date(dt, "days")) %>%
#'   group_by(dt) %>%
#'   summarise(quantity = mean(quantity)) %>%
#'   ungroup() %>%
#'   mutate(wday = lubridate::wday(dt, week_start = 1), isoweek = as.factor(lubridate::isoweek(dt))) %>%
#'   ggplot(., aes(wday, quantity, col = isoweek)) + geom_line()
#'
en_load_actual_total_load <- function(eic, period_start, period_end, security_token = NULL){

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
    "&processType=A16",
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

  attr(en_cont, "unit") <- en_unit
  attr(en_cont, "zone") <- en_out_bidding_zone

  en_cont
}


#' Get total load from Entsoe
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
#' en_load_day_ahead_total_load_forecast(eic = "10Y1001A1001A82H", period_start = lubridate::ymd("2019-03-31", tz = "CET"),
#'                           period_end = lubridate::ymd_hm("2019-03-31 23:00", tz = "CET"))
#'
#' # German average daily load.
#' en_load_day_ahead_total_load_forecast(eic = "10Y1001A1001A83F", period_start = lubridate::ymd(as.character(Sys.Date() - 30), tz = "UTC"),
#'                           period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC")) %>%
#'   mutate(dt = lubridate::floor_date(dt, "hours")) %>%
#'   group_by(dt) %>%
#'   summarise(quantity = mean(quantity)) %>%
#'   ungroup() %>%
#'   mutate(dt = lubridate::floor_date(dt, "days")) %>%
#'   group_by(dt) %>%
#'   summarise(quantity = mean(quantity)) %>%
#'   ungroup() %>%
#'   mutate(wday = lubridate::wday(dt, week_start = 1), isoweek = as.factor(lubridate::isoweek(dt))) %>%
#'   ggplot(., aes(wday, quantity, col = isoweek)) + geom_line()
#'
en_load_day_ahead_total_load_forecast <- function(eic, period_start, period_end, security_token = NULL){

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
    "&processType=A01",
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

  attr(en_cont, "unit") <- en_unit
  attr(en_cont, "zone") <- en_out_bidding_zone

  en_cont
}



load_actual_total_load_helper <- function(x){
  x <- x$GL_MarketDocument[names(x$GL_MarketDocument) == "TimeSeries"]

  x <- purrr::map(x, ~timeseries_extract(.x))
  x <- dplyr::bind_rows(x)

  x
}

timeseries_extract <- function(x){

  dt <- lubridate::ymd_hm(x$Period$timeInterval$start[[1]], tz = "UTC")
  position <- as.integer(unlist(purrr::map(x$Period, "position")))
  quantity <- as.numeric(unlist(purrr::map(x$Period, "quantity")))

  dt <- dt_helper(tz_start = dt, tz_resolution = x$Period$resolution[[1]], tz_position = position)

  tibble::tibble(dt, quantity)
}


