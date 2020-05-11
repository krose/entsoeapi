

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
#'
#'
#'
en_generation_inst_gen_cap_agg <- function(eic, year, security_token = NULL){

  period_start <- lubridate::ymd(paste0(year, "-01-01"), tz = "CET")
  period_end <- lubridate::ymd_hm(paste0(year, "-01-01 23:00"), tz = "CET")
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }
  gen_type <- c("B01", "B02", "B03", "B04", "B05", "B06", "B08", "B09", "B10", "B11", "B12", "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20")

  url_list <- lapply(gen_type, en_gen_inst_agg_gen_cap_api_req_helper, eic = eic, period_start = period_start, period_end = period_end, security_token = security_token)

  api_req_safe <- purrr::safely(api_req)

  en_cont <- purrr::map(url_list, api_req_safe)
  en_cont <- purrr::map(en_cont, "result")
  en_cont <- purrr::map(en_cont, "GL_MarketDocument")
  en_cont[sapply(en_cont, is.null)] <- NULL

  en_cont <- purrr::map(en_cont, xml2::as_list)

  # en_unit <- unlist(en_cont$GL_MarketDocument$TimeSeries$quantity_Measure_Unit.name)
  # en_out_bidding_zone <- unlist(en_cont$GL_MarketDocument$TimeSeries$outBiddingZone_Domain.mRID)
  #
  # en_cont <- load_actual_total_load_helper(en_cont)
  #
  # attr(en_cont, "unit") <- en_unit
  # attr(en_cont, "zone") <- en_out_bidding_zone

  en_cont
}


en_gen_inst_agg_gen_cap_api_req_helper <- function(psr_type, eic, period_start, period_end, security_token){

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A68",
    "&processType=A33",
    "&psrType=", psr_type,
    "&in_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  url
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
#'
#' fr_2020 <- en_generation_agg_gen_per_type(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2020-02-01", tz = "CET"), period_end = lubridate::ymd("2020-03-01", tz = "CET"))
#'
en_generation_agg_gen_per_type <- function(eic, period_start, period_end, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  gen_type <- as.character(c(NA))

  url_list <- lapply(gen_type, en_gen_agg_gen_pertype_api_req_helper, eic = eic, period_start = period_start, period_end = period_end, security_token = security_token)

  api_req_safe <- purrr::safely(api_req)

  en_cont <- purrr::map(url_list, api_req_safe)
  en_cont <- purrr::map(en_cont, "result")
  #en_cont <- purrr::map(en_cont, "GL_MarketDocument")
  en_cont[sapply(en_cont, is.null)] <- NULL

  en_cont <- purrr::map(en_cont, xml2::as_list)
  en_cont <- en_cont[[1]]$GL_MarketDocument
  en_cont <- en_cont[names(en_cont) == "TimeSeries"]
  en_cont <- lapply(en_cont, ts_agg_gen_helper)

  en_cont <- dplyr::bind_rows(en_cont)
  en_cont <- dplyr::select(en_cont, -mRID, -businessType, -objectAggregation, -curveType, -position)

  en_cont
}

en_gen_agg_gen_pertype_api_req_helper <- function(psr_type, eic, period_start, period_end, security_token){

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A75",
    "&processType=A16",
    #"&psrType=", psr_type,
    "&in_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  url
}

ts_agg_gen_helper <- function(ts){

  period <- ts$Period
  ts$Period <- NULL

  points <- period[names(period) == "Point"]
  points <- dplyr::bind_rows(lapply(points, function(x){data.frame(position = x$position[[1]], quantity = x$quantity[[1]], stringsAsFactors = FALSE)}))
  points$position <- as.integer(points$position)
  points$quantity <- as.integer(points$quantity)

  points$dt <- dt_helper(tz_start = lubridate::ymd_hm(period$timeInterval$start[[1]]), tz_resolution = period$resolution[[1]], tz_position = points$position)

  ts <- lapply(ts, unlist)
  ts <- dplyr::bind_cols(ts)
  ts$points <- list(points)
  ts <- tidyr::unnest(ts, "points")

  ts
}

