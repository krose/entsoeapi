

#' Get Installed generation capacity aggregated per type from Entsoe
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
#' fr_2020 <- en_generation_inst_gen_cap_agg(eic = "10YFR-RTE------C", year=2020)
#'
en_generation_inst_gen_cap_agg <- function(eic, year, security_token = NULL){

  period_start <- lubridate::ymd(paste0(year, "-01-01"), tz = "CET")
  period_end <- lubridate::ymd(paste0(year, "-01-02"), tz = "CET")
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
    "?documentType=A68",
    "&processType=A33",
    "&in_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req(url)
  en_cont <- xml2::as_list(en_cont)
  en_cont <- generation_inst_gen_cap_agg_helper(en_cont)

}


generation_inst_gen_cap_agg_helper <- function(x){

  dt_created <- x$GL_MarketDocument$createdDateTime[[1]]

  x <- x$GL_MarketDocument[names(x$GL_MarketDocument) == "TimeSeries"]
  x <- unname(x)

  unit <- unlist(purrr::map(x, ~.x$quantity_Measure_Unit.name[[1]]))
  start <- unlist(purrr::map(x, ~.x$Period$timeInterval$start[[1]]))
  end <- unlist(purrr::map(x, ~.x$Period$timeInterval$end[[1]]))
  resolution <- unlist(purrr::map(x, ~.x$Period$resolution))
  psr_type <- unlist(purrr::map(x, ~.x$MktPSRType))
  quantity <- unlist(purrr::map(x, ~.x$Period$Point$quantity[[1]]))

  tibble::tibble(dt_created, start, end, resolution, psr_type, quantity, unit)
}



#' Get aggregated generation per type from Entsoe
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
#' fr_2020 <- en_generation_day_ahead_agg_gen(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2020-02-01", tz = "CET"), period_end = lubridate::ymd("2020-03-01", tz = "CET"))
#'
en_generation_day_ahead_agg_gen <- function(eic, period_start, period_end, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  gen_type <- as.character(c(NA))

  url_list <- lapply(gen_type, en_gen_day_ahead_agg_gen_api_req_helper, eic = eic, period_start = period_start, period_end = period_end, security_token = security_token)

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

en_gen_day_ahead_agg_gen_api_req_helper <- function(psr_type, eic, period_start, period_end, security_token){

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A71",
    "&processType=A01",
    #"&psrType=", psr_type,
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
#' fr_2020 <- en_generation_day_ahead_gen_forcast_ws(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2020-02-01", tz = "CET"), period_end = lubridate::ymd("2020-03-01", tz = "CET"))
#'
en_generation_day_ahead_gen_forecast_ws <- function(eic, period_start, period_end, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  gen_type <- as.character(c("B16", "B18", "B19"))

  url_list <- lapply(gen_type, en_gen_day_ahead_gen_forecast_ws_api_req_helper, eic = eic, period_start = period_start, period_end = period_end, security_token = security_token)

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

en_gen_day_ahead_gen_forecast_ws_api_req_helper <- function(psr_type, eic, period_start, period_end, security_token){

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A69",
    "&processType=A01",
    #"&psrType=", psr_type,
    "&in_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  url
}

