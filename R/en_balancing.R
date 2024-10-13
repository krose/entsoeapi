utils::globalVariables(
  c(
    "url_posixct_format",
    "api_req_safe",
    "extract_response"
  )
)



#' @title
#' Get Accepted Aggregated Offers (17.1.D)
#'
#' @description
#' Energy volumes available for activation.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param reserve_type Defaults to NULL, otherwise choose among the
#'                     list of reserve type codes (A95, A96, A97, A98)
#'                     from business_types table
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df <- balancing_accepted_aggr_offers(eic          = "10YHU-MAVIR----U",
#'                                      period_start = ymd(x = "2020-02-01",
#'                                                         tz = "CET"),
#'                                      period_end   = ymd(x = "2020-03-01",
#'                                                         tz = "CET"),
#'                                      tidy_output  = TRUE,
#'                                      reserve_type = "A96")
#' str(df)
#'
balancing_accepted_aggr_offers <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  reserve_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {

  # checking if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }

  # checking if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # converting timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

  # composing GET request url for a (maximum) 1 year long period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A82",
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    {if (is.null(reserve_type)) "" else paste0("&businessType=", reserve_type)},
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Activated Balancing Reserves (17.1.E)
#'
#' @description
#' The amount of activated balancing energy.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param reserve_type Defaults to NULL, otherwise choose among the
#'                     list of reserve type codes (A95, A96, A97, A98)
#'                     from business_types table
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df <- balancing_activated_reserves(eic          = "10YHU-MAVIR----U",
#'                                    period_start = ymd(x = "2020-02-01",
#'                                                       tz = "CET"),
#'                                    period_end   = ymd(x = "2020-03-01",
#'                                                       tz = "CET"),
#'                                    tidy_output  = TRUE,
#'                                    reserve_type = "A96")
#' str(df)
#'
balancing_activated_reserves <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  reserve_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {

  # checking if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }

  # checking if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # converting timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

  # composing GET request url for a (maximum) 1 year long period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A83",
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    {if (is.null(reserve_type)) "" else paste0("&businessType=", reserve_type)},
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}
