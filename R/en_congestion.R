# https://documenter.getpostman.com/view/7009892/2s93JtP3F6



#' @title title
#' Get Redispatching Cross Border (13.1.A)
#'
#' @description
#' Changes in production and load (increase or decrease) to
#' relieve congested internal lines that exceeds its capacity.
#' 100 documents limit applies!!
#'
#' @param eic_in Energy Identification Code of the control area
#' @param eic_out Energy Identification Code of the control area
#' @param period_start
#' POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end
#' POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output
#' Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token
#' Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' # Germany's cross-border redispatching between TenneT and 50Hertz TSO.
#' df <- redispatching_x_border(
#'   eic_in       = "10YDE-EON------1",
#'   eic_out      = "10YDE-VE-------2",
#'   period_start = ymd(x = "2024-09-01", tz = "CET"),
#'   period_end   = ymd(x = "2024-10-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df)
#'
redispatching_x_border <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1 || length(eic_out) > 1) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A63",
    "&businessType=A46",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Redispatching Internal (13.1.A)
#'
#' @description
#' Changes in production and load (increase or decrease) to
#' relieve internal congestion lines that exceeds its capacity.
#' 100 documents limit applies!!
#'
#' @param eic Energy Identification Code of the control area
#' @param period_start
#' POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end
#' POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output
#' Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token
#' Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' # Netherlands' internal redispatching.
#' df <- redispatching_internal(
#'   eic          = "10YNL----------L",
#'   period_start = ymd(x = "2023-11-01", tz = "CET"),
#'   period_end   = ymd(x = "2023-12-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df)
#'
redispatching_internal <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1) {
    stop("This wrapper only supports one EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A63",
    "&businessType=A85",
    "&in_Domain=", eic,
    "&out_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title title
#' Get Countertrading (13.1.B)
#'
#' @description
#' Buying or cancelling generation on different side of the border
#' to relieve congested cross-border lines that exceeds its capacity.
#' The time interval in the query response depends on duration of
#' matching counter trades
#' 100 documents limit applies!!
#'
#' @param eic_in Energy Identification Code of the control area/bidding zone
#' @param eic_out Energy Identification Code of the control area/bidding zone
#' @param period_start
#' POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end
#' POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output
#' Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token
#' Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' # Counter trading between Germany and Denmark.
#' df <- countertrading(
#'   eic_in       = "10Y1001A1001A82H",
#'   eic_out      = "10YDK-1--------W",
#'   period_start = ymd(x = "2024-09-01", tz = "CET"),
#'   period_end   = ymd(x = "2024-10-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df)
#'
countertrading <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1 || length(eic_out) > 1) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A91",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Costs of Congestion Management (13.1.C)
#'
#' @description
#' Costs of TSO for redispatching and counter trading together
#' with costs for any other remedial actions taken to relieve
#' congested lines in transmission grid.
#' 100 documents limit applies!!
#'
#' @param eic Energy Identification Code of the control area
#' @param period_start
#' POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end
#' POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param event_nature "A46" for system Operator redispatching
#'                     "B03" for counter trade
#'                     "B04" for congestion costs
#'                     Defaults to NULL which means both of them.
#' @param tidy_output
#' Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token
#' Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' # Czech's Costs of Congestion Management
#' df <- costs_of_congestion_management(
#'   eic          = "10YCZ-CEPS-----N",
#'   period_start = ymd(x = "2016-01-01", tz = "CET"),
#'   period_end   = ymd(x = "2017-01-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df)
#'
costs_of_congestion_management <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 31L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  event_nature = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1) {
    stop("This wrapper only supports one EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # check if event_nature value is valid
  if (isFALSE(event_nature %in% c("A46", "B03", "B04"))) {
    stop("The 'event_nature' parameter should be 'A46', 'B03', 'B04' or NULL.")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A92",
    "&in_Domain=", eic,
    "&out_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )
  if (!is.null(event_nature)) {
    query_string <- paste0(query_string, "&businessType=", event_nature)
  }

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}
