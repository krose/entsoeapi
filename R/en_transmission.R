utils::globalVariables(
  c(
    "url_posixct_format",
    "api_req_safe",
    "extract_response"
  )
)



#' @title
#' Get Cross-Border Physical Flow (12.1.G)
#'
#' @description
#' It is the measured real flow of energy between
#' the neighbouring areas on the cross borders.
#'
#' @param eic_in Energy Identification Code of in domain
#' @param eic_out Energy Identification Code of out domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   Minimum time interval in query response is an MTU period,
#'                   but 1 year range limit applies.
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df1 <- transm_x_border_phys_flow(
#'   eic_in       = "10Y1001A1001A83F",
#'   eic_out      = "10YCZ-CEPS-----N",
#'   period_start = ymd(x = "2020-01-01", tz = "CET"),
#'   period_end   = ymd(x = "2020-01-02", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df1)
#'
#' df2 <- transm_x_border_phys_flow(
#'   eic_in       = "10YCZ-CEPS-----N",
#'   eic_out      = "10Y1001A1001A83F",
#'   period_start = ymd(x = "2020-01-01", tz = "CET"),
#'   period_end   = ymd(x = "2020-01-02", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df2)
#'
transm_x_border_phys_flow <- function(
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

  # compose GET request url for the denoted period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A11",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Day-Ahead Forecasted Transfer Capacities (11.1)
#'
#' @description
#' Day-ahead forecasted transmission capacities (MW)
#' per direction between areas.
#'
#' @param eic_in Energy Identification Code of in domain
#' @param eic_out Energy Identification Code of out domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df1 <- transm_day_ahead_transf_cap(
#'   eic_in       = "10YCZ-CEPS-----N",
#'   eic_out      = "10YSK-SEPS-----K",
#'   period_start = ymd(x = "2019-11-01", tz = "CET"),
#'   period_end   = ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df1)
#'
#' df2 <- transm_day_ahead_transf_cap(
#'   eic_in       = "10YDK-1--------W",
#'   eic_out      = "10Y1001A1001A82H",
#'   period_start = ymd(x = "2019-11-01", tz = "CET"),
#'   period_end   = ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df2)
#'
transm_day_ahead_transf_cap <- function(
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

  # compose GET request url for the denoted period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A61",
    "&contract_MarketAgreement.Type=A01",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Day-Ahead Commercial Schedules (12.1.F)
#'
#' @description
#' Day-ahead commercial exchanges in aggregated form between
#' bidding zones per direction and market time unit.
#'
#' @param eic_in Energy Identification Code of in domain
#' @param eic_out Energy Identification Code of out domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df1 <- transm_day_ahead_comm_sched(
#'   eic_in       = "10YCZ-CEPS-----N",
#'   eic_out      = "10YSK-SEPS-----K",
#'   period_start = ymd(x = "2019-11-01", tz = "CET"),
#'   period_end   = ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df1)
#'
#' df2 <- transm_day_ahead_comm_sched(
#'   eic_in       = "10YDK-1--------W",
#'   eic_out      = "10Y1001A1001A82H",
#'   period_start = ymd(x = "2019-11-01", tz = "CET"),
#'   period_end   = ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df2)
#'
transm_day_ahead_comm_sched <- function(
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

  # compose GET request url for the denoted period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A09",
    "&contract_MarketAgreement.Type=A01",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Total Commercial Schedules (12.1.F)
#'
#' @description
#' Aggregated capacity nominated for all time horizons
#' (including Intra-Day) corresponding to implicit and
#' explicit allocations after each nomination process.
#'
#' @param eic_in Energy Identification Code of in domain
#' @param eic_out Energy Identification Code of out domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df1 <- transm_total_comm_sched(eic_in       = "10YCZ-CEPS-----N",
#'                                eic_out      = "10YSK-SEPS-----K",
#'                                period_start = ymd(
#'                                  x = "2019-11-01",
#'                                  tz = "CET"
#'                                ),
#'                                period_end   = ymd(
#'                                  x = "2019-12-01",
#'                                  tz = "CET"
#'                                ),
#'                                tidy_output  = TRUE)
#'
#' str(df1)
#'
#' df2 <- transm_total_comm_sched(eic_in       = "10YDK-1--------W",
#'                                eic_out      = "10Y1001A1001A82H",
#'                                period_start = ymd(
#'                                  x = "2019-11-01",
#'                                  tz = "CET"
#'                                ),
#'                                period_end   = ymd(
#'                                  x = "2019-12-01",
#'                                  tz = "CET"
#'                                ),
#'                                tidy_output  = TRUE)
#'
#' str(df2)
#'
transm_total_comm_sched <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
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

  # compose GET request url for the denoted period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A09",
    "&contract_MarketAgreement.Type=A05",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Day-Ahead Prices (12.1.D)
#'
#' @description
#' Prices in currency/MWh created on spot (Day-Ahead) market.
#' The data is delivered for each market time unit.
#'
#' @param eic Energy Identification Code of the related domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df1 <- transm_day_ahead_prices(eic          = "10YCZ-CEPS-----N",
#'                                period_start = ymd(
#'                                  x = "2019-11-01",
#'                                  tz = "CET"
#'                                ),
#'                                period_end   = ymd(
#'                                  x = "2019-12-01",
#'                                  tz = "CET"
#'                                ),
#'                                tidy_output  = TRUE)
#' str(df1)
#'
#' df2 <- transm_day_ahead_prices(eic          = "10YDK-1--------W",
#'                                period_start = ymd(
#'                                  x = "2019-11-01",
#'                                  tz = "CET"
#'                                ),
#'                                period_end   = ymd(
#'                                  x = "2019-12-01",
#'                                  tz = "CET"
#'                                ),
#'                                tidy_output  = TRUE)
#' str(df2)
#'
transm_day_ahead_prices <- function(
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

  # compose GET request url for the denoted period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A44",
    "&in_Domain=", eic,
    "&out_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Total Nominated Capacity (12.1.B)
#'
#' @description
#' Aggregated capacity nominated by market participants from
#' time horizons (including Intra-Day) corresponding to explicit
#' allocations, agreed between the TSOs and confirmed
#' to the market.
#'
#' @param eic_in Energy Identification Code of in domain
#' @param eic_out Energy Identification Code of out domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df <- transm_total_nominated_cap(
#'   eic_in       = "10YDE-VE-------2",
#'   eic_out      = "10YCZ-CEPS-----N",
#'   period_start = ymd(x = "2019-02-01", tz = "CET"),
#'   period_end   = ymd(x = "2019-03-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df)
#'
transm_total_nominated_cap <- function(
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

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for the denoted period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A26",
    "&businessType=B08",
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Total Already Allocated Capacity (12.1.C)
#'
#' @description
#' Total capacity allocated, for all time horizons
#' (including Intra-Day) after each allocation process
#' per market time unit.
#'
#' @param eic_in Energy Identification Code of the bidding zone
#'               or control area (TSO)
#' @param eic_out Energy Identification Code of the bidding zone
#'                or control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param auction_type Auction category, valid values can be checked
#'                     from auction_types table;
#'                     Defaults to "A04" (Mixed)
#' @param contract_type Contract market agreement type, valid values
#'                      can be checked from contract_types table;
#'                      Defaults to "A05" (Total)
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df <- transm_already_allocated_cap(
#'   eic_in        = "10YDE-VE-------2",
#'   eic_out       = "10YCZ-CEPS-----N",
#'   period_start  = ymd(x = "2019-02-01", tz = "CET"),
#'   period_end    = ymd(x = "2019-02-02", tz = "CET"),
#'   auction_type  = "A02",
#'   contract_type = "A01"
#' )
#' str(df)
#'
transm_already_allocated_cap <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  auction_type = "A04",
  contract_type = "A05",
  tidy_output = FALSE,
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

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for the denoted period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A26",
    "&businessType=A29",
    "&Auction.Type=", auction_type,
    "&contract_MarketAgreement.Type=", contract_type,
    "&in_Domain=", eic_in,
    "&out_Domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}
