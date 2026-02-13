utils::globalVariables(
  c(
    "url_posixct_format",
    "api_req_safe",
    "extract_response"
  )
)



# Expansion and Dismantling Projects (9.1)  @@@@
# 100 documents limit applies
# Time interval in query response depends on duration of matching projects
# Mandatory parameters:
# - DocumentType
# - In_Domain
# - Out_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional parameters:
# - BusinessType
# - DocStatus
# GET /api?documentType=A90
# &businessType=B01
# &in_Domain=10YCZ-CEPS-----N
# &out_Domain=10YSK-SEPS-----K
# &periodStart=201512312300
# &periodEnd=201612312300



#' @title
#' Get Forecasted Transfer Capacities (11.1.A)
#'
#' @description
#' forecasted transfer capacities (MW) per direction between areas.
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
#' df1 <- entsoeapi::forecasted_transfer_capacities(
#'   eic_in = "10YCZ-CEPS-----N",
#'   eic_out = "10YSK-SEPS-----K",
#'   period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df1)
#'
#' df2 <- entsoeapi::forecasted_transfer_capacities(
#'   eic_in = "10YDK-1--------W",
#'   eic_out = "10Y1001A1001A82H",
#'   period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df2)
#'
forecasted_transfer_capacities <- function(
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
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for the denoted period
  query_string <- paste0(
    "documentType=A61",
    "&contract_MarketAgreement.Type=A01",
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
  extract_response(content = en_cont_list, tidy_output = tidy_output)
}



# Cross Border Capacity of DC Links - Intraday Transfer Limits  @@@@
# 4.2.5. Intraday Transfer Limits [11.3]
# One year range limit applies
# Minimum time interval in query response ranges from part of day up to one day
# Mandatory parameters:
# - DocumentType
# - In_Domain
# - Out_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A93
# &in_Domain=10YFR-RTE------C
# &out_Domain=10YGB----------A
# &periodStart=201512312300
# &periodEnd=201612312300



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
#' df1 <- entsoeapi::day_ahead_commercial_sched(
#'   eic_in = "10YCZ-CEPS-----N",
#'   eic_out = "10YSK-SEPS-----K",
#'   period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#' str(df1)
#'
#' df2 <- entsoeapi::day_ahead_commercial_sched(
#'   eic_in = "10YDK-1--------W",
#'   eic_out = "10Y1001A1001A82H",
#'   period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#' str(df2)
#'
day_ahead_commercial_sched <- function(
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
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for the denoted period
  query_string <- paste0(
    "documentType=A09",
    "&contract_MarketAgreement.Type=A01",
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
  extract_response(content = en_cont_list, tidy_output = tidy_output)
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
#' df1 <- entsoeapi::total_commercial_sched(
#'   eic_in = "10YCZ-CEPS-----N",
#'   eic_out = "10YSK-SEPS-----K",
#'   period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#' str(df1)
#'
#' df2 <- entsoeapi::total_commercial_sched(
#'   eic_in = "10YDK-1--------W",
#'   eic_out = "10Y1001A1001A82H",
#'   period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#' str(df2)
#'
total_commercial_sched <- function(
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

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for the denoted period
  query_string <- paste0(
    "documentType=A09",
    "&contract_MarketAgreement.Type=A05",
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
  extract_response(content = en_cont_list, tidy_output = tidy_output)
}



#' @title
#' Get Cross-Border Physical Flows (12.1.G)
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
#' df1 <- entsoeapi::cross_border_physical_flows(
#'   eic_in = "10Y1001A1001A83F",
#'   eic_out = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2020-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df1)
#'
#' df2 <- entsoeapi::cross_border_physical_flows(
#'   eic_in = "10YCZ-CEPS-----N",
#'   eic_out = "10Y1001A1001A83F",
#'   period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2020-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df2)
#'
cross_border_physical_flows <- function(
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
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for the denoted period
  query_string <- paste0(
    "documentType=A11",
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
  extract_response(content = en_cont_list, tidy_output = tidy_output)
}



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
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' # Germany's cross-border redispatching between TenneT and 50Hertz TSO.
#' df <- entsoeapi::redispatching_cross_border(
#'   eic_in = "10YDE-EON------1",
#'   eic_out = "10YDE-VE-------2",
#'   period_start = lubridate::ymd(x = "2024-09-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-10-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
redispatching_cross_border <- function(
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
  extract_response(content = en_cont_list, tidy_output = tidy_output)
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
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' # Netherlands' internal redispatching.
#' df <- entsoeapi::redispatching_internal(
#'   eic = "10YNL----------L",
#'   period_start = lubridate::ymd(x = "2023-11-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-12-01", tz = "CET"),
#'   tidy_output = TRUE
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
  extract_response(content = en_cont_list, tidy_output = tidy_output)
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
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' # Counter trading between Germany and Denmark.
#' df <- entsoeapi::countertrading(
#'   eic_in = "10Y1001A1001A82H",
#'   eic_out = "10YDK-1--------W",
#'   period_start = lubridate::ymd(x = "2024-09-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-10-01", tz = "CET"),
#'   tidy_output = TRUE
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
  extract_response(content = en_cont_list, tidy_output = tidy_output)
}



#' @title
#' Get Costs of Congestion Management (13.1.C)
#'
#' @description
#' Costs of TSO for redispatching and counter trading together
#' with costs for any other remedial actions taken to relieve
#' congested lines in transmission grid.
#'
#' @param eic Energy Identification Code of the control area
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param event_nature "A46" for system Operator redispatching
#'                     "B03" for counter trade
#'                     "B04" for congestion costs
#'                     Defaults to NULL which means both of them.
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' # Belgium's Costs of Congestion Management
#' df <- entsoeapi::costs_of_congestion_management(
#'   eic = "10YBE----------2",
#'   period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2016-12-31", tz = "CET"),
#'   tidy_output = TRUE
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

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
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
  extract_response(content = en_cont_list, tidy_output = tidy_output)
}
