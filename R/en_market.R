utils::globalVariables(
  c(
    "url_posixct_format",
    "api_req_safe",
    "extract_response"
  )
)



#' @title
#' Get Implicit Offered Transfer Capacity (11.1)
#'
#' @description
#' Implicit offered transfer capacity values.
#'
#' @param eic_in Energy Identification Code of the bidding zone
#'               or control area (TSO)
#' @param eic_out Energy Identification Code of the bidding zone
#'                or control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param contract_type Contract market agreement type
#'                      A01 = Day ahead
#'                      A07 = Intraday
#'                      Defaults to "A01" (Day ahead)
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::implicit_offered_transfer_capacity(
#'   eic_in = "10Y1001A1001A82H",
#'   eic_out = "10YDK-1--------W",
#'   period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
implicit_offered_transfer_capacity <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1L || length(eic_out) > 1L) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }

  # check if contract_type value is valid
  if (isFALSE(contract_type %in% c("A01", "A07"))) {
    stop("The 'contract_type' parameter should be 'A01' or 'A07'.")
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
    "documentType=A31",
    "&auction.Type=A01",
    "&contract_MarketAgreement.Type=", contract_type,
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
#' Get Explicit Offered Transfer Capacity (11.1.A)
#'
#' @description
#' Explicit offered transfer capacity values.
#'
#' @param eic_in Energy Identification Code of the bidding zone
#'               or control area (TSO)
#' @param eic_out Energy Identification Code of the bidding zone
#'                or control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param contract_type Contract market agreement type, valid values
#'                      can be checked from contract_types table;
#'                      A01 = Day ahead
#'                      A02 = Weekly
#'                      A03 = Monthly
#'                      A04 = Yearly
#'                      A06 = Long Term
#'                      A07 = Intraday
#'                      A08 = Quarterly
#'                      Defaults to "A01" (Day ahead)
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::explicit_offered_transfer_capacity(
#'   eic_in = "10YBE----------2",
#'   eic_out = "10YGB----------A",
#'   period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
explicit_offered_transfer_capacity <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1L || length(eic_out) > 1L) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }

  # check if contract_type value is valid
  if (isFALSE(contract_type %in% c("A01", "A02", "A03", "A04", "A06", "A07",
                                   "A08"))) {
    stop(
      "The 'contract_type' parameter should be 'A01', 'A02', 'A03', 'A04', ",
      "'A06', 'A07' or 'A08'."
    )
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
    "documentType=A31",
    "&auction.Type=A02",
    "&contract_MarketAgreement.Type=", contract_type,
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
#' Get Continuous Offered Transfer Capacity (11.1)
#'
#' @description
#' Continuous offered transfer capacity values.
#'
#' @param eic_in Energy Identification Code of the bidding zone
#'               or control area (TSO)
#' @param eic_out Energy Identification Code of the bidding zone
#'                or control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::continuous_offered_transfer_capacity(
#'   eic_in = "10YNL----------L",
#'   eic_out = "10YBE----------2",
#'   period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
continuous_offered_transfer_capacity <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1L || length(eic_out) > 1L) {
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
    "documentType=A31",
    "&auction.Type=A08",
    "&contract_MarketAgreement.Type=A07",
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
#' Get Flow Based Allocations (11.1.B)
#'
#' @description
#' Flow based capacity allocated, for all time horizons.
#'
#' @param eic Energy Identification Code of the area
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param process_type Contract market agreement type, valid values
#'                      can be checked from process_types table;
#'                      "A32" = Month-ahead
#'                      "A33" = Year-ahead
#'                      "A43" = Day ahead
#'                      "A44" = Intraday
#'                      Defaults to "A43" (Day ahead)
#' @param archive Defaults to FALSE, set to TRUE if archives to be queried.
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df1 <- entsoeapi::flow_based_allocations(
#'   eic = "10Y1001A1001A91G",
#'   period_start = lubridate::ymd(x = "2025-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2026-01-01", tz = "CET"),
#'   process_type = "A43",
#'   archive = FALSE,
#'   tidy_output = TRUE
#' )
#'
#' str(df1)
#'
#' df2 <- entsoeapi::flow_based_allocations(
#'   eic = "10YDOM-REGION-1V",
#'   period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
#'   process_type = "A32",
#'   archive = TRUE,
#'   tidy_output = TRUE
#' )
#'
#' str(df2)
#'
flow_based_allocations <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  process_type = "A43",
  archive = FALSE,
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One 'in' control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one EIC per request.")
  }

  # check if valid process_type provided
  if (!process_type %in% c("A32", "A33", "A43", "A44")) {
    stop("The 'process_type' parameter should be 'A32', 'A33', 'A43' or 'A44'.")
  }

  # check if 'archive' is logical
  if (!is.logical(archive)) {
    stop("The 'archive' argument should be TRUE or FALSE!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for the denoted period
  query_string <- paste0(
    "documentType=B09",
    "&processType=", process_type,
    if (archive) "&StorageType=archive" else "",
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



# 4.2.6. Explicit Allocations (Use of the Transfer Capacity) [12.1.A]
# 100 documents limit applies
# Minimum time interval in query response ranges from part of day to year,
# depending on selected Contract_MarketAgreement.Type
# Mandatory parameters:
# - DocumentType
# - BusinessType
# - Contract_MarketAgreement.Type
# - In_Domain
# - Out_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional parameters:
# - Auction.Category
# - ClassificationSequence_AttributeInstanceComponent.Position
# GET /api?documentType=A25
# &businessType=B05
# &contract_MarketAgreement.Type=A01
# &in_Domain=10YSK-SEPS-----K
# &out_Domain=10YCZ-CEPS-----N
# &auction.Category=A01
# &classificationSequence_AttributeInstanceComponent.Position=1
# &periodStart=201601012300
# &periodEnd=201601022300



#' @title
#' Get the Auction Revenue (12.1.A)
#'
#' @description
#' Explicit Allocations - Auction Revenue.
#'
#' @param eic_in Energy Identification Code of the bidding zone
#'               or control area (TSO)
#' @param eic_out Energy Identification Code of the bidding zone
#'                or control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param contract_type Contract market agreement type, valid values
#'                      can be checked from contract_types table;
#'                      "A01" = Daily
#'                      "A02" = Weekly
#'                      "A03" = Monthly
#'                      "A04" = Yearly
#'                      "A06" = Long Term
#'                      "A07" = Intraday
#'                      "A08" = Quarterly
#'                      Defaults to "A01" (Daily)
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::auction_revenue(
#'   eic_in = "10YBA-JPCC-----D",
#'   eic_out = "10YHR-HEP------M",
#'   period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
auction_revenue <- function(
    eic_in = NULL,
    eic_out = NULL,
    period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                  tz = "CET"),
    period_end = lubridate::ymd(Sys.Date(),
                                tz = "CET"),
    contract_type = "A01",
    tidy_output = FALSE,
    security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1L || length(eic_out) > 1L) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }

  # check if contract_type value is valid
  if (isFALSE(contract_type %in% c("A01", "A02", "A03", "A04", "A06", "A07",
                                   "A08"))) {
    stop(
      "The 'contract_type' parameter should be 'A01', 'A02', 'A03', 'A04', ",
      "'A06', 'A07' or 'A08'."
    )
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
    "documentType=A25",
    "&businessType=B07",
    "&contract_MarketAgreement.Type=", contract_type,
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
#' df <- entsoeapi::total_nominated_capacity(
#'   eic_in       = "10YDE-VE-------2",
#'   eic_out      = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2019-03-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' str(df)
#'
total_nominated_capacity <- function(
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
  if (length(eic_in) > 1L || length(eic_out) > 1L) {
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
  query_string <- paste0(
    "documentType=A26",
    "&businessType=B08",
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
#' Get Already Allocated Total Capacity (12.1.C)
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
#' @param auction_category
#'                        A01 = Base
#'                        A02 = Peak
#'                        A03 = Off Peak
#'                        A04 = Hourly
#'                        Defaults to "A04" (Hourly)
#' @param contract_type Contract market agreement type, valid values
#'                      can be checked from contract_types table;
#'                      A01 = Daily
#'                      A02 = Weekly
#'                      A03 = Monthly
#'                      A04 = Yearly
#'                      A06 = Long Term
#'                      A07 = Intraday
#'                      A08 = Quarterly
#'                      Defaults to "A01" (Daily)
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::already_allocated_total_capacity(
#'   eic_in = "10YDE-VE-------2",
#'   eic_out = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2025-02-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2025-02-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#' str(df)
#'
already_allocated_total_capacity <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  auction_category = "A04",
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1L || length(eic_out) > 1L) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # check if the auction_category value is valid
  if (isFALSE(auction_category %in% c("A01", "A02", "A03", "A04"))) {
    stop("The auction_category shoud be 'A01', 'A02', 'A03' or 'A04'")
  }

  # check if contract_type value is valid
  if (isFALSE(contract_type %in% c("A01", "A02", "A03", "A04", "A06", "A07",
                                   "A08"))) {
    stop(
      "The 'contract_type' parameter should be 'A01', 'A02', 'A03', 'A04', ",
      "'A06', 'A07' or 'A08'."
    )
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for the denoted period
  query_string <- paste0(
    "documentType=A26",
    "&businessType=A29",
    "&contract_MarketAgreement.Type=", contract_type,
    "&auction.Category=", auction_category,
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
#' df1 <- entsoeapi::day_ahead_prices(
#'   eic          = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2019-12-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#' str(df1)
#'
#' df2 <- entsoeapi::day_ahead_prices(
#'   eic          = "10Y1001A1001A82H",
#'   period_start = lubridate::ymd(x = "2026-02-13", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2026-02-13", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#' str(df2)
#'
day_ahead_prices <- function(
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
  if (length(eic) > 1L) {
    stop("This wrapper only supports one EIC per request.")
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
    "documentType=A44",
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



# Implicit Allocations - Net positions  @@@@
# Intraday Implicit Allocations - Congestion Income  @@@@
# Daily Implicit Allocations - Congestion Income  @@@@
# Daily Flow Based Allocations - Congestion Income  @@@@
# 4.2.11. Implicit Auction — Net Positions [12.1.E]
# One year range limit applies
# Minimum time interval in query response is one day
# Mandatory parameters:
# - DocumentType
# - BusinessType
# - Contract_MarketAgreement.Type
# - In_Domain
# - Out_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# - In_Domain and Out_Domain must be populated with the same
#   bidding zone EIC code
# GET /api?documentType=A25
# &businessType=B09
# &contract_MarketAgreement.Type=A01
# &in_Domain=10YCZ-CEPS-----N
# &out_Domain=10YCZ-CEPS-----N
# &periodStart=201512312300
# &periodEnd=201612312300



# 4.2.12. Implicit Auction — Congestion Income [12.1.E]
# 100 documents limit applies
# Minimum time interval in query response ranges from part of day to one day,
# depending on selected Contract_MarketAgreement.Type
# Mandatory parameters:
# - DocumentType
# - BusinessType
# - Contract_MarketAgreement.Type
# - In_Domain
# - Out_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# For implicit allocations, In_Domain and Out_Domain must be populated with
# the same border EIC code. For flow-based, they must be populated with
# the same bidding zone EIC code
# GET /api?documentType=A25
# &businessType=B10
# &contract_MarketAgreement.Type=A01
# &in_Domain=10YDOM-1001A083J
# &out_Domain=10YDOM-1001A083J
# &periodStart=201601012300
# &periodEnd=201601022300



# Transfer Capacities Allocated with Third Countries (Implicit)
# Transfer Capacities Allocated with Third Countries
# 4.2.16. Capacity Allocated Outside EU [12.1.H]  @@@@
# 100 documents limit applies
# Minimum time interval in query response ranges from part of day to year,
# depending on selected Contract_MarketAgreement.Type
# Mandatory parameters:
# - DocumentType
# - Auction.Type
# - Contract_MarketAgreement.Type
# - In_Domain
# - Out_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional parameters:
# - Auction.Category
# - ClassificationSequence_AttributeInstanceComponent.Position
# GET /api?documentType=A94
# &contract_MarketAgreement.Type=A01
# &in_Domain=10YSK-SEPS-----K
# &out_Domain=10YUA-WEPS-----0
# &auction.Type=A02
# &auction.Category=A04
# &classificationSequence_AttributeInstanceComponent.Position=1
# &periodStart=201601012300
# &periodEnd=201601022300
