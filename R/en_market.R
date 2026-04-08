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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::implicit_offered_transfer_capacity(
#'   eic_in = "10Y1001A1001A82H",
#'   eic_out = "10YDK-1--------W",
#'   period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
implicit_offered_transfer_capacity <- function( # nolint: object_length_linter
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  assert_choice(x = contract_type, choices = c("A01", "A07"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A31",
      "&auction.Type=A01",
      "&contract_MarketAgreement.Type=", contract_type,
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::explicit_offered_transfer_capacity(
#'   eic_in = "10YBE----------2",
#'   eic_out = "10YGB----------A",
#'   period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
explicit_offered_transfer_capacity <- function( # nolint: object_length_linter
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  assert_choice(
    x = contract_type,
    choices = c("A01", "A02", "A03", "A04", "A06", "A07", "A08")
  )
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A31",
      "&auction.Type=A02",
      "&contract_MarketAgreement.Type=", contract_type,
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::continuous_offered_transfer_capacity(
#'   eic_in = "10YNL----------L",
#'   eic_out = "10YBE----------2",
#'   period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
continuous_offered_transfer_capacity <- function( # nolint: object_length_linter
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A31",
      "&auction.Type=A08",
      "&contract_MarketAgreement.Type=A07",
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Implicit Offered Transfer Capacities — Day-Ahead & Intraday (11.1)
#'
#' @description
#' Convenience wrapper that queries the day-ahead (A01) or intraday (A07)
#' implicit offered transfer capacities.
#'
#' @param eic_in Energy Identification Code of the bidding zone
#'               or control area (TSO)
#' @param eic_out Energy Identification Code of the bidding zone
#'                or control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param contract_type Contract market agreement type;
#'                      "A01" = Day ahead
#'                      "A07" = Intraday
#'                      Defaults to "A01" (Day ahead)
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df1 <- entsoeapi::implicit_offered_transfer_capacities(
#'   eic_in = "10Y1001A1001A82H",
#'   eic_out = "10YDK-1--------W",
#'   period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df1)
#'
#' df2 <- entsoeapi::implicit_offered_transfer_capacities(
#'   eic_in = "10YFR-RTE------C",
#'   eic_out = "10YCH-SWISSGRIDZ",
#'   period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
#'   contract_type = "A07",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df2)
#'
implicit_offered_transfer_capacities <- function( # nolint: object_length_linter
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(x = Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  assert_choice(x = contract_type, choices = c("A01", "A07"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A31",
      "&auction.Type=A01",
      "&contract_MarketAgreement.Type=", contract_type,
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Explicit Offered Transfer Capacities — All Contract Types (11.1.A)
#'
#' @description
#' Convenience wrapper that queries explicit offered transfer capacities
#' across **all** contract types (day-ahead, weekly, monthly, yearly,
#' long-term, intraday, quarterly) and returns the combined result.
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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::explicit_offered_transfer_capacities(
#'   eic_in = "10YBE----------2",
#'   eic_out = "10YGB----------A",
#'   period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
explicit_offered_transfer_capacities <- function( # nolint: object_length_linter
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  check_sec_token(security_token = security_token)
  assert_choice(
    x = contract_type,
    choices = c("A01", "A02", "A03", "A04", "A06", "A07", "A08")
  )
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A31",
      "&auction.Type=A02",
      "&contract_MarketAgreement.Type=", contract_type,
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Continuous Offered Transfer Capacities (11.1)
#'
#' @description
#' Continuous offered transfer capacities for the intraday continuous market.
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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::continuous_offered_transfer_capacities(
#'   eic_in = "10YNL----------L",
#'   eic_out = "10YBE----------2",
#'   period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
continuous_offered_transfer_capacities <- function( # nolint: object_length_linter
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A31",
      "&auction.Type=A08",
      "&contract_MarketAgreement.Type=A07",
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice assert_flag
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::flow_based_allocations(
#'   eic = "10YDOM-REGION-1V",
#'   period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
#'   process_type = "A32",
#'   archive = TRUE,
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
flow_based_allocations <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  process_type = "A43",
  archive = FALSE,
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(
    process_type,
    choices = c("A32", "A33", "A43", "A44")
  )
  assert_flag(archive)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=B09",
      "&processType=", process_type,
      if (archive) "&StorageType=archive" else "",
      "&in_Domain=", eic,
      "&out_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::auction_revenue(
#'   eic_in = "10YBA-JPCC-----D",
#'   eic_out = "10YHR-HEP------M",
#'   period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
auction_revenue <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  assert_choice(
    x = contract_type,
    choices = c("A01", "A02", "A03", "A04", "A06", "A07", "A08")
  )
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A25",
      "&businessType=B07",
      "&contract_MarketAgreement.Type=", contract_type,
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::total_nominated_capacity(
#'   eic_in = "10YDE-VE-------2",
#'   eic_out = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-03-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
total_nominated_capacity <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A26",
      "&businessType=B08",
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::already_allocated_total_capacity(
#'   eic_in = "10YDE-VE-------2",
#'   eic_out = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2025-02-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2025-02-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
already_allocated_total_capacity <- function( # nolint: object_length_linter
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  auction_category = "A04",
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  check_sec_token(security_token = security_token)
  assert_choice(
    x = auction_category,
    choices = c("A01", "A02", "A03", "A04")
  )
  assert_choice(
    x = contract_type,
    choices = c("A01", "A02", "A03", "A04", "A06", "A07", "A08")
  )
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A26",
      "&businessType=A29",
      "&contract_MarketAgreement.Type=", contract_type,
      "&auction.Category=", auction_category,
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Day-Ahead or Intraday Energy Prices (12.1.D)
#'
#' @description
#' Prices in currency/MWh created on spot (Day-Ahead or Intraday) market.
#' The data is delivered for each market time unit.
#'
#' @param eic Energy Identification Code of the related domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param contract_type Contract market agreement type;
#'                      "A01" = Day ahead
#'                      "A07" = Intraday
#'                      Defaults to "A01" (Day ahead)
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df1 <- entsoeapi::energy_prices(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df1)
#'
#' df2 <- entsoeapi::energy_prices(
#'   eic = "10YES-REE------0",
#'   period_start = lubridate::ymd(x = "2026-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2026-01-02", tz = "CET"),
#'   contract_type = "A07",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df2)
#'
energy_prices <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(x = contract_type, choices = c("A01", "A07"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A44",
      "&in_Domain=", eic,
      "&out_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end,
      "&contract_MarketAgreement.type=", contract_type
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Implicit Auction — Net Positions (12.1.E)
#'
#' @description
#' Net positions resulting from implicit auctions per bidding zone.
#'
#' @param eic Energy Identification Code of the bidding zone
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param contract_type Contract market agreement type;
#'                      "A01" = Day ahead
#'                      "A07" = Intraday
#'                      Defaults to "A01" (Day ahead)
#' @param tidy_output Defaults to FALSE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::net_positions(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
net_positions <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(x = contract_type, choices = c("A01", "A07"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A25",
      "&businessType=B09",
      "&contract_MarketAgreement.Type=", contract_type,
      "&in_Domain=", eic,
      "&out_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Implicit and Flow-based Allocations — Congestion Income (12.1.E)
#'
#' @description
#' Congestion income from implicit and flow-based allocations.
#' For implicit allocations, In_Domain and Out_Domain must be the same
#' border EIC code. For flow-based, both must be the same bidding zone EIC.
#'
#' @param eic Energy Identification Code of the border or bidding zone
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param contract_type Contract market agreement type, valid values
#'                      can be checked from contract_types table;
#'                      "A01" = Day ahead
#'                      "A02" = Weekly
#'                      "A03" = Monthly
#'                      "A04" = Yearly
#'                      "A06" = Long Term
#'                      "A07" = Intraday
#'                      "A08" = Quarterly
#'                      Defaults to "A01" (Day ahead)
#' @param tidy_output Defaults to FALSE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::congestion_income(
#'   eic = "10YDOM-1001A083J",
#'   period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
#'   contract_type = "A01",
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
congestion_income <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(
    x = contract_type,
    choices = c("A01", "A02", "A03", "A04", "A06", "A07", "A08")
  )
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # compose GET request url for the denoted period
  run_api_query(
    query_string = paste0(
      "documentType=A25",
      "&businessType=B10",
      "&contract_MarketAgreement.Type=", contract_type,
      "&in_Domain=", eic,
      "&out_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Transfer Capacities Allocated with Third Countries — Explicit (12.1.H)
#'
#' @description
#' Capacity allocated outside the EU via explicit auction (auction.Type=A02).
#' Minimum time interval in query response ranges from part of day to year,
#' depending on the selected contract type. A 100-document limit applies.
#'
#' @param eic_in Energy Identification Code of the in domain
#' @param eic_out Energy Identification Code of the out domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param contract_type Contract market agreement type, valid values
#'                      can be checked from contract_types table;
#'                      "A01" = Day ahead
#'                      "A02" = Weekly
#'                      "A03" = Monthly
#'                      "A04" = Yearly
#'                      "A06" = Long Term
#'                      "A07" = Intraday
#'                      "A08" = Quarterly
#'                      Defaults to "A01" (Day ahead)
#' @param auction_category Optional auction category;
#'                         "A01" = Base
#'                         "A02" = Peak
#'                         "A03" = Off Peak
#'                         "A04" = Hourly
#'                         Defaults to "A04"
#' @param position Integer position for ts_classification_sequence_position.
#'                 Defaults to 1.
#' @param tidy_output Defaults to FALSE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice assert_count
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::allocated_transfer_capacities_3rd_countries(
#'   eic_in = "10YSK-SEPS-----K",
#'   eic_out = "10YUA-WEPS-----0",
#'   period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
#'   contract_type = "A01",
#'   auction_category = "A04",
#'   position = 1L,
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
allocated_transfer_capacities_3rd_countries <- function( # nolint: object_length_linter
  eic_in = NULL,
  eic_out = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  contract_type = "A01",
  auction_category = "A04",
  position = 1L,
  tidy_output = FALSE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  assert_choice(
    x = contract_type,
    choices = c("A01", "A02", "A03", "A04", "A06", "A07", "A08")
  )
  assert_choice(
    x = auction_category,
    choices = c("A01", "A02", "A03", "A04")
  )
  assert_count(position, positive = TRUE)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A94",
      "&auction.Type=A02",
      "&contract_MarketAgreement.Type=", contract_type,
      "&in_Domain=", eic_in,
      "&out_Domain=", eic_out,
      if (!is.null(auction_category)) {
        paste0("&auction.Category=", auction_category)
      },
      if (!is.null(position)) {
        paste0(
          "&classificationSequence_AttributeInstanceComponent.Position=",
          as.integer(position)
        )
      },
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Intraday Prices (12.1.D)
#'
#' @description
#' Prices in currency/MWh from the intraday market.
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
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::intraday_prices(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-02-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
intraday_prices <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A44",
      "&contract_MarketAgreement.Type=A07",
      "&in_Domain=", eic,
      "&out_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Aggregated Bids (12.3.D)
#'
#' @description
#' Aggregated filled and unfilled bids per market time unit.
#'
#' @param eic Energy Identification Code of the area domain
#' @param process_type Process type code:
#'                     "A51" = aFRR
#'                     "A46" = Replacement Reserve
#'                     "A47" = Manual Frequency Restoration Reserve
#'                     "A60" = mFRR with scheduled activation
#'                     "A61" = mFRR with direct activation
#'                     "A67" = Frequency Containment Reserve
#'                     "A68" = Automatic and Manual Frequency
#'                             Restoration Reserve
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family market endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days years
#' @importFrom checkmate assert_choice
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::aggregated_bids(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
aggregated_bids <- function(
  eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(
    x = process_type,
    choices = c("A51", "A46", "A47", "A60", "A61", "A67", "A68")
  )
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A24",
      "&processType=", process_type,
      "&area_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}
