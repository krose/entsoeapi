#' @title
#' Get Elastic Demands (IFs aFRR 3.4 & mFRR 3.4)
#'
#' @description
#' Elastic demands for scheduled activation of standard aFRR/mFRR product.
#'
#' @param eic Energy Identification Code of the scheduling area
#' @param process_type type of frequency restoration reserve
#'                     "A47" mFRR
#'                     "A51" aFRR
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::elastic_demands(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A47",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-08-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
elastic_demands <- function(
  eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(x = process_type, choices = c("A47", "A51"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A37",
      "&processType=", process_type,
      "&businessType=B75",
      "&Acquiring_domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Netted Volumes (IFs IN 3.10)
#'
#' @description
#' The net position informs whether the given area imports or exports energy.
#' Those rows which hold the queried eic value in the
#' 'ts_connecting_domain_mrid' column show the export value.
#' Those rows which hold the queried eic value in the
#' 'ts_acquiring_domain_mrid' column show the import value.
#'
#' @param eic Energy Identification Code of the area
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One day range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One day range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::netted_volumes(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-08-17", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
netted_volumes <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 day"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=B17",
      "&processType=A63",
      "&Acquiring_domain=", eic,
      "&Connecting_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Exchanged Volumes (aFRR 3.16, mFRR 3.17)
#'
#' @description
#' The net position informs whether the given area imports
#' or exports energy.
#' Those rows which hold the queried eic value in the
#' 'ts_connecting_domain_mrid' column
#' show the export value.
#' Those rows which hold the queried eic value in the
#' 'ts_acquiring_domain_mrid' column
#' show the import value.
#'
#' @param eic Energy Identification Code of the area
#' @param process_type type of frequency restoration reserve
#'                     "A51" aFRR
#'                     "A60" mFRR with scheduled activation
#'                     "A61" mFRR with direct activation
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One day range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One day range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::exchanged_volumes(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
#'   period_end = lubridate::ymd_hm(x = "2022-08-16 02:00", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
exchanged_volumes <- function(
  eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(x = process_type, choices = c("A51", "A60", "A61"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 day"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=B17",
      "&processType=", process_type,
      "&Acquiring_domain=", eic,
      "&Connecting_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Balancing Border Capacity Limitations (IFs 4.3 & 4.4)
#'
#' @description
#' This data item publish limitations on borders requested by
#' participating or affected TSOs.
#'
#' @param eic_in Energy Identification Code of an in LFC Area (LFA)
#'               or in Scheduling area (SCA)
#' @param eic_out Energy Identification Code of out an out LFC Area (LFA)
#'                or out Scheduling area (SCA)
#' @param process_type type of frequency restoration reserve
#'                     "A47" mFRR
#'                     "A51" aFRR
#'                     "A63" Imbalance Netting
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::balancing_border_cap_limit(
#'   eic_in = "10YDE-RWENET---I",
#'   eic_out = "10YBE----------2",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2022-06-22", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-06-23", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
balancing_border_cap_limit <- function(
  eic_in = NULL,
  eic_out = NULL,
  process_type = "A51",
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  assert_choice(x = process_type, choices = c("A47", "A51", "A63"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A31",
      "&BusinessType=A26",
      "&processType=", process_type,
      "&In_Domain=", eic_in,
      "&Out_Domain=", eic_out,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Netted & Exchanged Volumes Per Border (IFs 3.10, 3.16 & 3.17)
#'
#' @description
#' Netted and exchanged balancing energy volumes on a specific border between
#' an acquiring and a connecting domain.
#'
#' @param acquiring_eic Energy Identification Code of the acquiring area
#' @param connecting_eic Energy Identification Code of the connecting area
#' @param process_type type of frequency restoration reserve
#'                     "A51" aFRR
#'                     "A60" mFRR with scheduled activation
#'                     "A61" mFRR with direct activation
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One day range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One day range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::exchanged_volumes_per_border(
#'   acquiring_eic = "10YBE----------2",
#'   connecting_eic = "10YFR-RTE------C",
#'   process_type = "A60",
#'   period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
exchanged_volumes_per_border <- function(
  acquiring_eic = NULL,
  connecting_eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = acquiring_eic, var_name = "acquiring_eic")
  assert_eic(eic = connecting_eic, var_name = "connecting_eic")
  assert_choice(x = process_type, choices = c("A51", "A60", "A61"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 day"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A30",
      "&processType=", process_type,
      "&Acquiring_domain=", acquiring_eic,
      "&Connecting_Domain=", connecting_eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Netted & Exchanged Volumes Per Border (IFs 3.10, 3.16 & 3.17)
#'
#' @description
#' Netted and exchanged balancing energy volumes on a specific border.
#' Covers imbalance netting (3.10), aFRR exchange (3.16), and
#' mFRR exchange (3.17) per border between an acquiring and a
#' connecting domain.
#'
#' @param acquiring_eic Energy Identification Code of the acquiring area
#' @param connecting_eic Energy Identification Code of the connecting area
#' @param process_type type of balancing process
#'                     "A51" aFRR
#'                     "A60" mFRR with scheduled activation
#'                     "A61" mFRR with direct activation
#'                     "A63" Imbalance Netting
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One day range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One day range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' \dontrun{
#' df <- entsoeapi::netted_volumes_per_border(
#'   acquiring_eic = "10YBE----------2",
#'   connecting_eic = "10YFR-RTE------C",
#'   process_type = "A63",
#'   period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#' }
#'
netted_volumes_per_border <- function(
  acquiring_eic = NULL,
  connecting_eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = acquiring_eic, var_name = "acquiring_eic")
  assert_eic(eic = connecting_eic, var_name = "connecting_eic")
  assert_choice(
    x = process_type,
    choices = c("A51", "A60", "A61", "A63")
  )
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 day"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A30",
      "&processType=", process_type,
      "&Acquiring_domain=", acquiring_eic,
      "&Connecting_Domain=", connecting_eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get HVDC Link Constraints (IFs 4.5)
#'
#' @description
#' Permanent allocation limitations to cross-border capacity on HVDC lines.
#'
#' @param eic_in Energy Identification Code of the in domain
#' @param eic_out Energy Identification Code of the out domain
#' @param eic_ic Energy Identification Code of the interconnector
#'               If used, data for the given Transmission Asset is returned
#' @param process_type "A47" Manual frequency restoration reserve
#'                     "A51" Automatic Frequency Restoration Reserve
#'                     "A63" Imbalance Netting;
#'                     Defaults to "A63"
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' \dontrun{
#' df <- entsoeapi::hvdc_link_constrains(
#'   eic_in = "10YAT-APG------L",
#'   eic_out = "10YDE-RWENET---I",
#'   process_type = "A63",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#' }
#'
hvdc_link_constrains <- function(
  eic_in = NULL,
  eic_out = NULL,
  eic_ic = NULL,
  process_type = "A63",
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_in, var_name = "eic_in")
  assert_eic(eic = eic_out, var_name = "eic_out")
  assert_eic(eic = eic_ic, var_name = "eic_ic", null_ok = TRUE)
  assert_choice(x = process_type, choices = c("A47", "A51", "A63"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A99",
      "&processType=", process_type,
      "&BusinessType=B06",
      "&In_Domain=", eic_in,
      "&Out_Domain=", eic_out,
      if (is.null(eic_ic)) "" else "&registeredResource=", eic_ic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Changes to Bid Availability (IFs mFRR 9.9, aFRR 9.6 & 9.8)
#'
#' @description
#' Changes to bid availability for scheduled activation products.
#' 100 documents limit applies; paging is handled transparently.
#'
#' @param eic Energy Identification Code of the domain
#' @param business_type type of frequency restoration reserve
#'                      "C40" Conditional bid
#'                      "C41" Thermal limit
#'                      "C42" Frequency limit
#'                      "C43" Voltage limit
#'                      "C44" Current limit
#'                      "C45" Short-circuit current limits
#'                      "C46" Dynamic stability limit
#'                      Defaults to "C46".
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' \dontrun{
#' df <- entsoeapi::changes_to_bid_availability(
#'   eic = "10YCZ-CEPS-----N",
#'   business_type = "C46",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#' }
#'
changes_to_bid_availability <- function(
  eic = NULL,
  business_type = "C46",
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(
    business_type,
    choices = c("C40", "C41", "C42", "C43", "C44", "C45", "C46")
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
      "documentType=B45",
      "&processType=A47",
      "&businessType=", business_type,
      "&ControlArea_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Current Balancing State (GL EB 12.3.A)
#'
#' @description
#' Current balancing state of the control area.
#' 100 day range limit applies.
#'
#' @param eic Energy Identification Code of the area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     100 day range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   100 day range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::current_balancing_state(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
current_balancing_state <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "100 days"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A86",
      "&businessType=B33",
      "&Area_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Balancing Energy Bids (GL EB 12.3.B&C)
#'
#' @description
#' Balancing energy bids submitted by BSPs.
#' 100 documents limit applies; paging is handled transparently.
#'
#' @param eic Energy Identification Code of the connecting domain
#' @param process_type Process type (currently only A47 is supported)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' \dontrun{
#' df <- entsoeapi::balancing_energy_bids(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A47",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#' }
#'
balancing_energy_bids <- function(
  eic = NULL,
  process_type = "A47",
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A37",
      "&processType=", process_type,
      "&businessType=B74",
      "&Connecting_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Aggregated Balancing Energy Bids (GL EB 12.3.E)
#'
#' @description
#' Aggregated balancing energy bids.
#'
#' @param eic Energy Identification Code of the area domain
#' @param process_type type of frequency restoration reserve
#'                     "A51" aFRR
#'                     "A46" RR
#'                     "A47" mFRR
#'                     "A60" mFRR with scheduled activation
#'                     "A61" mFRR with direct activation
#'                     "A67" Central selection aFRR
#'                     "A68" Local selection aFRR
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::aggregated_balancing_energy_bids(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
aggregated_balancing_energy_bids <- function( # nolint: object_length_linter
  eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
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


#' @title
#' Get Procured Balancing Capacity (GL EB 12.3.F)
#'
#' @description
#' Procured balancing capacity by TSOs.
#' 100 documents limit applies; paging is handled transparently.
#'
#' @param eic Energy Identification Code of the area domain
#' @param process_type type of frequency restoration reserve
#'                     "A51" aFRR
#'                     "A52" FCR
#'                     "A47" mFRR
#' @param market_agreement_type Optional market agreement type code
#'                              "A01" Daily
#'                              "A02" Weekly
#'                              "A03" Monthly
#'                              "A04" Yearly
#'                              "A05" Total
#'                              "A06" Long term
#'                              "A07" Intraday
#'                              "A13" = Hourly
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::procured_balancing_capacity(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
procured_balancing_capacity <- function(
  eic = NULL,
  process_type = NULL,
  market_agreement_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(x = process_type, choices = c("A51", "A52", "A47"))
  assert_choice(
    x = market_agreement_type,
    choices = c("A01", "A02", "A03", "A04", "A05", "A06", "A07", "A13"),
    null.ok = TRUE
  )
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A15",
      "&processType=", process_type,
      "&area_Domain=", eic,
      if (!is.null(market_agreement_type)) {
        paste0("&type_MarketAgreement.Type=", market_agreement_type)
      },
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Allocation of Cross-Zonal Balancing Capacity (GL EB 12.3.H&I)
#'
#' @description
#' Use of allocated cross-zonal balancing capacity between
#' an acquiring and a connecting domain.
#'
#' @param eic_acquiring Energy Identification Code of the acquiring domain
#' @param eic_connecting Energy Identification Code of the connecting domain
#' @param market_agreement_type Optional market agreement type code
#'                              "A01" Daily
#'                              "A02" Weekly
#'                              "A06" Long term
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' \dontrun{
#' df <- entsoeapi::allocation_of_cross_zonal_balancing_cap(
#'   eic_acquiring = "10YAT-APG------L",
#'   eic_connecting = "10YCH-SWISSGRIDZ",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#' }
#'
allocation_of_cross_zonal_balancing_cap <- function( # nolint: object_length_linter
  eic_acquiring = NULL,
  eic_connecting = NULL,
  market_agreement_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_acquiring, var_name = "eic_acquiring")
  assert_eic(eic = eic_connecting, var_name = "eic_connecting")
  assert_choice(
    x = market_agreement_type,
    choices = c("A01", "A02", "A06"),
    null.ok = TRUE
  )
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A38",
      "&processType=A46",
      "&Acquiring_Domain=", eic_acquiring,
      "&Connecting_Domain=", eic_connecting,
      if (!is.null(market_agreement_type)) {
        paste0("&type_MarketAgreement.Type=", market_agreement_type)
      },
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Contracted Reserves (17.1.B&C)
#'
#' @description
#' Volumes and prices of contracted reserves.
#' 100 documents limit applies; paging is handled transparently.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param market_agreement_type Market agreement type code (mandatory)
#'                              "A01" Daily
#'                              "A02" Weekly
#'                              "A03" Monthly
#'                              "A04" Yearly
#'                              "A06" Long term
#'                              "A13" Hourly
#' @param process_type Optional process type code
#'                     "A46" Replacement reserve
#'                     "A47" Manual frequency restoration reserve
#'                     "A51" Automatic frequency restoration reserve
#'                     "A52" Frequency containment reserve
#' @param psr_type Optional PSR type code
#'                 "A03" Mixed
#'                 "A04" Generation
#'                 "A05" Load
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::contracted_reserves(
#'   eic = "10YCZ-CEPS-----N",
#'   market_agreement_type = "A01",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
contracted_reserves <- function(
  eic = NULL,
  market_agreement_type = NULL,
  process_type = NULL,
  psr_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(
    x = market_agreement_type,
    choices = c("A01", "A02", "A03", "A04", "A06", "A13")
  )
  assert_choice(
    x = process_type,
    choices = c("A46", "A47", "A51", "A52"),
    null.ok = TRUE
  )
  assert_choice(
    x = psr_type,
    choices = c("A03", "A04", "A05"),
    null.ok = TRUE
  )
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A81",
      "&businessType=B95",
      "&type_MarketAgreement.Type=", market_agreement_type,
      if (!is.null(process_type)) paste0("&processType=", process_type),
      if (!is.null(psr_type)) paste0("&psrType=", psr_type),
      "&controlArea_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Activated Balancing Prices (TR 17.1.F, IF aFRR 3.16)
#'
#' @description
#' Prices of activated balancing energy and aFRR cross-border marginal prices.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param process_type Process type code, defaults to "A16"
#' @param business_type Optional business type code
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::activated_balancing_prices(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
activated_balancing_prices <- function(
  eic = NULL,
  process_type = "A16",
  business_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
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
      "documentType=A84",
      "&processType=", process_type,
      "&controlArea_Domain=", eic,
      if (!is.null(business_type)) paste0("&businessType=", business_type),
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Imbalance Prices (17.1.G)
#'
#' @description
#' Imbalance prices of the control area.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::imbalance_prices(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
imbalance_prices <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
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
      "documentType=A85",
      "&controlArea_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Imbalance Volumes (17.1.H)
#'
#' @description
#' Total imbalance volumes of the control area.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::imbalance_volumes(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
imbalance_volumes <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
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
      "documentType=A86",
      "&controlArea_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Financial Expenses and Income for Balancing (17.1.I)
#'
#' @description
#' Financial expenses and income for balancing of the control area.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::financial_expenses_and_income(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2022-02-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-03-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
financial_expenses_and_income <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
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
      "documentType=A87",
      "&controlArea_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get FCR Total Capacity (SO GL 187.2)
#'
#' @description
#' Total Frequency Containment Reserve capacity.
#'
#' @param eic Energy Identification Code of the area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::fcr_total_capacity(
#'   eic = "10YEU-CONT-SYNC0",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-12-31", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
fcr_total_capacity <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
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
      "documentType=A26",
      "&businessType=A25",
      "&area_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Shares of FCR Capacity (SO GL 187.2)
#'
#' @description
#' Shares of Frequency Containment Reserve capacity per area.
#'
#' @param eic Energy Identification Code of the area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::shares_of_fcr_capacity(
#'   eic = "10YCB-GERMANY--8",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-12-31", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
shares_of_fcr_capacity <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
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
      "documentType=A26",
      "&businessType=C23",
      "&area_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get RR and FRR Actual Capacity (SO GL 188.4 & 189.3)
#'
#' @description
#' Actual capacity of Frequency Restoration Reserve (FRR) or
#' Replacement Reserve (RR).
#'
#' @param eic Energy Identification Code of the area domain
#' @param process_type type of reserve
#'                     "A56" FRR
#'                     "A46" RR
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df1 <- entsoeapi::rr_and_frr_actual_capacity(
#'   eic = "10YFR-RTE------C",
#'   process_type = "A46",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-04-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df1)
#'
#' df2 <- entsoeapi::rr_and_frr_actual_capacity(
#'   eic = "10YFR-RTE------C",
#'   process_type = "A56",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-04-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df2)
#'
rr_and_frr_actual_capacity <- function(
  eic = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(x = process_type, choices = c("A56", "A46"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A26",
      "&processType=", process_type,
      "&businessType=C78",
      "&Area_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Sharing of RR and FRR Capacity (SO GL 190.1)
#'
#' @description
#' Sharing of Replacement Reserve and Frequency Restoration Reserve capacity
#' in the same Synchronous Area.
#'
#' @param eic_acquiring Energy Identification Code of the acquiring domain
#' @param eic_connecting Energy Identification Code of the connecting domain
#' @param process_type type of reserve
#'                     "A56" FRR
#'                     "A46" RR
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family balancing endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom checkmate assert_choice
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' \dontrun{
#' df1 <- entsoeapi::sharing_of_rr_and_frr_capacity(
#'   eic_acquiring = "10YCB-GERMANY--8",
#'   eic_connecting = "10YAT-APG------L",
#'   process_type = "A46",
#'   period_start = lubridate::ymd(x = "2025-10-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2025-12-31", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df1)
#'
#' df2 <- entsoeapi::sharing_of_rr_and_frr_capacity(
#'   eic_acquiring = "10YCB-GERMANY--8",
#'   eic_connecting = "10YAT-APG------L",
#'   process_type = "A56",
#'   period_start = lubridate::ymd(x = "2025-10-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2025-12-31", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' dplyr::glimpse(df2)
#' }
#'
sharing_of_rr_and_frr_capacity <- function(
  eic_acquiring = NULL,
  eic_connecting = NULL,
  process_type = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic_acquiring, var_name = "eic_acquiring")
  assert_eic(eic = eic_connecting, var_name = "eic_connecting")
  assert_choice(x = process_type, choices = c("A56", "A46"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A26",
      "&BusinessType=C22",
      "&processType=", process_type,
      "&Acquiring_Domain=", eic_acquiring,
      "&Connecting_Domain=", eic_connecting,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}
