utils::globalVariables(
  c(
    "url_posixct_format",
    "api_req_safe",
    "extract_response"
  )
)



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
#' @export
#'
#' @examples
#' df <- entsoeapi::elastic_demands(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A47",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-11-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
elastic_demands <- function(
  eic = NULL,
  process_type = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if proper process_type provided
  if (is.null(process_type) || !process_type %in% c("A47", "A51")) {
    stop("The 'process_type' should be 'A47' or 'A51'.")
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

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A37",
    "&processType=", process_type,
    "&businessType=B75",
    "&Acquiring_domain=", eic,
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
#' @export
#'
#' @examples
#' df <- entsoeapi::netted_volumes(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-08-17", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
netted_volumes <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # check if the requested period is not longer than one day
  if (difftime(period_end, period_start, units = "day") > 1L) {
    stop("One day range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=B17",
    "&processType=A63",
    "&Acquiring_domain=", eic,
    "&Connecting_Domain=", eic,
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
#' @export
#'
#' @examples
#' df1 <- entsoeapi::exchanged_volumes(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-08-17", tz = "CET"),
#'   tidy_output = TRUE
#' )
#' str(df1)
#'
#' df2 <- entsoeapi::exchanged_volumes(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A60",
#'   period_start = lubridate::ymd(x = "2024-07-11", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-07-12", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df2)
#'
exchanged_volumes <- function(
  eic = NULL,
  process_type = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {

  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if proper process_type provided
  if (is.null(process_type) || !process_type %in% c("A51", "A60", "A61")) {
    stop("The 'process_type' should be 'A51', 'A60' or 'A61'.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # check if the requested period is not longer than one day
  if (difftime(period_end, period_start, units = "day") > 1L) {
    stop("One day range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=B17",
    "&processType=", process_type,
    "&Acquiring_domain=", eic,
    "&Connecting_Domain=", eic,
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
#' @export
#'
#' @examples
#' df <- entsoeapi::balancing_border_cap_limit(
#'   eic_in = "10YDE-RWENET---I",
#'   eic_out = "10YBE----------2",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2022-06-22", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-06-23", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
balancing_border_cap_limit <- function(
  eic_in = NULL,
  eic_out = NULL,
  process_type = "A51",
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one in and only one out eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1L || length(eic_out) > 1L) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }

  # check if proper process_type provided
  if (!process_type %in% c("A47", "A51", "A63")) {
    stop("The 'process_type' should be 'A47', 'A51' or 'A63'.")
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

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A31",
    "&BusinessType=A26",
    "&processType=", process_type,
    "&In_Domain=", eic_in,
    "&Out_Domain=", eic_out,
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
#' @export
#'
#' @examples
#' df <- entsoeapi::exchanged_volumes_per_border(
#'   acquiring_eic = "10YBE----------2",
#'   connecting_eic = "10YFR-RTE------C",
#'   process_type = "A60",
#'   period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
exchanged_volumes_per_border <- function(
  acquiring_eic = NULL,
  connecting_eic = NULL,
  process_type = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one acquiring_eic provided
  if (is.null(acquiring_eic)) {
    stop("One acquiring EIC should be provided.")
  }
  if (length(acquiring_eic) > 1L) {
    stop("This wrapper only supports one acquiring EIC per request.")
  }

  # check if only one connecting_eic provided
  if (is.null(connecting_eic)) {
    stop("One connecting EIC should be provided.")
  }
  if (length(connecting_eic) > 1L) {
    stop("This wrapper only supports one connecting EIC per request.")
  }

  # check if proper process_type provided
  if (is.null(process_type) || !process_type %in% c("A51", "A60", "A61")) {
    stop("The 'process_type' should be 'A51', 'A60' or 'A61'.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # check if the requested period is not longer than one day
  if (difftime(period_end, period_start, units = "day") > 1L) {
    stop("One day range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A30",
    "&processType=", process_type,
    "&Acquiring_domain=", acquiring_eic,
    "&Connecting_Domain=", connecting_eic,
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
#' @export
#'
#' @examples
#' df <- entsoeapi::hvdc_link_constrains(
#'   eic_in = "10YAT-APG------L",
#'   eic_out = "10YDE-RWENET---I",
#'   process_type = "A63",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
hvdc_link_constrains <- function(
  eic_in = NULL,
  eic_out = NULL,
  eic_ic = NULL,
  process_type = "A63",
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_in)) stop("One 'in' domain EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' domain EIC should be provided.")
  if (length(eic_in) > 1L || length(eic_out) > 1L) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }
  if (length(eic_ic) > 1L) {
    stop("This wrapper only supports one interconnector EIC per request.")
  }

  # check if proper process_type provided
  if (!process_type %in% c("A47", "A51", "A63")) {
    stop("The 'process_type' should be 'A47', 'A51' or 'A63'.")
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

  # compose GET request url
  query_string <- paste0(
    "documentType=A99",
    "&processType=", process_type,
    "&BusinessType=B06",
    "&In_Domain=", eic_in,
    "&Out_Domain=", eic_out,
    if (is.null(eic_ic)) "" else "&registeredResource=", eic_ic,
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
#' @export
#'
#' @examples
#' df <- entsoeapi::changes_to_bid_availability(
#'   eic = "10YCZ-CEPS-----N",
#'   business_type = "C46",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
changes_to_bid_availability <- function(
  eic = NULL,
  business_type = "C46",
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if proper business_type provided
  if (is.null(business_type) || !business_type %in% c(
    "C40", "C41", "C42", "C43", "C44", "C45", "C46"
  )
  ) {
    stop(
      "The 'business_type' should be 'C40', 'C41', ",
      "'C42', 'C43', 'C44', 'C45', 'C46'."
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

  # compose GET request url
  query_string <- paste0(
    "documentType=B45",
    "&processType=A47",
    "&businessType=", business_type,
    "&ControlArea_Domain=", eic,
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
#' @export
#'
#' @examples
#' df <- entsoeapi::current_balancing_state(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
current_balancing_state <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # check if the requested period is not longer than 100 days
  if (difftime(period_end, period_start, units = "day") > 100L) {
    stop("100 day range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A86",
    "&businessType=B33",
    "&Area_Domain=", eic,
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
#' @export
#'
#' @examples
#' df <- entsoeapi::balancing_energy_bids(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A47",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
balancing_energy_bids <- function(
  eic = NULL,
  process_type = "A47",
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One connecting domain EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one connecting domain EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A37",
    "&processType=", process_type,
    "&businessType=B74",
    "&Connecting_Domain=", eic,
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
#' Get Aggregated Balancing Energy Bids (GL EB 12.3.E)
#'
#' @description
#' Aggregated balancing energy bids.
#' One year range limit applies.
#'
#' @param eic Energy Identification Code of the area domain
#' @param process_type type of frequency restoration reserve
#'                     "A51" aFRR
#'                     "A46" RR
#'                     "A47" mFRR
#'                     "A60" mFRR with scheduled activation
#'                     "A61" mFRR with direct activation
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::aggregated_balancing_energy_bids(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
aggregated_balancing_energy_bids <- function(
  eic = NULL,
  process_type = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if proper process_type provided
  if (
    is.null(process_type) ||
      !process_type %in% c("A51", "A46", "A47", "A60", "A61")
  ) {
    stop("The 'process_type' should be 'A51', 'A46', 'A47', 'A60' or 'A61'.")
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

  # compose GET request url
  query_string <- paste0(
    "documentType=A24",
    "&processType=", process_type,
    "&area_Domain=", eic,
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
#' @param type_market_agreement Optional market agreement type code
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::procured_balancing_capacity(
#'   eic = "10YCZ-CEPS-----N",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
procured_balancing_capacity <- function(
  eic = NULL,
  process_type = NULL,
  type_market_agreement = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if proper process_type provided
  if (is.null(process_type) || !process_type %in% c("A51", "A52", "A47")) {
    stop("The 'process_type' should be 'A51', 'A52' or 'A47'.")
  }

  # check if none or only one type_market_agreement provided
  if (!is.null(type_market_agreement) && length(type_market_agreement) > 1L) {
    stop("None or one 'type_market_agreement' should be provided.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A15",
    "&processType=", process_type,
    "&area_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )

  # if type_market_agreement is provided, add it to the query string
  if (!is.null(type_market_agreement)) {
    query_string <- paste0(
      query_string,
      "&type_MarketAgreement.Type=", type_market_agreement
    )
  }

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  extract_response(content = en_cont_list, tidy_output = tidy_output)
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
#' @param type_market_agreement Optional market agreement type code
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::allocation_of_cross_zonal_balancing_cap(
#'   eic_acquiring = "10YAT-APG------L",
#'   eic_connecting = "10YCH-SWISSGRIDZ",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
allocation_of_cross_zonal_balancing_cap <- function(
  eic_acquiring = NULL,
  eic_connecting = NULL,
  type_market_agreement = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic_acquiring)) {
    stop("One acquiring domain EIC should be provided.")
  }
  if (is.null(eic_connecting)) {
    stop("One connecting domain EIC should be provided.")
  }
  if (length(eic_acquiring) > 1L || length(eic_connecting) > 1L) {
    stop(
      "This wrapper only supports one acquiring and one connecting EIC ",
      "per request."
    )
  }

  # check if none or only one type_market_agreement provided
  if (!is.null(type_market_agreement) && length(type_market_agreement) > 1L) {
    stop("None or one 'type_market_agreement' should be provided.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A38",
    "&processType=A46",
    "&Acquiring_Domain=", eic_acquiring,
    "&Connecting_Domain=", eic_connecting,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )

  # if type_market_agreement is provided, add it to the query string
  if (!is.null(type_market_agreement)) {
    query_string <- paste0(
      query_string,
      "&type_MarketAgreement.Type=", type_market_agreement
    )
  }

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  extract_response(content = en_cont_list, tidy_output = tidy_output)
}



#' @title
#' Get Contracted Reserves (17.1.B&C)
#'
#' @description
#' Volumes and prices of contracted reserves.
#' 100 documents limit applies; paging is handled transparently.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param type_market_agreement Market agreement type code (mandatory)
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
#' @export
#'
#' @examples
#' df <- entsoeapi::contracted_reserves(
#'   eic = "10YCZ-CEPS-----N",
#'   type_market_agreement = "A13",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
contracted_reserves <- function(
  eic = NULL,
  type_market_agreement = NULL,
  process_type = NULL,
  psr_type = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if type_market_agreement is provided
  if (is.null(type_market_agreement)) {
    stop("A 'type_market_agreement' value should be provided.")
  }
  if (length(type_market_agreement) > 1L) {
    stop("Only one 'type_market_agreement' value should be provided.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A81",
    "&businessType=B95",
    "&type_MarketAgreement.Type=", type_market_agreement,
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )

  # add optional parameters if provided
  if (!is.null(process_type)) {
    query_string <- paste0(query_string, "&processType=", process_type)
  }
  if (!is.null(psr_type)) {
    query_string <- paste0(query_string, "&psrType=", psr_type)
  }

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  extract_response(content = en_cont_list, tidy_output = tidy_output)
}



#' @title
#' Get Activated Balancing Prices (TR 17.1.F, IF aFRR 3.16)
#'
#' @description
#' Prices of activated balancing energy and aFRR cross-border marginal prices.
#' One year range limit applies.
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
#' @export
#'
#' @examples
#' df <- entsoeapi::activated_balancing_prices(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
activated_balancing_prices <- function(
  eic = NULL,
  process_type = "A16",
  business_type = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
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

  # compose GET request url
  query_string <- paste0(
    "documentType=A84",
    "&processType=", process_type,
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )

  # add optional business_type if provided
  if (!is.null(business_type)) {
    query_string <- paste0(query_string, "&businessType=", business_type)
  }

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  extract_response(content = en_cont_list, tidy_output = tidy_output)
}



#' @title
#' Get Imbalance Prices (17.1.G)
#'
#' @description
#' Imbalance prices of the control area.
#' One year range limit applies.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::imbalance_prices(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
imbalance_prices <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
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

  # compose GET request url
  query_string <- paste0(
    "documentType=A85",
    "&controlArea_Domain=", eic,
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
#' Get Imbalance Volumes (17.1.H)
#'
#' @description
#' Total imbalance volumes of the control area.
#' One year range limit applies.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::imbalance_volumes(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
imbalance_volumes <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
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

  # compose GET request url
  query_string <- paste0(
    "documentType=A86",
    "&controlArea_Domain=", eic,
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
#' Get Financial Expenses and Income for Balancing (17.1.I)
#'
#' @description
#' Financial expenses and income for balancing of the control area.
#' One year range limit applies.
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::financial_expenses_and_income_for_balancing(
#'   eic = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
financial_expenses_and_income_for_balancing <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
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

  # compose GET request url
  query_string <- paste0(
    "documentType=A87",
    "&controlArea_Domain=", eic,
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
#' Get FCR Total Capacity (SO GL 187.2)
#'
#' @description
#' Total Frequency Containment Reserve capacity.
#' One year range limit applies.
#'
#' @param eic Energy Identification Code of the area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::fcr_total_capacity(
#'   eic = "10YEU-CONT-SYNC0",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
fcr_total_capacity <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one area EIC per request.")
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

  # compose GET request url
  query_string <- paste0(
    "documentType=A26",
    "&businessType=A25",
    "&area_Domain=", eic,
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
#' Get Shares of FCR Capacity (SO GL 187.2)
#'
#' @description
#' Shares of Frequency Containment Reserve capacity per area.
#' One year range limit applies.
#'
#' @param eic Energy Identification Code of the area domain
#' @param business_type type of FCR capacity share
#'                      "C23" share of FCR capacity
#'                      "B95" contracted FCR capacity
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::shares_of_fcr_capacity(
#'   eic = "10YDE-VE-------2",
#'   business_type = "C23",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
shares_of_fcr_capacity <- function(
  eic = NULL,
  business_type = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one area EIC per request.")
  }

  # check if proper business_type provided
  if (is.null(business_type) || !business_type %in% c("C23", "B95")) {
    stop("The 'business_type' should be 'C23' or 'B95'.")
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

  # compose GET request url
  query_string <- paste0(
    "documentType=A26",
    "&businessType=", business_type,
    "&area_Domain=", eic,
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
#' @param business_type type of aggregation
#'                      "C77" minimum
#'                      "C78" average
#'                      "C79" max
#'                      defaults to "C78"
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::rr_and_frr_actual_capacity(
#'   eic = "10YAT-APG------L",
#'   process_type = "A56",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
rr_and_frr_actual_capacity <- function(
  eic = NULL,
  process_type = NULL,
  business_type = "C78",
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one area EIC per request.")
  }

  # check if proper process_type provided
  if (is.null(process_type) || !process_type %in% c("A56", "A46")) {
    stop("The 'process_type' should be 'A56' (FRR) or 'A46' (RR).")
  }

  # check if proper process_type provided
  if (is.null(business_type) || !business_type %in% c("C77", "C78", "C79")) {
    stop("The 'business_type' should be 'C77', 'C78' or 'C79'.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  query_string <- paste0(
    "documentType=A26",
    "&processType=", process_type,
    "&businessType=", business_type,
    "&Area_Domain=", eic,
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
#' Get RR Actual Capacity (SO GL 189.3)
#'
#' @description
#' Actual capacity of Replacement Reserve (RR).
#' Process type is hardcoded to A46 (Replacement Reserve).
#'
#' @param eic Energy Identification Code of the area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::rr_actual_capacity(
#'   eic = "10YAT-APG------L",
#'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
#'   tidy_output = TRUE
#' )
#'
#' str(df)
#'
rr_actual_capacity <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(
    Sys.Date(),
    tz = "CET"
  ),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one area EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url (processType=A46 hardcoded for RR)
  query_string <- paste0(
    "documentType=A26",
    "&processType=A46",
    "&businessType=C77",
    "&area_Domain=", eic,
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



#' #' @title
#' #' Get Sharing of FRR Capacity (SO GL 190.1)
#' #'
#' #' @description
#' #' Sharing of Frequency Restoration Reserve capacity between areas.
#' #' One year range limit applies.
#' #'
#' #' @param eic_acquiring Energy Identification Code of the acquiring domain
#' #' @param eic_connecting Energy Identification Code of the connecting domain
#' #' @param process_type type of reserve
#' #'                     "A56" FRR
#' #'                     "A46" RR
#' #' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' #'                     One year range limit applies
#' #' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' #'                   One year range limit applies
#' #' @param tidy_output Defaults to TRUE. flatten nested tables
#' #' @param security_token Security token for ENTSO-E transparency platform
#' #'
#' #' export
#' #'
#' #' @examples
#' #' df <- entsoeapi::sharing_of_frr_capacity(
#' #'   eic_acquiring = "10YCB-GERMANY--8",
#' #'   eic_connecting = "10YAT-APG------L",
#' #'   process_type = "A56",
#' #'   period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#' #'   period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#' #'   tidy_output = TRUE
#' #' )
#' #'
#' #' str(df)
#' #'
#' sharing_of_frr_capacity <- function(
#'   eic_acquiring = NULL,
#'   eic_connecting = NULL,
#'   process_type = NULL,
#'   period_start = lubridate::ymd(
#'     Sys.Date() - lubridate::days(x = 7L),
#'     tz = "CET"
#'   ),
#'   period_end = lubridate::ymd(
#'     Sys.Date(),
#'     tz = "CET"
#'   ),
#'   tidy_output = TRUE,
#'   security_token = Sys.getenv("ENTSOE_PAT")
#' ) {
#'   # check if only one eic provided
#'   if (is.null(eic_acquiring)) {
#'     stop("One acquiring domain EIC should be provided.")
#'   }
#'   if (is.null(eic_connecting)) {
#'     stop("One connecting domain EIC should be provided.")
#'   }
#'   if (length(eic_acquiring) > 1L || length(eic_connecting) > 1L) {
#'     stop(
#'       "This wrapper only supports one acquiring and one connecting EIC ",
#'       "per request."
#'     )
#'   }
#'
#'   # check if proper process_type provided
#'   if (is.null(process_type) || !process_type %in% c("A56", "A46")) {
#'     stop("The 'process_type' should be 'A56' (FRR) or 'A46' (RR).")
#'   }
#'
#'   # check if valid security token is provided
#'   if (security_token == "") stop("Valid security token should be provided.")
#'
#'   # check if the requested period is not longer than one year
#'   if (difftime(period_end, period_start, units = "day") > 365L) {
#'     stop("One year range limit should be applied!")
#'   }
#'
#'   # convert timestamps into accepted format
#'   period_start <- url_posixct_format(period_start)
#'   period_end <- url_posixct_format(period_end)
#'
#'   # compose GET request url
#'   query_string <- paste0(
#'     "documentType=A26",
#'     "&BusinessType=C22",
#'     "&processType=", process_type,
#'     "&Acquiring_Domain=", eic_acquiring,
#'     "&Connecting_Domain=", eic_connecting,
#'     "&periodStart=", period_start,
#'     "&periodEnd=", period_end
#'   )
#'
#'   # send GET request
#'   en_cont_list <- api_req_safe(
#'     query_string = query_string,
#'     security_token = security_token
#'   )
#'
#'   # return with the extracted the response
#'   extract_response(content = en_cont_list, tidy_output = tidy_output)
#' }
