utils::globalVariables(
  c(
    "url_posixct_format",
    "api_req_safe",
    "extract_response"
  )
)



#' @title
#' Get Elastic Demands (IF mFRR 3.4)
#'
#' @description
#' Elastic demands for scheduled activation of standard mFRR product.
#'
#' @param eic Energy Identification Code of the scheduling area
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
#'   eic          = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2024-11-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#' str(df)
#'
elastic_demands <- function(
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

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A37",
    "&processType=A47",
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
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Netted Volumes (IFs IN 3.10)
#'
#' @description
#' The net position informs whether the given area imports or exports energy.
#' Those rows which hold the queried eic value in the
#' 'ts_connecting_domain_mrid' column
#' show the export value.
#' Those rows which hold the queried eic value in the
#' 'ts_acquiring_domain_mrid' column
#' show the import value.
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
#'   eic          = "10YCZ-CEPS-----N",
#'   period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2022-08-17", tz = "CET"),
#'   tidy_output  = TRUE
#' )
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

  # check if the requested period is not longer than one day
  if (difftime(period_end, period_start, units = "day") > 1) {
    stop("One day range limit should be applied!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

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
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
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
#'        A51 aFRR
#'        A60 mFRR scheduled activation
#'        A61 mFRR direct activation
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
#'   eic          = "10YCZ-CEPS-----N",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2022-08-17", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#' str(df1)
#'
#' df2 <- entsoeapi::exchanged_volumes(
#'   eic          = "10YCZ-CEPS-----N",
#'   process_type = "A60",
#'   period_start = lubridate::ymd(x = "2024-07-11", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2024-07-12", tz = "CET"),
#'   tidy_output  = TRUE
#' )
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
  if (!process_type %in% c("A51", "A60", "A61")) {
    stop("The 'process_type' should be 'A51', 'A60' or 'A61'.")
  }

  # check if the requested period is not longer than one day
  if (difftime(period_end, period_start, units = "day") > 1) {
    stop("One day range limit should be applied!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

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
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
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
#' @param eic_interconnector Energy Identification Code of a Transmission Asset,
#'                           optional filter
#' @param process_type type of frequency restoration reserve
#'        A47 mFRR
#'        A51 aFRR
#'        A63 Imbalance Netting
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::balancing_border_cap_limit(
#'   eic_in       = "10YDE-RWENET---I",
#'   eic_out      = "10YBE----------2",
#'   process_type = "A51",
#'   period_start = lubridate::ymd(x = "2022-06-22", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2022-06-23", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#' str(df)
#'
balancing_border_cap_limit <- function(
  eic_in = NULL,
  eic_out = NULL,
  eic_interconnector = NULL,
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
  # check if only one in and only one out eic provided
  if (is.null(eic_in)) stop("One 'in' control area EIC should be provided.")
  if (is.null(eic_out)) stop("One 'out' control area EIC should be provided.")
  if (length(eic_in) > 1 || length(eic_out) > 1) {
    stop("This wrapper only supports one in and one out EIC per request.")
  }

  # check if none or only one eic_interconnector provided
  if (!is.null(eic_interconnector) && length(eic_interconnector) > 1) {
    stop(
      "None or one Transmission Asset (eic_interconnector) ",
      "should be provided."
    )
  }

  # check if proper process_type provided
  if (!process_type %in% c("A47", "A51", "A63")) {
    stop("The 'process_type' should be 'A47', 'A51' or 'A63'.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

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

  # if eic_interconnector is provided, then add it to the query string
  if (!is.null(eic_interconnector)) {
    query_string <- paste0(
      query_string,
      "&registeredResource=", eic_interconnector
    )
  }

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



# 4.6.24. Permanent Allocation Limitations to
# Cross-border Capacity on HVDC Lines [IFs 4.5]
# Minimum time interval in query response depends on the duration
# of matching published instances.
# Mandatory parameters:
# - DocumentType
# - ProcessType
# - BusinessType
# - In_Domain
# - Out_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional Parameters:
# - registeredResource (If used, data for the given
#   Transmission Asset is returned, otherwise,
#   the data for IC not specified is returned.)
# GET /api?documentType=A99
# &processType=A51
# &BusinessType=B06
# &In_Domain=10YAT-APG------L
# &TimeInterval=2020-12-31T23:00Z/2021-01-01T23:00Z
# &Out_Domain=10YDE-RWENET---I



# 4.6.3.Changes to Bid Availability [IFs mFRR 9.9, aFRR 9.6 & 9.8]
# 100 documents limit applies
# Mandatory Parameters:
# - DocumentType
# - ProcessType
# - Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional Parameters:
# - Offset (allows downloading more than 100 documents. The offset ∈ [0,4800]
#   so that paging is restricted to query for 4900 documents max.,
#   offset=n returns files in sequence between n+1 and n+100)
# - BusinessType
# GET /api? documentType=B45
# &processtype=A47
# &Domain=10YCZ-CEPS-----N
# &periodStart=202208152200
# &periodEnd=202208162200



# 4.6.1. Current Balancing State [GL EB 12.3.A]
# 100 day range limit applies
# Mandatory Parameters:
# - DocumentType
# - BusinessType
# - Area_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A86
# &businessType=B33
# &Area_Domain=10YCZ-CEPS-----N
# &periodStart=201912190000
# &periodEnd=201912190010



# 4.6.2.Balancing Energy Bids [GL EB 12.3.B&C]
# 100 documents limit applies
# Mandatory Parameters:
# - DocumentType
# - ProcessType
# - connecting_Domain
# - businessType
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional Parameters:
# - Offset (allows downloading more than 100 documents. The offset ∈ [0,4800]
#   so that paging is restricted to query for 4900 documents max.,
#   offset=n returns files in sequence between n+1 and n+100)
# - Standard_MarketProduct
# - Original_MarketProduct
# - Direction
# GET /api? documentType=A37
# &ProcessType=A47
# &businessType=B74
# &connecting_Domain=10YCZ-CEPS-----N
# &periodStart=202208152200
# &periodEnd=202208162200



# 4.6.4. Aggregated Balancing Energy Bids [GL EB 12.3.E]
# One year range limit applies
# Mandatory Parameters:
# - DocumentType
# - ProcessType
# - Area_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api? documentType=A24
# &processType=A51
# &area_Domain=10YCZ-CEPS-----N
# &TimeInterval=2019-12-16T13:00Z/2019-12-16T18:00Z



# 4.6.5.  Procured Balancing Capacity [GL EB 12.3.F]
# 100 document limit applies
# Mandatory Parameters:
# - DocumentType
# - ProcessType
# - Area_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional Parameters:
# - Offset (allows downloading more than 100 documents.
#   The offset [0,4800] so that paging is restricted to query for
#   4900 documents max., offset=n returns files in sequence between
#   n+1 and n+100)
# - Type_MarketAgreement.Type
# GET /api? documentType=A15
# &processType=A51
# &area_Domain=10YCZ-CEPS-----N
# &TimeInterval=2019-12-31T23:00Z/2020-01-01T00:00Z



# 4.6.6. Use of Allocated Cross-Zonal Balancing Capacity [GL EB 12.3.H & I]
# Mandatory parameters:
# - DocumentType
# - ProcessType
# - Acquiring_Domain
# - Connecting_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional parameters:
# - Type_MarketAgreement.Type
# GET /api?documentType=A38
# &processType=A46
# &Acquiring_Domain=10YAT-APG------L
# &Connecting_Domain=10YCH-SWISSGRIDZ
# &TimeInterval=2019-12-16T00:00Z/2019-12-17T00:00Z



# 4.6.8. Amount of Balancing Reserves Under Contract [17.1.B]
# Minimum time interval in query response ranges from
# part of day to year, depending on selected Type_MarketAgreement.Type
# Mandatory parameters:
# - DocumentType
# - Type_MarketAgreement.Type
# - ControlArea_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional parameters:
# - BusinessType
# - PsrType
# - offset (allows downloading more than 100 documents.
#   The offset ∈ [0,4800] so that paging is restricted to query for
#   4900 documents max., offset=n returns files in sequence
#   between n+1 and n+100)
# GET /api?documentType=A81
# &type_MarketAgreement.Type=A13
# &businessType=A95
# &psrType=A04
# &controlArea_Domain=10YCZ-CEPS-----N
# &periodStart=201512312300
# &periodEnd=201601012300



# 4.6.9. Prices of Procured Balancing Reserves [17.1.C]
# Minimum time interval in query response ranges from part of day to year,
# depending on selected Type_MarketAgreement.Type
# Mandatory parameters:
# - DocumentType
# - Type_MarketAgreement.Type
# - ControlArea_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional parameters:
# - BusinessType
# - PsrType
# - offset (allows downloading more than 100 documents.
#   The offset ∈ [0,4800] so that paging is restricted to query for 4900
#   documents max., offset=n returns files in sequence between n+1 and n+100)
# GET /api?documentType=A89
# &type_MarketAgreement.Type=A01
# &businessType=A96
# &controlArea_Domain=10YCZ-CEPS-----N
# &periodStart=201512312300
# &periodEnd=201601012300



# 4.6.7. Volumes and Prices of Contracted Reserves [17.1.B & C]
# Minimum time interval in query response ranges from part of day to year,
# depending on selected Type_MarketAgreement.Type
# Mandatory parameters:
# - DocumentType
# - BusinessType
# - ProcessType
# - Type_MarketAgreement.Type
# - ControlArea_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional parameters:
# - PsrType
# - offset - (allows downloading more than 100 documents.
#   The offset must belong to the range [0,4800] so
#   that paging is restricted to query for 4900 documents max.,
#   offset=n returns files in sequence between n+1 and n+100)
# GET /api?documentType=A81
# &type_MarketAgreement.Type=A13
# &businessType=B95
# &processType=A52
# &psrType=A04
# &controlArea_Domain=10YCZ-CEPS-----N
# &periodStart=201512312300
# &periodEnd=201601012300



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
#' df <- entsoeapi::balancing_accepted_aggr_offers(
#'   eic          = "10YHU-MAVIR----U",
#'   period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
#'   tidy_output  = TRUE,
#'   reserve_type = "A96"
#' )
#' str(df)
#'
balancing_accepted_aggr_offers <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  reserve_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {

  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A82",
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    {
      if (is.null(reserve_type)) "" else paste0("&businessType=", reserve_type)
    }
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
#' df <- entsoeapi::balancing_activated_reserves(
#'   eic          = "10YHU-MAVIR----U",
#'   period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
#'   tidy_output  = TRUE,
#'   reserve_type = "A96"
#' )
#' str(df)
#'
balancing_activated_reserves <- function(
  eic = NULL,
  period_start = lubridate::ymd(
    Sys.Date() - lubridate::days(x = 7L),
    tz = "CET"
  ),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  reserve_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {

  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A83",
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    {
      if (is.null(reserve_type)) "" else paste0("&businessType=", reserve_type)
    }
  )

  # send GET request
  en_cont_list <- api_req_safe(
    query_string = query_string,
    security_token = security_token
  )

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



# 4.6.12. Prices of Activated Balancing Energy &
# aFRR CBMPs [TR 17.1.F, IF aFRR 3.16]
# One year range limit applies
# Minimum time interval in query response is one ISP period
# Mandatory parameters:
# - DocumentType
# - ControlArea_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# - ProcessType - A16 used if not provided (temporary solution)
# Optional parameters:
# - BusinessType
# - PsrType
# - Standard_MarketProduct
# - Original_MarketProduct
# - ExportType=zip (to be used only when queried data is available
#   in legacy and new technologies)
# GET /api?documentType=A84
# &processtype=A16
# &controlArea_Domain=10YCZ-CEPS-----N
# &periodStart=202208152200
# &periodEnd=202208162200



# 4.6.13. Imbalance Prices [17.1.G]
# One year range limit applies
# Minimum time interval in query response is one BTU period
# Mandatory parameters:
# - DocumentType
# - ControlArea_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# Optional parameters:
# - PsrType
# GET /api?documentType=A85
# &controlArea_Domain=10YCZ-CEPS-----N
# &periodStart=201512312300
# &periodEnd=201612312300



# 4.6.14. Total Imbalance Volumes [17.1.H]
# One year range limit applies
# Minimum time interval in query response is one BTU period
# Mandatory parameters:
# - DocumentType
# - ControlArea_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A86
# &controlArea_Domain=10YCZ-CEPS-----N
# &periodStart=201512312300
# &periodEnd=201612312300



# 4.6.15. Financial Expenses and Income for Balancing [17.1.I]
# One year range limit applies
# Minimum time interval in query response is one month
# Mandatory parameters:
# - DocumentType
# - ControlArea_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A87
# &controlArea_Domain=10YCZ-CEPS-----N
# &periodStart=201512312300
# &periodEnd=201612312300



# 4.6.16. Cross-border Balancing [17.1.J]
# One year range limit applies
# Minimum time interval in query response is one BTU period
# Mandatory parameters:
# - DocumentType
# - Acquiring_Domain
# - Connecting_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# In the query response, the attribute secondaryQuantity contains
# the Aggregated offers, quantity contains the activated offers
# and prices are available in the minimum_Price.amount and
# maximum_Price.amount attributes.
# GET /api?documentType=A88
# &acquiring_Domain=10YCZ-CEPS-----N
# &connecting_Domain=10YSK-SEPS-----K
# &periodStart=201512312300
# &periodEnd=201601010100



# 4.6.17. FCR Total capacity [SO GL 187.2]
# One year range limit applies
# Mandatory parameters:
# - DocumentType
# - BusinessType
# - Area_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A26
# &area_Domain=10YEU-CONT-SYNC0
# &businessType=A25
# &TimeInterval=2018-12-31T23:00Z/2019-12-31T23:00Z



# 4.6.18. Shares of FCR capacity - share of capacity [SO GL 187.2]
# One year range limit applies
# Mandatory parameters:
# - DocumentType
# - BusinessType
# - Area_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A26
# &area_Domain=10YDE-VE-------2
# &businessType=C23
# &TimeInterval=2019-12-31T23:00Z/2020-12-31T23:00Z



# 4.6.19. Shares of FCR capacity - contracted reserve capacity [SO GL 187.2]
# One year range limit applies
# Mandatory parameters:
# - DocumentType
# - BusinessType
# - Area_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A26
# &businessType=B95
# &TimeInterval=2019-12-31T23:00Z/2020-12-31T23:00Z
# &Area_Domain=10YDE-RWENET---I



# 4.6.20. FRR Actual Capacity [SO GL 188.4]
# Mandatory parameters:
# - DocumentType
# - ProcessType
# - BusinessType
# - Area_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A26
# &processType=A56
# &businessType=C24
# &Area_Domain=10YAT-APG------L
# &TimeInterval=2019-12-31T23:00Z/2020-03-31T22:00Z



# 4.6.21. RR Actual Capacity [SO GL 189.3]
# Mandatory parameters:
# - DocumentType
# - ProcessType
# - BusinessType
# - Area_Domain
# - TimeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A26
# &processType=A46
# &businessType=C24
# &Area_Domain=10YAT-APG------L
# &TimeInterval=2019-12-31T23:00Z/2020-03-31T22:00Z



# 4.6.22. Sharing of RR and FRR [SO GL 190.1]
# One year range limit applies
# Mandatory parameters:
# - DocumentType
# - BusinessType
# - ProcessType
# - Acquiring_Domain
# - Connecting_Domain
# T- imeInterval or combination of PeriodStart and PeriodEnd
# GET /api?documentType=A26
# &businessType=C22
# &TimeInterval=2019-12-31T23:00Z/2020-12-31T23:00Z
# &Connecting_Domain=10YAT-APG------L
# &Acquiring_Domain=10YCB-GERMANY--8
# &processType=A56
