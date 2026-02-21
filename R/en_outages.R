utils::globalVariables(
  c(
    "url_posixct_format",
    "api_req_safe",
    "extract_response"
  )
)



#' @title
#' Get Unavailability of Production & Generation Units (15.1.A&B + 15.1.C&D)
#'
#' @description
#' The planned and forced unavailability of production and
#' generation units expected to last at least one market time unit
#' up to 3 years ahead. The "available capacity during the event"
#' means the minimum available generation capacity during the period specified.
#'
#' @param eic Energy Identification Code of the bidding zone/control area
#'        (To extract outages of bidding zone DE-AT-LU area,
#'        it is recommended to send queries per control area
#'        i.e. CTA|DE(50Hertz), CTA|DE(Amprion), CTA|DE(TeneTGer),
#'        CTA|DE(TransnetBW),CTA|AT,CTA|LU but not per bidding zone.)
#' @param period_start the starting date of the in-scope period
#'                     in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end the ending date of the outage in-scope period
#'                   in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param doc_status Notification document status. "A05" for active,
#'                   "A09" for cancelled and "A13" for withdrawn.
#'                   Defaults to NULL which means "A05" and "A09" together.
#' @param event_nature "A53" for planned maintenance.
#'                      "A54" for unplanned outage.
#'                      Defaults to NULL which means both of them.
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::outages_both(
#'   eic = "10YFR-RTE------C",
#'   period_start = lubridate::ymd(
#'                    x = Sys.Date() + lubridate::days(x = 1L),
#'                    tz = "CET"
#'                  ),
#'   period_end = lubridate::ymd(
#'                  x = Sys.Date() + lubridate::days(x = 2L),
#'                  tz = "CET"
#'                )
#' )
#'
#' str(df)
#'
outages_both <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 2L),
                              tz = "CET"),
  doc_status = NULL,
  event_nature = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  tbl_gu <- try(outages_gen_units(eic = eic,
                                  period_start = period_start,
                                  period_end = period_end,
                                  doc_status = doc_status,
                                  event_nature = event_nature,
                                  tidy_output = tidy_output,
                                  security_token = security_token))
  tbl_pu <- try(outages_prod_units(eic = eic,
                                   period_start = period_start,
                                   period_end = period_end,
                                   doc_status = doc_status,
                                   event_nature = event_nature,
                                   tidy_output = tidy_output,
                                   security_token = security_token))

  if (inherits(tbl_gu, "try-error")) {
    message(
      "Get unavailability of generation units error. ",
      "Try calling the function outages_gen_units() to see the error message."
    )
    tbl_gu <- NULL
  }

  if (inherits(tbl_pu, "try-error")) {
    message(
      "Get unavailability of production units error. ",
      "Try calling the function outages_prod_units() to see the error message."
    )
    tbl_pu <- NULL
  }

  # append the results and return
  list(tbl_gu, tbl_pu) |>
    purrr::compact() |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE)
}



#' @title
#' Get Unavailability of Generation Units. (15.1.A&B)
#'
#' @description
#' The planned and forced unavailability of generation units
#' expected to last at least one market time unit up to 3 years
#' ahead.
#' The "available capacity during the event" means the minimum
#' available generation capacity during the period specified.
#'
#' @param eic Energy Identification Code of the bidding zone/
#'            control area
#'            (To extract outages of bidding zone DE-AT-LU area,
#'            it is recommended to send queries per control area
#'            i.e. CTA|DE(50Hertz), CTA|DE(Amprion),
#'            CTA|DE(TeneTGer),CTA|DE(TransnetBW), CTA|AT,CTA|LU
#'            but not per bidding zone.)
#' @param period_start the starting date of the in-scope period
#'                     in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end the ending date of the outage in-scope period
#'                   in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param doc_status Notification document status. "A05" for active,
#'                   "A09" for cancelled and "A13" for withdrawn.
#'                   Defaults to NULL which means "A05" and "A09" together.
#' @param event_nature "A53" for planned maintenance.
#'                      "A54" for unplanned outage.
#'                      Defaults to NULL which means both of them.
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::outages_gen_units(
#'   eic = "10YFR-RTE------C",
#'   period_start = lubridate::ymd(
#'                    x = Sys.Date() + lubridate::days(x = 1L),
#'                    tz = "CET"
#'                  ),
#'   period_end = lubridate::ymd(
#'                  x = Sys.Date() + lubridate::days(x = 2L),
#'                  tz = "CET"
#'                )
#' )
#'
#' str(df)
#'
outages_gen_units <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 2L),
                              tz = "CET"),
  doc_status = NULL,
  event_nature = NULL,
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

  # check if doc_status value is valid
  if (isFALSE(doc_status %in% c("A05", "A09", "A13"))) {
    stop("The 'doc_status' parameter should be 'A05', 'A09', 'A13' or NULL.")
  }

  # check if event_nature value is valid
  if (isFALSE(event_nature %in% c("A53", "A54"))) {
    stop("The 'event_nature' parameter should be 'A53', 'A54' or NULL.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A80",
    "&biddingZone_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )
  if (!is.null(doc_status)) {
    query_string <- paste0(query_string, "&docStatus=", doc_status)
  }
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



#' @title
#' Get Unavailability of Production Units. (15.1.C&D)
#'
#' @description
#' The planned and forced unavailability of production units
#' expected to last at least one market time unit up to
#' 3 years ahead.
#' The "available capacity during the event" means the minimum
#' available generation capacity during the period specified.
#'
#' @param eic Energy Identification Code of the bidding zone/
#'            control area (To extract outages of bidding zone
#'            DE-AT-LU area, it is recommended to send queries
#'            per control area i.e. CTA|DE(50Hertz), CTA|DE(Amprion),
#'            CTA|DE(TeneTGer), CTA|DE(TransnetBW),CTA|AT,CTA|LU
#'            but not per bidding zone.)
#' @param period_start the starting date of the in-scope period
#'                     in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end the ending date of the outage in-scope period
#'                   in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param doc_status Notification document status. "A05" for active,
#'                   "A09" for cancelled and "A13" for withdrawn.
#'                   Defaults to NULL which means "A05" and "A09" together.
#' @param event_nature "A53" for planned maintenance.
#'                      "A54" for unplanned outage.
#'                      Defaults to NULL which means both of them.
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::outages_prod_units(
#'   eic                 = "10YFR-RTE------C",
#'   period_start        = lubridate::ymd(x = Sys.Date() +
#'                                          lubridate::days(x = 1L),
#'                                        tz = "CET"),
#'   period_end          = lubridate::ymd(x = Sys.Date() +
#'                                          lubridate::days(x = 2L),
#'                                        tz = "CET")
#' )
#'
#' str(df)
#'
outages_prod_units <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 2L),
                              tz = "CET"),
  doc_status = NULL,
  event_nature = NULL,
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

  # check if doc_status value is valid
  if (isFALSE(doc_status %in% c("A05", "A09", "A13"))) {
    stop("The 'doc_status' parameter should be 'A05', 'A09', 'A13' or NULL.")
  }

  # check if event_nature value is valid
  if (isFALSE(event_nature %in% c("A53", "A54"))) {
    stop("The 'event_nature' parameter should be 'A53', 'A54' or NULL.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A77",
    "&biddingZone_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )
  if (!is.null(doc_status)) {
    query_string <- paste0(query_string, "&docStatus=", doc_status)
  }
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



#' @title
#' Get Unavailability of Offshore Grid Infrastructure. (10.1.C)
#'
#' @description
#' Unavailability of the off-shore grid that reduce wind power
#' feed-in during at least one market time unit.
#' Wind power fed in at the time of the change in the availability
#' is provided.
#'
#' @param eic Energy Identification Code of the bidding zone/
#'            control area (To extract outages of bidding zone
#'            DE-AT-LU area, it is recommended to send queries
#'            per control area i.e. CTA|DE(50Hertz), CTA|DE(Amprion),
#'            CTA|DE(TeneTGer), CTA|DE(TransnetBW),CTA|AT,CTA|LU
#'            but not per bidding zone.)
#' @param period_start the starting date of the in-scope period
#'                     in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end the ending date of the outage in-scope period
#'                   in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param doc_status Notification document status. NULL or "A05"
#'                   for active and "A13" for withdrawn.
#'                   Defaults to NULL.
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::outages_offshore_grid(
#'   eic = "10Y1001A1001A82H",
#'   period_start = lubridate::ymd(
#'                    x = Sys.Date() -lubridate::days(x = 365L),
#'                    tz = "CET"
#'                  ),
#'   period_end = lubridate::ymd(x = Sys.Date(), tz = "CET")
#' )
#'
#' str(df)
#'
outages_offshore_grid <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 2L),
                              tz = "CET"),
  doc_status = NULL,
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

  # check if doc_status value is valid
  if (isFALSE(doc_status %in% c("A05", "A09", "A13"))) {
    stop("The 'doc_status' parameter should be 'A05', 'A09', 'A13' or NULL.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A79",
    "&biddingZone_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )
  if (!is.null(doc_status)) {
    query_string <- paste0(query_string, "&docStatus=", doc_status)
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
#' Get Unavailability of Consumption Units. (7.1.A&B)
#'
#' @description
#' Unavailability of consumption units in aggregated form.
#' All planned and forced outages in selected area are
#' aggregated according to the market time unit. The list of
#' specific consumption units are not provided.
#'
#' @param eic Energy Identification Code of the bidding zone/
#'            control area
#'            (To extract outages of bidding zone DE-AT-LU area,
#'            it is recommended to send queries per control area
#'            i.e. CTA|DE(50Hertz), CTA|DE(Amprion), CTA|DE(TeneTGer),
#'            CTA|DE(TransnetBW),CTA|AT,CTA|LU but not per bidding zone.)
#' @param period_start the starting date of the in-scope period
#'                     in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end the ending date of the outage in-scope period
#'                   in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param period_start_update notification submission/update starting date
#'                            in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                            One year range limit applies
#' @param period_end_update notification submission/update ending date
#'                          in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                          One year range limit applies
#' @param doc_status Notification document status. "A05" for active,
#'                   "A09" for cancelled and "A13" for withdrawn.
#'                   Defaults to NULL which means "A05" and "A09" together.
#' @param event_nature "A53" for planned maintenance.
#'                      "A54" for unplanned outage.
#'                      Defaults to NULL which means both of them.
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::outages_cons_units(
#'   eic = "10YFI-1--------U",
#'   period_start = lubridate::ymd(x = "2024-04-10", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-04-11", tz = "CET")
#' )
#'
#' str(df)
#'
outages_cons_units <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 2L),
                              tz = "CET"),
  period_start_update = NULL,
  period_end_update = NULL,
  doc_status = NULL,
  event_nature = NULL,
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

  # check if doc_status value is valid
  if (isFALSE(doc_status %in% c("A05", "A09", "A13"))) {
    stop("The 'doc_status' parameter should be 'A05', 'A09', 'A13' or NULL.")
  }

  # check if event_nature value is valid
  if (isFALSE(event_nature %in% c("A53", "A54"))) {
    stop("The 'event_nature' parameter should be 'A53', 'A54' or NULL.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)
  period_start_update <- url_posixct_format(period_start_update)
  period_end_update <- url_posixct_format(period_end_update)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A76",
    "&biddingZone_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )
  if (!is.null(doc_status)) {
    query_string <- paste0(query_string, "&docStatus=", doc_status)
  }
  if (!is.null(event_nature)) {
    query_string <- paste0(query_string, "&businessType=", event_nature)
  }
  if (!is.null(period_start_update) && !is.null(period_end_update)) {
    query_string <- paste0(query_string,
                           "&periodStartUpdate=", period_start_update,
                           "&periodEndUpdate=", period_end_update)
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
#' Get Unavailability of Transmission Infrastructure. (10.1.A&B)
#'
#' @description
#' The planned and forced unavailability, including changes in
#' unavailability of interconnections in the transmission grid
#' that reduce transfer capacities between areas during at least
#' one market time unit including information about new
#' net transfer capacity.
#'
#' @param eic_in Energy Identification Code of the IN bidding zone area
#' @param eic_out Energy Identification Code of the OUT bidding zone area
#' @param period_start the starting date of the in-scope period
#'                     in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end the ending date of the outage in-scope period
#'                   in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param period_start_update notification submission/update starting date
#'                            in POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end_update notification submission/update ending date
#'                          in POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param doc_status Notification document status. "A05" for active,
#'                   "A09" for cancelled and "A13" for withdrawn.
#'                   Defaults to NULL which means "A05" and "A09" together.
#' @param event_nature "A53" for planned maintenance.
#'                     "A54" for unplanned outage.
#'                     Defaults to NULL which means both of them.
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#' df <- entsoeapi::outages_transmission_grid(
#'   eic_in = "10YFR-RTE------C",
#'   eic_out = "10Y1001A1001A82H",
#'   period_start = lubridate::ymd(
#'                    x = Sys.Date() + lubridate::days(x = 1),
#'                    tz = "CET"
#'                  ),
#'   period_end = lubridate::ymd(
#'                  x = Sys.Date() + lubridate::days(x = 2),
#'                  tz = "CET"
#'                ),
#'   period_start_update = lubridate::ymd(
#'                           x = Sys.Date() -lubridate::days(x = 7),
#'                           tz = "CET"
#'                         ),
#'   period_end_update = lubridate::ymd(x = Sys.Date(), tz = "CET")
#' )
#'
#' str(df)
#'
outages_transmission_grid <- function(
  eic_in = NULL,
  eic_out = NULL,
  period_start = lubridate::ymd(Sys.Date() + lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date() + lubridate::days(x = 3L),
                              tz = "CET"),
  period_start_update = NULL,
  period_end_update = NULL,
  doc_status = NULL,
  event_nature = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic per direction provided
  if (is.null(eic_in)) stop("One IN control area EIC should be provided.")
  if (length(eic_in) > 1L) {
    stop("This wrapper only supports one IN control area EIC per request.")
  }
  if (is.null(eic_out)) stop("One OUT control area EIC should be provided.")
  if (length(eic_out) > 1L) {
    stop("This wrapper only supports one OUT control area EIC per request.")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  # check if doc_status value is valid
  if (isFALSE(doc_status %in% c("A05", "A09", "A13"))) {
    stop("The 'doc_status' parameter should be 'A05', 'A09', 'A13' or NULL.")
  }

  # check if event_nature value is valid
  if (isFALSE(event_nature %in% c("A53", "A54"))) {
    stop("The 'event_nature' parameter should be 'A53', 'A54' or NULL.")
  }

  # check if the requested period is not longer than one year
  if (difftime(period_end, period_start, units = "day") > 365L) {
    stop("One year range limit should be applied!")
  }

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)
  period_start_update <- url_posixct_format(period_start_update)
  period_end_update <- url_posixct_format(period_end_update)

  # compose GET request url for a (maximum) 1 year long period
  query_string <- paste0(
    "documentType=A78",
    "&in_Domain=", eic_in,
    "&out_domain=", eic_out,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )
  if (!is.null(doc_status)) {
    query_string <- paste0(query_string, "&docStatus=", doc_status)
  }
  if (!is.null(event_nature)) {
    query_string <- paste0(query_string, "&businessType=", event_nature)
  }
  if (!is.null(period_start_update) && !is.null(period_end_update)) {
    query_string <- paste0(query_string,
                           "&periodStartUpdate=", period_start_update,
                           "&periodEndUpdate=", period_end_update)
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
#' Get Fall-Back Procedures. (IFs IN 7.2, mFRR 3.11, aFRR 3.10)
#'
#' @description
#' It publishes of application of fall back procedures by participants
#' in European platforms as a result of disconnection of DSO from
#' the European platform, unavailability of European platform itself
#' (planned or unplanned outage) or the situation where the algorithm
#' used on the platform fails or does not find solution.
#'
#' @param eic Energy Identification Code of the bidding zone/
#'            control area
#' @param period_start the starting date of the in-scope period
#'                     in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end the ending date of the outage in-scope period
#'                   in POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param process_type "A47" = mFRR
#'                     "A51" = aFRR
#'                     "A63" = imbalance netting
#'                     defaults to "A63"
#' @param event_nature "C47" = Disconnection,
#'                   "A53" = Planned maintenance,
#'                   "A54": Unplanned outage,
#'                   "A83" = Auction cancellation (used in case
#'                   no solution found or algorithm failure);
#'                   Defaults to "A53".
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples#'
#' df <- entsoeapi::outages_fallbacks(
#'   eic = "10YBE----------2",
#'   period_start = lubridate::ymd(x = "2023-01-01", tz = "CET"),
#'   period_end = lubridate::ymd(x = "2024-01-01", tz = "CET"),
#'   process_type = "A51",
#'   event_nature = "C47")
#'
#' str(df)
#'
outages_fallbacks <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(),
                              tz = "CET"),
  process_type = "A63",
  event_nature = "A53",
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request.")
  }

  # check if valid process_type provided
  if (!process_type %in% c("A47", "A51", "A63")) {
    stop("The 'process_type' parameter should be 'A47', 'A51' or 'A63'.")
  }

  # check if valid event_nature provided
  if (!event_nature %in% c("C47", "A53", "A54", "A83")) {
    stop("The 'event_nature' parameter should be ",
         "'C47', 'A53', 'A54' or 'A83'.")
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
    "documentType=A53",
    "&biddingZone_Domain=", eic,
    "&processType=", process_type,
    "&businessType=", event_nature,
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
