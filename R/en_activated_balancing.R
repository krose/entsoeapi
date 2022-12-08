#' Get Accepted Offers and Activated Balancing Reserves (4.6.11.) from Entso-e
#'
#' @param eic Energy Identification Code of the control area domain
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param reserve_type Defaults to NULL, otherwise list of generation type codes (A95, A96, A97, A98) from StandardBusinessTypeList table
#' @param tidy_output Defaults to TRUE. flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' de_lu_bal_202002 <- en_activated_balancing_reserves(eic          = "10Y1001A1001A82H",
#'                                                     period_start = lubridate::ymd("2020-02-01", tz = "CET"),
#'                                                     period_end   = lubridate::ymd("2020-03-01", tz = "CET"))
#'
en_activated_balancing_reserves <- function(eic,
                                            period_start         = lubridate::ymd(Sys.Date() - 1L, tz = "CET"),
                                            period_end           = lubridate::ymd(Sys.Date(), tz = "CET"),
                                            reserve_type         = NULL,
                                            tidy_output          = TRUE,
                                            security_token       = Sys.getenv("ENTSOE_PAT")){

  ## checking if only one eic provided
  if (!exists("eic")) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")

  ## checking if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  ## converting timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

  ## checking if target period not longer than 1 year
  period_range <- difftime(time1 = strptime(x = period_end, format = "%Y%m%d%H%M", tz = "UTC") %>% as.POSIXct(tz = "UTC"),
                           time2 = strptime(x = period_start, format = "%Y%m%d%H%M", tz = "UTC") %>% as.POSIXct(tz = "UTC"),
                           units = "days")
  if (period_range > 366L) stop("One year range limit should be applied!")

  ## composing GET request url for a (maximum) 1 year long period
  URL      <- en_activated_balancing_reserves_api_req_helper(eic            = eic,
                                                             period_start   = period_start,
                                                             period_end     = period_end,
                                                             business_type  = reserve_type,
                                                             security_token = security_token)

  ## send GET request and extract result content and removing null contents
  en_cont  <- unname( URL ) %>%
    purrr::map(api_req_safe) %>%
    purrr::map("result") %>%
    purrr::compact()

  ## if valid contents left
  if (length(en_cont) > 0L) {

    ## composing a result table
    ec       <- en_cont %>%
      purrr::map(xml2::as_list) %>%
      purrr::map("Balancing_MarketDocument")
    main     <- purrr::map( ec,
                            ~{ lst   <- unlist(x = .x, recursive = FALSE)
                               tibble::tibble(document.mRID             = unlist(lst[names(lst) == "mRID"],
                                                                                 use.names = FALSE),
                                              revisionNumber            = unlist(lst[names(lst) == "revisionNumber"],
                                                                                 use.names = FALSE),
                                              process.processType       = unlist(lst[names(lst) == "process.processType"],
                                                                                 use.names = FALSE),
                                              controlArea_Domain.mRID   = unlist(lst[names(lst) == "controlArea_Domain.mRID"],
                                                                                 use.names = FALSE),
                                              createdDateTime           = unlist(lst[names(lst) == "createdDateTime"],
                                                                                 use.names = FALSE),
                                              period.timeInterval.start = unlist(lst[names(lst) == "period.timeInterval.start"],
                                                                                 use.names = FALSE),
                                              period.timeInterval.end   = unlist(lst[names(lst) == "period.timeInterval.end"],
                                                                                 use.names = FALSE)) })
    ts       <- purrr::map( ec,
                            ~purrr::map_dfr(.x[names(.x) == "TimeSeries"],
                                            ts_act_bal_res_helper,
                                            tidy_output = tidy_output))
    periodly <- purrr::map_dfr(seq_along(ec),
                               ~dplyr::bind_cols(main[[.x]], ts[[.x]]))

    ## if the output should be tidy
    if (tidy_output & nrow(periodly) > 0) {

      ## remove not necessary columns
      periodly <- periodly %>%
        dplyr::select(-period.timeInterval.start,
                      -period.timeInterval.end)

      ## renaming columns
      names(periodly) <- names(periodly) %>%
        gsub(pattern = ".mRID", replacement = "_mrid", fixed = TRUE) %>%
        gsub(pattern = "Type", replacement = "_type", fixed = TRUE) %>%
        gsub(pattern = "revisionNumber", replacement = "revision_number", fixed = TRUE) %>%
        gsub(pattern = "createdDateTime", replacement = "dt_created", fixed = TRUE) %>%
        gsub(pattern = "flowDirection", replacement = "flow_direction", fixed = TRUE) %>%
        gsub(pattern = "TimeSeries_mrid", replacement = "ts_mrid", fixed = TRUE) %>%
        gsub(pattern = "TimeSeries.", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "quantity_Measure_Unit.name", replacement = "quantity_measure_unit", fixed = TRUE) %>%
        gsub(pattern = "process.", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "Period.", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "timeInterval.", replacement = "dt_", fixed = TRUE) %>%
        gsub(pattern = "StartDateTime", replacement = "start", fixed = TRUE) %>%
        tolower()

      ## converting timestamp-like columns to timestamps
      ## adding definitions to codes
      ## and reordering columns
      periodly <- periodly %>%
        purrr::modify_at(c("dt_created", "dt_start", "dt_end"),
                         ~as.POSIXct(x  = .x,
                                     tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                    "%Y-%m-%dT%H:%M:%SZ"),
                                     tz = "UTC")) %>%
        merge(y     = StandardProcessTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(process_type     = CODE,
                              process_type_def = DEFINITION),
              by    = "process_type",
              all.x = TRUE) %>%
        merge(y     = StandardBusinessTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(business_type     = CODE,
                              business_type_def = DEFINITION),
              by    = "business_type",
              all.x = TRUE) %>%
        merge(y     = StandardDirectionTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(flow_direction     = CODE,
                              flow_direction_def = DEFINITION),
              by    = "flow_direction",
              all.x = TRUE) %>%
        merge(y     = StandardCurveTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(curve_type     = CODE,
                              curve_type_def = DEFINITION),
              by    = "curve_type",
              all.x = TRUE) %>%
        dplyr::select(base::intersect(x = c("process_type", "process_type_def",
                                            "curve_type", "curve_type_def",
                                            "flow_direction", "flow_direction_def",
                                            "business_type", "business_type_def",
                                            "document_mrid", "controlarea_domain_mrid",
                                            "revision_number",
                                            "ts_mrid", "dt_created",
                                            "dt_start", "dt_end",
                                            "resolution", "position",
                                            "start", "quantity", "quantity_measure_unit"),
                                      y = names(.))) %>%
        dplyr::arrange(dt_created, dt_start, start)

    }

    ## returning with the periodly table
    return(tibble::as_tibble(periodly))

  } else {

    ## returning with an empty table
    return(tibble::tibble())

  }
}

en_activated_balancing_reserves_api_req_helper <- function(eic,
                                                           period_start,
                                                           period_end,
                                                           business_type = NULL,
                                                           security_token){

  ## composing url(s)
  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A83",
    "&controlArea_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    { if(!is.null(business_type)) paste0("&businessType=", business_type) else "" },
    "&securityToken=", security_token
    )

  return(url)
}

ts_act_bal_res_helper <- function(ts, tidy_output = FALSE){

  ## extracting content of "Period" subbranch
  points        <- ts$Period[names(ts$Period) == "Point"] %>%
                     purrr::map_df(unlist) %>%
                     purrr::map_df(as.numeric) %>%
                     tibble::add_column(StartDateTime = dt_helper(tz_start      = unlist(ts$Period$timeInterval$start,
                                                                                         use.names = FALSE),
                                                                  tz_resolution = unlist(ts$Period$resolution,
                                                                                         use.names = FALSE),
                                                                  tz_position   = .$position),
                                        resolution    = unlist(ts$Period[names(ts$Period) == "resolution"],
                                                               use.names = FALSE))
  points        <- purrr::map_df(ts$Period[names(ts$Period) == "timeInterval"],
                                 unlist) %>%
                     dplyr::rename_with(~paste0("timeInterval.", .x)) %>%
                     dplyr::bind_cols(points) %>%
                     dplyr::rename_with(~paste0("TimeSeries.Period.", .x))

  ## extracting content of "businessType" subbranch
  businesstype  <- tibble::tibble(businessType = unlist(ts$businessType)) %>%
    dplyr::rename_with(~paste0("TimeSeries.", .x))

  ## extracting content of "flowDirection.direction" subbranch
  flowdirection <- tibble::tibble(flowDirection = unlist(ts$flowDirection.direction)) %>%
    dplyr::rename_with(~paste0("TimeSeries.", .x))

  ## removing "Period", "businessType" and "flowDirection.direction" subbranches
  ts$Period                  <- NULL
  ts$businessType            <- NULL
  ts$flowDirection.direction <- NULL

  ## converting to table the remaining parts
  ts            <- ts %>%
                     purrr::map_df(unlist) %>%
                     dplyr::rename_with(~paste0("TimeSeries.", .x))

  ## column-wise appending the parts
  if (tidy_output) {
    dplyr::bind_cols(ts, businesstype, flowdirection, points) %>%
      return()
  } else {
    dplyr::bind_cols(ts, businesstype, flowdirection) %>%
      tibble::add_column(periods = list(points)) %>%
      return()
  }

}
