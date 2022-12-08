

#' Get Installed generation capacity aggregated per type from Entsoe
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' fr_2020 <- en_generation_inst_gen_cap_agg(eic = "10YFR-RTE------C", year=2020)
#'
en_generation_inst_gen_cap_agg <- function(eic, year, security_token = Sys.getenv("ENTSOE_PAT")){

  period_start <- lubridate::ymd(paste0(year, "-01-01"), tz = "CET")
  period_end <- lubridate::ymd(paste0(year, "-01-02"), tz = "CET")
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A68",
    "&processType=A33",
    "&in_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req(url)
  en_cont <- xml2::as_list(en_cont)
  en_cont <- generation_inst_gen_cap_agg_helper(en_cont)

}


generation_inst_gen_cap_agg_helper <- function(x){

  dt_created <- x$GL_MarketDocument$createdDateTime[[1]]

  x <- x$GL_MarketDocument[names(x$GL_MarketDocument) == "TimeSeries"]
  x <- unname(x)

  unit <- unlist(purrr::map(x, ~.x$quantity_Measure_Unit.name[[1]]))
  start <- unlist(purrr::map(x, ~.x$Period$timeInterval$start[[1]]))
  end <- unlist(purrr::map(x, ~.x$Period$timeInterval$end[[1]]))
  resolution <- unlist(purrr::map(x, ~.x$Period$resolution))
  psr_type <- unlist(purrr::map(x, ~.x$MktPSRType))
  quantity <- unlist(purrr::map(x, ~.x$Period$Point$quantity[[1]]))

  tibble::tibble(dt_created, start, end, resolution, psr_type, quantity, unit)
}



#' Get aggregated generation per type from Entsoe
#'
#' @param eic Energy Identification Code of the control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param gen_type Defaults to NULL, otherwise list of generation type codes from StandardAssetTypeList table
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
#' fr_202002 <- en_generation_agg_gen_per_type(eic = "10YFR-RTE------C",
#'                                             period_start = lubridate::ymd("2020-02-01", tz = "CET"),
#'                                             period_end = lubridate::ymd("2020-03-01", tz = "CET"))
#'
en_generation_agg_gen_per_type <- function(eic,
                                           period_start   = lubridate::ymd(Sys.Date() - 1L, tz = "CET"),
                                           period_end     = lubridate::ymd(Sys.Date(), tz = "CET"),
                                           gen_type       = NULL,
                                           tidy_output    = TRUE,
                                           security_token = Sys.getenv("ENTSOE_PAT")){

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
  URL      <- en_gen_agg_gen_pertype_api_req_helper(eic            = eic,
                                                    period_start   = period_start,
                                                    period_end     = period_end,
                                                    psr_type       = gen_type,
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
      purrr::map("GL_MarketDocument") %>%
      unlist(recursive = FALSE)
    main     <- dplyr::bind_cols(tibble::tibble(document.mRID       = unlist(ec[names(ec) == "mRID"],
                                                                             use.names = FALSE),
                                                revisionNumber      = unlist(ec[names(ec) == "revisionNumber"],
                                                                             use.names = FALSE),
                                                process.processType = unlist(ec[names(ec) == "process.processType"],
                                                                             use.names = FALSE),
                                                createdDateTime     = unlist(ec[names(ec) == "createdDateTime"],
                                                                             use.names = FALSE)),
                                 dplyr::bind_rows(unlist(ec[names(ec) == "time_Period.timeInterval"])))
    ts       <- ec[names(ec) == "TimeSeries"] %>%
      purrr::map_dfr(ts_agg_gen_helper,
                     tidy_output = tidy_output)
    periodly <- dplyr::bind_cols(main, ts)

    ## if the output should be tidy
    if (tidy_output) {

      ## remove not necessary columns
      periodly <- periodly %>%
        dplyr::select(-time_Period.timeInterval.start,
                      -time_Period.timeInterval.end)

      ## renaming columns
      names(periodly) <- names(periodly) %>%
        gsub(pattern = ".mRID", replacement = "_mrid", fixed = TRUE) %>%
        gsub(pattern = "Type", replacement = "_type", fixed = TRUE) %>%
        gsub(pattern = "revisionNumber", replacement = "revision_number", fixed = TRUE) %>%
        gsub(pattern = "createdDateTime", replacement = "dt_created", fixed = TRUE) %>%
        gsub(pattern = "objectAggregation", replacement = "object_aggregation", fixed = TRUE) %>%
        gsub(pattern = "TimeSeries_mrid", replacement = "ts_mrid", fixed = TRUE) %>%
        gsub(pattern = "TimeSeries.", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "quantity_Measure_Unit.name", replacement = "quantity_measure_unit", fixed = TRUE) %>%
        gsub(pattern = "process.", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "PowerSystemResources.psr_type", replacement = "resource_psr_type", fixed = TRUE) %>%
        gsub(pattern = "PowerSystemResources.", replacement = "resource_psr_type_", fixed = TRUE) %>%
        gsub(pattern = "PowerSystemResources", replacement = "resource_psr_type", fixed = TRUE) %>%
        gsub(pattern = "Period.", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "timeInterval.", replacement = "dt_", fixed = TRUE) %>%
        gsub(pattern = "StartDateTime", replacement = "start", fixed = TRUE) %>%
        tolower()

      ## converting timestamp-like columns to timestamps
      ## adding definitions to codes
      ## and reordering columns
      periodly <- periodly %>%
        dplyr::mutate(dt_created = as.POSIXct(x = dt_created,
                                              tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                             "%Y-%m-%dT%H:%M:%SZ"),
                                              tz = "UTC"),
                      dt_start   = as.POSIXct(x = dt_start,
                                              tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                             "%Y-%m-%dT%H:%M:%SZ"),
                                              tz = "UTC"),
                      dt_end     = as.POSIXct(x = dt_end,
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
        merge(y     = StandardObjectAggregationTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(object_aggregation     = CODE,
                              object_aggregation_def = DEFINITION),
              by    = "object_aggregation",
              all.x = TRUE) %>%
        merge(y     = StandardCurveTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(curve_type     = CODE,
                              curve_type_def = DEFINITION),
              by    = "curve_type",
              all.x = TRUE) %>%
        merge(y     = StandardAssetTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(resource_psr_type     = CODE,
                              resource_psr_type_def = DEFINITION),
              by    = "resource_psr_type",
              all.x = TRUE) %>%
        dplyr::select(base::intersect(x = c("process_type", "process_type_def",
                                            "curve_type", "curve_type_def",
                                            "object_aggregation", "object_aggregation_def",
                                            "business_type", "business_type_def",
                                            "document_mrid", "inbiddingzone_domain_mrid", "outbiddingzone_domain_mrid",
                                            "resource_mrid",
                                            "resource_psr_type", "resource_psr_type_def",
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

en_gen_agg_gen_pertype_api_req_helper <- function(eic,
                                                  period_start,
                                                  period_end,
                                                  security_token,
                                                  psr_type = NULL){

  ## composing url(s)
  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A75",               # <- Actual generation per type - A document providing the actual generation per generation type for a period.
    "&processType=A16",                # <- Realised - The process for the treatment of realised data as opposed to forecast data
    "&in_Domain=", eic,                # <- reflects Generation values
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    { if(!is.null(psr_type)) paste0("&psrType=", psr_type) },
    "&securityToken=", security_token
  )

  return(url)
}

ts_agg_gen_helper <- function(ts, tidy_output = FALSE){

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

  ## extracting content of "MktPSRType" subbranch
  mktpsrtype    <- tibble::tibble(psrType = unlist(ts$MktPSRType$psrType)) %>%
                     dplyr::rename_with(~paste0("TimeSeries.PowerSystemResources.", .x))

  ## removing "Period" and "MktPSRType" subbranches
  ts$Period     <- NULL
  ts$MktPSRType <- NULL

  ## converting to table the remaining parts
  ts            <- ts %>%
                     purrr::map_df(unlist) %>%
                     dplyr::rename_with(~paste0("TimeSeries.", .x))

  ## if we have "MktPSRType" data, then we add as a new column
  if (nrow(mktpsrtype) > 0) {
    ts <- dplyr::bind_cols(ts, mktpsrtype)
  }

  ## column-wise appending the parts
  if (tidy_output) {
    dplyr::bind_cols(ts, points) %>%
      return()
  } else {
    ts %>%
      tibble::add_column(periods = list(points)) %>%
      return()
  }

}



#' Get actual generation output per generation unit from Entsoe
#'
#' @param eic Energy Identification Code(s) of the control area(s) (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format  One day range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format  One day range limit applies
#' @param gen_type Defaults to NULL, otherwise list of generation type codes from StandardAssetTypeList table
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
#' de_50Hertz_20180101 <- en_generation_act_gen_per_unit(eic = "10YDE-VE-------2",
#'                                                       period_start = lubridate::ymd("2020-01-31", tz = "CET"),
#'                                                       period_end = lubridate::ymd("2020-02-01", tz = "CET"))
#'
en_generation_act_gen_per_unit <- function(eic,
                                           period_start   = lubridate::ymd(Sys.Date() - 1L, tz = "CET"),
                                           period_end     = lubridate::ymd(Sys.Date(), tz = "CET"),
                                           gen_type       = NULL,
                                           tidy_output    = TRUE,
                                           security_token = Sys.getenv("ENTSOE_PAT")) {

  ## checking if only one eic provided
  if (!exists("eic")) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")

  ## checking if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  ## converting timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end   <- url_posixct_format(period_end)

  ## breaking time interval of period_start and period_end into 24 hour long parts
  period_start_list <- strptime(x = period_start, format = "%Y%m%d%H%M", tz = "UTC") %>% as.POSIXct(tz = "UTC") +
    seq(from = 0L,
        to   = difftime(time1 = strptime(x = period_end, format = "%Y%m%d%H%M", tz = "UTC") %>% as.POSIXct(tz = "UTC"),
                        time2 = strptime(x = period_start, format = "%Y%m%d%H%M", tz = "UTC") %>% as.POSIXct(tz = "UTC"),
                        units = "days") %>%
          ceiling() - 1L) * 24L*60L*60L
  period_end_list   <- data.table::shift(x    = period_start_list,
                                         type = "lead",
                                         fill = strptime(x = period_end, format = "%Y%m%d%H%M", tz = "UTC") %>% as.POSIXct(tz = "UTC"))

  ## converting timestamps into accepted format
  period_start_list <- url_posixct_format(period_start_list)
  period_end_list   <- url_posixct_format(period_end_list)

  ## composing GET request url for a (maximum) 24 hours long period
  url_list <- seq_along(period_start_list) %>%
    purrr::map(~en_gen_act_gen_perunit_api_req_helper(eic            = eic,
                                                      period_start   = period_start_list[[ .x ]],
                                                      period_end     = period_end_list[[ .x ]],
                                                      psr_type       = gen_type,
                                                      security_token = security_token))

  ## iterating (maximum) 24 hours long periods thru
  ## and append them into one tibble
  gnrtn  <- url_list %>%
    purrr::map_df(function(URL) {

      ## send GET request and extract result content and removing null contents
      en_cont <- unname( URL ) %>%
        purrr::map(api_req_safe) %>%
        purrr::map("result") %>%
        purrr::compact()

      ## if valid contents left
      if (length(en_cont) > 0L) {

        ## composing a result table
        ec      <- en_cont %>%
          purrr::map(xml2::as_list) %>%
          purrr::map("GL_MarketDocument") %>%
          unlist(recursive = FALSE)
        main  <- dplyr::bind_cols(tibble::tibble(document.mRID = unlist(ec[names(ec) == "mRID"],
                                                                        use.names = FALSE),
                                                 revisionNumber = unlist(ec[names(ec) == "revisionNumber"],
                                                                         use.names = FALSE),
                                                 process.processType = unlist(ec[names(ec) == "process.processType"],
                                                                              use.names = FALSE),
                                                 createdDateTime = unlist(ec[names(ec) == "createdDateTime"],
                                                                          use.names = FALSE)),
                                  dplyr::bind_rows(unlist(ec[names(ec) == "time_Period.timeInterval"])))
        ts    <- ec[names(ec) == "TimeSeries"] %>%
          purrr::map_dfr(ts_act_gen_helper,
                         tidy_output = tidy_output)
        periodly   <- dplyr::bind_cols(main, ts)

        ## if the output should be tidy
        if (tidy_output) {

          ## remove not necessary columns
          periodly   <- periodly %>%
            dplyr::select(-time_Period.timeInterval.start,
                          -time_Period.timeInterval.end)

          ## renaming columns
          names(periodly) <- names(periodly) %>%
            gsub(pattern = ".mRID", replacement = "_mrid", fixed = TRUE) %>%
            gsub(pattern = "Type", replacement = "_type", fixed = TRUE) %>%
            gsub(pattern = "revisionNumber", replacement = "revision_number", fixed = TRUE) %>%
            gsub(pattern = "createdDateTime", replacement = "dt_created", fixed = TRUE) %>%
            gsub(pattern = "objectAggregation", replacement = "object_aggregation", fixed = TRUE) %>%
            gsub(pattern = "registeredResource_mrid", replacement = "resource_mrid", fixed = TRUE) %>%
            gsub(pattern = "TimeSeries_mrid", replacement = "ts_mrid", fixed = TRUE) %>%
            gsub(pattern = "TimeSeries.", replacement = "", fixed = TRUE) %>%
            gsub(pattern = "quantity_Measure_Unit.name", replacement = "quantity_measure_unit", fixed = TRUE) %>%
            gsub(pattern = "process.", replacement = "", fixed = TRUE) %>%
            gsub(pattern = "PowerSystemResources.psr_type", replacement = "resource_psr_type", fixed = TRUE) %>%
            gsub(pattern = "PowerSystemResources.", replacement = "resource_psr_type_", fixed = TRUE) %>%
            gsub(pattern = "PowerSystemResources", replacement = "resource_psr_type", fixed = TRUE) %>%
            gsub(pattern = "Period.", replacement = "", fixed = TRUE) %>%
            gsub(pattern = "timeInterval.", replacement = "dt_", fixed = TRUE) %>%
            gsub(pattern = "StartDateTime", replacement = "start", fixed = TRUE) %>%
            tolower()

          ## converting timestamp-like columns to timestamps
          ## adding definitions to codes
          ## and reordering columns
          periodly   <- periodly %>%
            dplyr::mutate(dt_created = as.POSIXct(x = dt_created,
                                                  tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                                 "%Y-%m-%dT%H:%M:%SZ"),
                                                  tz = "UTC"),
                          dt_start   = as.POSIXct(x = dt_start,
                                                  tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                                 "%Y-%m-%dT%H:%M:%SZ"),
                                                  tz = "UTC"),
                          dt_end     = as.POSIXct(x = dt_end,
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
            merge(y     = StandardObjectAggregationTypeList[, c("CODE", "DEFINITION")] %>%
                    dplyr::rename(object_aggregation = CODE,
                                  object_aggregation_def = DEFINITION),
                  by    = "object_aggregation",
                  all.x = TRUE) %>%
            merge(y     = StandardCurveTypeList[, c("CODE", "DEFINITION")] %>%
                    dplyr::rename(curve_type = CODE,
                                  curve_type_def = DEFINITION),
                  by    = "curve_type",
                  all.x = TRUE) %>%
            merge(y     = StandardAssetTypeList[, c("CODE", "DEFINITION")] %>%
                    dplyr::rename(resource_psr_type = CODE,
                                  resource_psr_type_def = DEFINITION),
                  by    = "resource_psr_type",
                  all.x = TRUE) %>%
            dplyr::select(base::intersect(x = c("process_type", "process_type_def",
                                                "curve_type", "curve_type_def",
                                                "object_aggregation", "object_aggregation_def",
                                                "business_type", "business_type_def",
                                                "document_mrid", "inbiddingzone_domain_mrid", "outbiddingzone_domain_mrid",
                                                "resource_mrid",
                                                "resource_psr_type", "resource_psr_type_def",
                                                "resource_psr_type_mrid", "resource_psr_type_name",
                                                "revision_number",
                                                "ts_mrid", "dt_created",
                                                "dt_start", "dt_end",
                                                "resolution", "position",
                                                "start", "quantity", "quantity_measure_unit"),
                                          y = names(.))) %>%
            dplyr::arrange(dt_created,dt_start, start)
        }

        ## returning with the periodly table
        return(tibble::as_tibble(periodly))

      } else {

        ## returning with an empty table
        return(tibble::tibble())

      }
    })

  ## returning with all the generation data
  return(gnrtn)
}


## url composer for Actual Generation Output per Generation Unit [16.1.A] request
en_gen_act_gen_perunit_api_req_helper <- function(eic,
                                                  period_start,
                                                  period_end,
                                                  security_token,
                                                  psr_type = NULL,
                                                  registered_resource = NULL) {

  ## creating list of to be combined parameter lists and removing empty combinations
  par_list    <- list("psrType" = psr_type,
                      "registeredResource" = registered_resource,
                      "in_Domain" = eic) %>%
                   purrr::compact()

  ## creating combination matrix
  par_matrix  <- expand.grid(par_list,
                             stringsAsFactors = FALSE,
                             KEEP.OUT.ATTRS   = FALSE)

  ## composing url(s)
  url <- paste0("https://transparency.entsoe.eu/api",
                "?documentType=A73",               # <- Actual generation - A document providing the actual generation for a period.
                "&processType=A16",                # <- Realised - The process for the treatment of realised data as opposed to forecast data
                "&periodStart=",period_start,
                "&periodEnd=", period_end,
                { apply(X = unique(par_matrix),
                        MARGIN = 1L,
                        FUN = function(r) {
                          paste(vapply(X = colnames(par_matrix),
                                         FUN = function(cn) sprintf(fmt = "&%s=%s", cn, r[[ cn ]]),
                                         FUN.VALUE = "foo"),
                                 collapse = "")
                        }) },
                "&securityToken=", security_token)

  ## naming result vector according to control area EIC
  names(url) <- eic

  ## returning with the composed url
  return(url)
}

## converter from list tree to table for Actual Generation Output per Generation Unit [16.1.A]
ts_act_gen_helper <- function(ts, tidy_output=FALSE) {

  ## extracting content of "Period" subbranch
  points        <- ts$Period[names(ts$Period) == "Point"] %>%
                     purrr::map_df(unlist) %>%
                     purrr::map_df(as.numeric) %>%
                     tibble::add_column(StartDateTime = dt_helper(tz_start      = unlist(ts$Period$timeInterval$start,
                                                                                         use.names = FALSE),
                                                                  tz_resolution = unlist(ts$Period$resolution,
                                                                                         use.names = FALSE),
                                                                  tz_position   = .$position),
                                        resolution = unlist(ts$Period[names(ts$Period) == "resolution"],
                                                            use.names = FALSE))
  points        <- purrr::map_df(ts$Period[names(ts$Period) == "timeInterval"],
                                 unlist) %>%
                     dplyr::rename_with(~ paste0("timeInterval.", .x)) %>%
                     dplyr::bind_cols(points) %>%
                     dplyr::rename_with(~ paste0("TimeSeries.Period.", .x))

  ## extracting content of "MktPSRType" subbranch
  mktpsrtype    <- dplyr::bind_cols(tibble::tibble(psrType = unlist(ts$MktPSRType$psrType)),
                                    tibble::as_tibble_row(unlist(ts$MktPSRType$PowerSystemResources))) %>%
                     dplyr::rename_with(~ paste0("TimeSeries.PowerSystemResources.", .x))

  ## removing "Period" and "MktPSRType" subbranches
  ts$Period     <- NULL
  ts$MktPSRType <- NULL

  ## converting to table the remaining parts
  ts            <- ts %>%
                     purrr::map_df(unlist) %>%
                     dplyr::rename_with(~ paste0("TimeSeries.", .x))

  ## log message
  message(mktpsrtype$TimeSeries.PowerSystemResources.mRID, " - ",
          mktpsrtype$TimeSeries.PowerSystemResources.name, " transfomation has done")

  ## column-wise appending the parts
  if(tidy_output) {
    dplyr::bind_cols(ts, mktpsrtype, points) %>%
      return()
  } else {
    dplyr::bind_cols(ts, mktpsrtype) %>%
      tibble::add_column(periods = list(points)) %>%
      return()
  }

}



#' Get Day-ahead aggregated generation forecast from Entsoe.
#' It is an estimate of the total scheduled generation per bidding zone for the following day
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' fr_2020 <- en_generation_day_ahead_agg_gen(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2020-02-01", tz = "CET"), period_end = lubridate::ymd("2020-03-01", tz = "CET"))
#'
en_generation_day_ahead_agg_gen <- function(eic, period_start, period_end, security_token = Sys.getenv("ENTSOE_PAT")){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url      <- en_gen_day_ahead_agg_gen_api_req_helper(eic = eic,
                                                      period_start = period_start,
                                                      period_end = period_end,
                                                      security_token = security_token)

  en_cont <- api_req_safe(url)
  en_cont <- xml2::as_list(en_cont$result)
  en_cont <- en_cont$GL_MarketDocument
  en_cont <- en_cont[names(en_cont) == "TimeSeries"]
  en_cont <- lapply(en_cont, ts_agg_gen_helper, tidy_output = TRUE) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-TimeSeries.mRID, -TimeSeries.businessType, -TimeSeries.objectAggregation, -TimeSeries.curveType, -TimeSeries.Period.position)

  en_cont
}

en_gen_day_ahead_agg_gen_api_req_helper <- function(eic, period_start, period_end, security_token){

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A71",
    "&processType=A01",
    "&in_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  url
}


#' Get total load from Entsoe
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' fr_2020 <- en_generation_day_ahead_gen_forecast_ws(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2020-02-01", tz = "CET"), period_end = lubridate::ymd("2020-03-01", tz = "CET"))
#'
en_generation_day_ahead_gen_forecast_ws <- function(eic, period_start, period_end, security_token = Sys.getenv("ENTSOE_PAT")){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  gen_type <- as.character(c("B16", "B18", "B19"))

  url_list <- purrr::map(gen_type,
                         ~en_gen_day_ahead_gen_forecast_ws_api_req_helper(psr_type = .x,
                                                                          eic = eic,
                                                                          period_start = period_start,
                                                                          period_end = period_end,
                                                                          security_token = security_token))

  en_cont <- purrr::map(url_list, api_req_safe)
  en_cont <- purrr::map(en_cont, "result")
  en_cont <- purrr::map(en_cont, xml2::as_list)
  en_cont <- purrr::map(en_cont, "GL_MarketDocument")
  names(en_cont) <- gen_type
  en_cont[sapply(en_cont, is.null)] <- NULL
  en_cont <- purrr::imap(en_cont, ~{ .x[names(.x) == "TimeSeries"] })
  en_cont <- purrr::map(en_cont, function(ec) purrr::map(ec, ~ts_agg_gen_helper(.x)) %>% dplyr::bind_rows())
  en_cont <- purrr::map(en_cont, ~dplyr::select(.x, -TimeSeries.mRID, -TimeSeries.businessType, -TimeSeries.objectAggregation, -TimeSeries.curveType) %>%
                                      dplyr::rename_with( function(col) sub( "TimeSeries\\.", "", col)))

  en_cont
}

en_gen_day_ahead_gen_forecast_ws_api_req_helper <- function(psr_type, eic, period_start, period_end, security_token){

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A69",
    "&processType=A01",
    "&psrType=", psr_type,
    "&in_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  url
}

