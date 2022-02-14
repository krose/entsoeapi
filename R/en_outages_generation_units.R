
#' Get outages for Generation units.
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_start_update Period start update, POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end_update Period end update, POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param doc_status Document status. "A05" for active, "A09" for cancelled and "A13" for withdrawn.
#' @param business_type Defaults to NULL. "A53" for planned maintenance. "A54" for unplanned outage.
#' @param tidy_output flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#'
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  france <- en_outages(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2019-11-12", tz = "CET"), period_end = lubridate::ymd("2019-11-13", tz = "CET"))
#'
en_outages <- function(eic,
                       period_start = lubridate::ymd(Sys.Date(), tz = "CET"),
                       period_end = lubridate::ymd(Sys.Date() + 3, tz = "CET"),
                       period_start_update = NULL,
                       period_end_update = NULL,
                       doc_status = "A05",
                       business_type = NULL,
                       tidy_output = TRUE,
                       security_token = Sys.getenv("ENTSOE_PAT")) {

  en_df_gen <- try(en_outages_generation_units(eic = eic,
                                               period_start = period_start,
                                               period_end = period_end,
                                               period_start_update = period_start_update,
                                               period_end_update = period_end_update,
                                               doc_status = doc_status,
                                               business_type = business_type,
                                               tidy_output = tidy_output,
                                               security_token = security_token))
  en_df_pro <- try(en_outages_production_units(eic = eic,
                                               period_start = period_start,
                                               period_end = period_end,
                                               period_start_update = period_start_update,
                                               period_end_update = period_end_update,
                                               doc_status = doc_status,
                                               business_type = business_type,
                                               tidy_output = tidy_output,
                                               security_token = security_token))

  if(inherits(en_df_gen, "try-error")){
    message("Outages Generation unit error. Try calling the function en_outages_generation_units() to see the error message.")
    en_df_gen <- NULL
  }

  if(inherits(en_df_pro, "try-error")){
    message("Outages Production unit error. Try calling the function en_outages_production_units() to see the error message.")
    en_df_pro <- NULL
  }

  if(!is.null(en_df_gen) | !is.null(en_df_pro)){
    en_df <- dplyr::bind_rows(en_df_gen, en_df_pro)
    #en_df <- dplyr::mutate(en_df, dt_created = lubridate::ymd_hms(dt_created, tz = "UTC")) << moved into _helper_tidy() functions
  } else {
    return(NULL)
  }

  en_df
}



#' Get outages for Generation units.
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_start_update Period start update, POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end_update Period end update, POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param doc_status Document status. "A05" for active, "A09" for cancelled and "A13" for withdrawn.
#' @param business_type Defaults to NULL. "A53" for planned maintenance. "A54" for unplanned outage.
#' @param tidy_output flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  france <- en_outages_generation_units(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2019-11-12", tz = "CET"), period_end = lubridate::ymd("2019-11-13", tz = "CET"))
#'
en_outages_generation_units <- function(eic,
                                        period_start = lubridate::ymd(Sys.Date(), tz = "CET"),
                                        period_end = lubridate::ymd(Sys.Date() + 3, tz = "CET"),
                                        period_start_update = NULL,
                                        period_end_update = NULL,
                                        doc_status = "A05",
                                        business_type = NULL,
                                        tidy_output = TRUE,
                                        security_token = Sys.getenv("ENTSOE_PAT")) {

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(eic) > 1){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A80",
    "&biddingZone_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )
  if(!is.null(doc_status)){
    url <- paste0(url, "&docStatus=", doc_status)
  }
  if(!is.null(business_type)){
    url <- paste0(url, "&businessType=", business_type)
  }
  if(!is.null(period_start_update) & !is.null(period_end_update)){
    url <- paste0(url,
                  "&periodStartUpdate=", url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  en_content <- api_req_zip(url, file_type = "generation")

  if(isTRUE(tidy_output) & nrow(en_content) > 0L){
    en_content <- outages_gen_helper_tidy(en_content)
    en_content$type <- "generation units"
    en_content$doc_status <- doc_status
  }

  en_content
}


#' Get outages for production units.
#'
#' @param eic Energy Identification Codeof the bidding zone
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_start_update Period start update, POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end_update Period end update, POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param doc_status Document status. "A05" for active, "A09" for cancelled and "A13" for withdrawn.
#' @param business_type Defaults to NULL. "A53" for planned maintenance. "A54" for unplanned outage.
#' @param tidy_output flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  france <- en_outages_production_units(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2019-11-12", tz = "CET"), period_end = lubridate::ymd("2019-11-13", tz = "CET"))
#'
en_outages_production_units <- function(eic,
                                        period_start = lubridate::ymd(Sys.Date(), tz = "CET"),
                                        period_end = lubridate::ymd(Sys.Date() + 3, tz = "CET"),
                                        period_start_update = NULL,
                                        period_end_update = NULL,
                                        doc_status = "A05",
                                        business_type = NULL,
                                        tidy_output = TRUE,
                                        security_token = Sys.getenv("ENTSOE_PAT")) {

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(eic) > 1L){
    stop("This wrapper only supports one EIC per request.")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A77",
    "&biddingZone_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )
  if(!is.null(doc_status)){
    url <- paste0(url, "&docStatus=", doc_status)
  }
  if(!is.null(business_type)){
    url <- paste0(url, "&businessType=", business_type)
  }
  if(!is.null(period_start_update) & !is.null(period_end_update)){
    url <- paste0(url,
                  "&periodStartUpdate=", url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  en_content <- api_req_zip(url, file_type = "production")

  if(isTRUE(tidy_output) & nrow(en_content) > 0L){
    en_content <- outages_prod_helper_tidy(en_content)
    en_content$type <- "production units"
    en_content$doc_status <- doc_status
  }

  en_content
}



outages_gen_helper_tidy <- function(out_gen_df){

  out_gen_df <- out_gen_df %>%
    dplyr::mutate(dt_created = lubridate::ymd_hms(createdDateTime, tz = "UTC")) %>%
    dplyr::select(-createdDateTime) %>%
    dplyr::mutate(dt_start = lubridate::ymd_hm(paste0(start_DateAndOrTime.date, " ", stringr::str_sub(start_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-start_DateAndOrTime.time, -start_DateAndOrTime.date) %>%
    dplyr::mutate(dt_end = lubridate::ymd_hm(paste0(end_DateAndOrTime.date, " ", stringr::str_sub(end_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-end_DateAndOrTime.time, -end_DateAndOrTime.date) %>%
    dplyr::rename(bidding_zone_mrid = biddingZone_Domain.mRID,
                  quantity_measure_unit = quantity_Measure_Unit.name,
                  curve_type = curveType,
                  resource_location_name = production_RegisteredResource.location.name,
                  resource_mrid = production_RegisteredResource.mRID,
                  resource_name = production_RegisteredResource.name,
                  resource_psr_type = production_RegisteredResource.pSRType.psrType,
                  resource_psr_type_mrid = production_RegisteredResource.pSRType.powerSystemResources.mRID,
                  resource_psr_type_name = production_RegisteredResource.pSRType.powerSystemResources.name,
                  resource_psr_type_capacity = production_RegisteredResource.pSRType.powerSystemResources.nominalP,
                  revision_number = revisionNumber) %>%
    dplyr::mutate(revision_number = as.integer(revision_number),
                  resource_psr_type_mrid = dplyr::if_else(is.na(resource_psr_type_mrid), "none", resource_psr_type_mrid),
                  resource_psr_type_name = dplyr::if_else(is.na(resource_psr_type_name), "none", resource_psr_type_name),
                  resource_psr_type_capacity = as.integer(resource_psr_type_capacity)) %>%
    dplyr::arrange(resource_psr_type, dt_start, resource_mrid) %>%
    tidyr::unnest(data = ., cols = vapply(X = ., FUN = is.list, FUN.VALUE = TRUE) %>% which() %>% names()) %>%
    {
      reason_cols <- grep(pattern = "^Reason|^reason", x = names(.), value = TRUE);
      if(length(x = reason_cols) > 0L) {
        tidyr::unite(data = ., col = "reason_code", reason_cols, sep = "|", na.rm = TRUE)
      } else {
        .
      }
    } %>%
    # dplyr::mutate(resource_psr_type_capacity = as.integer(resource_psr_type_capacity)
    #               quantity = as.numeric(quantity),
    #               start = lubridate::ymd_hm(start, tz = "UTC"),
    #               end = lubridate::ymd_hm(end, tz = "UTC")
    #               )
    dplyr::select(mRID, businessType, bidding_zone_mrid, quantity_measure_unit, curve_type,
                  resource_mrid, resource_name, resource_location_name, resource_psr_type,
                  resource_psr_type_mrid, resource_psr_type_name, resource_psr_type_capacity,
                  mkt_doc_mrid, revision_number, reason_code, start, end, resolution, position,
                  quantity, dt_created, dt_start, dt_end)

  return(out_gen_df)
}

outages_prod_helper_tidy <- function(out_prod_df){

  out_prod_df <- out_prod_df %>%
    dplyr::mutate(dt_created = lubridate::ymd_hms(createdDateTime, tz = "UTC")) %>%
    dplyr::select(-createdDateTime) %>%
    dplyr::mutate(dt_start = lubridate::ymd_hm(paste0(start_DateAndOrTime.date, " ", stringr::str_sub(start_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-start_DateAndOrTime.time, -start_DateAndOrTime.date) %>%
    dplyr::mutate(dt_end = lubridate::ymd_hm(paste0(end_DateAndOrTime.date, " ", stringr::str_sub(end_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-end_DateAndOrTime.time, -end_DateAndOrTime.date) %>%
    dplyr::rename(bidding_zone_mrid = biddingZone_Domain.mRID,
                  quantity_measure_unit = quantity_Measure_Unit.name,
                  curve_type = curveType,
                  resource_location_name = production_RegisteredResource.location.name,
                  resource_mrid = production_RegisteredResource.mRID,
                  resource_name = production_RegisteredResource.name,
                  resource_psr_type = production_RegisteredResource.pSRType.psrType,
                  resource_psr_type_capacity = production_RegisteredResource.pSRType.powerSystemResources.nominalP,
                  revision_number = revisionNumber) %>%
    dplyr::mutate(revision_number = as.integer(revision_number),
                  resource_psr_type_mrid = NA_character_,
                  resource_psr_type_name = NA_character_,
                  resource_psr_type_mrid = dplyr::if_else(is.na(resource_psr_type_mrid), "none", resource_psr_type_mrid),
                  resource_psr_type_name = dplyr::if_else(is.na(resource_psr_type_name), "none", resource_psr_type_name),
                  resource_psr_type_capacity = as.integer(resource_psr_type_capacity)) %>%
    dplyr::arrange(resource_psr_type, dt_start, resource_mrid) %>%
    tidyr::unnest(data = ., cols = vapply(X = ., FUN = is.list, FUN.VALUE = TRUE) %>% which() %>% names()) %>%
    {
      reason_cols <- grep(pattern = "^Reason|^reason", x = names(.), value = TRUE);
      if(length(x = reason_cols) > 0L) {
        tidyr::unite(data = ., col = "reason_code", reason_cols, sep = "|", na.rm = TRUE)
      } else {
        .
      }
    } %>%
    # dplyr::mutate(resource_psr_type_capacity = as.integer(resource_psr_type_capacity)
    #               quantity = as.numeric(quantity),
    #               start = lubridate::ymd_hm(start, tz = "UTC"),
    #               end = lubridate::ymd_hm(end, tz = "UTC"))
    dplyr::select(mRID, businessType, bidding_zone_mrid, quantity_measure_unit, curve_type,
                  resource_mrid, resource_name, resource_location_name, resource_psr_type,
                  resource_psr_type_mrid, resource_psr_type_name, resource_psr_type_capacity,
                  mkt_doc_mrid, revision_number, reason_code, start, end, resolution, position,
                  quantity, dt_created, dt_start, dt_end)

  return(out_prod_df)
}

#' Convert the outages to an hourly timeseries.
#'
#' The function removes tidy observations less than 59 minutes.
#'
#' @param out_df Tidy data.frame of outages.
#'
#' @export
#'
en_outages_tidy_to_ts <- function(out_df){
if(any("resource_psr_type_capacity" == names(x = out_df))) {
  out_df <- out_df %>%
    dplyr::filter((end - start) > 0.25) %>%
    tibble::add_column(ts = lapply(X   = 1L:nrow(.),
                                   FUN = function(i) {
                                     dt_seq_helper(from = .$start[i],
                                                   to   = .$end[i],
                                                   qty  = .$quantity[i])
                                   })) %>%
    tidyr::unnest(cols = ts) %>%
    tibble::add_column(qty_outage = .$resource_psr_type_capacity - .$qty)
} else {
  warning("Table has not transformed, only the 'tidy_output' version\nof the outage table can be transformed with this function!")
}

return(out_df)
}
# en_outages_tidy_to_ts <- function(out_g_df){
#
#   # filtrer mindre end 15 minutters beskeder - filters messages of outages less than 15 minutes
#   out_gen_df <- out_g_df %>%
#     dplyr::filter((end - start) > 0.25) %>%
#     dplyr::select(any_of(c("mRID", "businessType", "mkt_doc_mrid", "resource_psr_type", "resource_psr_type_mrid",
#                   "resource_psr_type_name", "resource_psr_type_capacity", "revision_number",
#                   "resolution", "dt_created", "start", "end", "quantity")))
#
#   out_gen_df$ts <- lapply(seq_along(out_gen_df$resource_psr_type),
#                           function(x){dt_seq_helper(out_gen_df$start[x], out_gen_df$end[x], out_gen_df$resolution[x], out_gen_df$quantity[x])})
#
#   out_gen_df <- out_gen_df %>%
#     tidyr::unnest(ts) %>%
#     dplyr::mutate(qty_outage = resource_psr_type_capacity - qty)
#
#   out_gen_df
# }

outages_gen_helper <- function(x){

  # ap_not <- x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) != "Available_Period"]
  # ap_not <- tibble::as_tibble(lapply(ap_not, unlist, recursive = FALSE), .name_repair = "minimal")
  # ap_not$Reason <- NULL
  # ap_not$mkt_doc_mrid <- x$Unavailability_MarketDocument$mRID[[1]]
  # ap_not$revisionNumber <- x$Unavailability_MarketDocument$revisionNumber[[1]]
  # ap_not$createdDateTime <- x$Unavailability_MarketDocument$createdDateTime[[1]]
  #
  # ap <- unname(x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) == "Available_Period"])
  #
  # start <- unlist(purrr::map(ap, ~.x$timeInterval$start[[1]]))
  # end <- unlist(purrr::map(ap, ~.x$timeInterval$end[[1]]))
  # resolution <- unlist(purrr::map(ap, ~.x$resolution))
  # position <- unlist(purrr::map(ap, ~.x$Point$position[[1]]))
  # quantity <- unlist(purrr::map(ap, ~.x$Point$quantity[[1]]))
  #
  # ap_not$available_period <- list(tibble::tibble(start, end, resolution, position, quantity))

  ## drilling one level deeper
  x                  <- x$Unavailability_MarketDocument

  ## picking main metadata of Unavailability_MarketDocument
  Reason             <- unlist(x$Reason)
  mkt_doc_mrid       <- x$mRID[[ 1L ]]
  revisionNumber     <- x$revisionNumber[[ 1L ]]
  createdDateTime    <- x$createdDateTime[[ 1L ]]

  ## drilling one level deeper
  x                  <- x$TimeSeries

  ## picking main metadata of TimeSeries branch
  ## and creating a tibble from those
  ap_not             <- x[ !names(x) %in% c("Available_Period", "Reason") ]
  ap_not             <- lapply(X   = ap_not,
                               FUN = unlist,
                               recursive = FALSE) %>%
                            tibble::as_tibble(.name_repair = "minimal")

  ## adding already collected meta data to the tibble as new columns
  ap_not$mkt_doc_mrid    <- mkt_doc_mrid
  ap_not$revisionNumber  <- revisionNumber
  ap_not$createdDateTime <- createdDateTime
  ap_not$Reason.code     <- unique(Reason[ names(Reason) == "code" ]) %>% sort()

  ## iterating Available_Period branches thru and appending the result tables together
  ap <- lapply(X   = x[ names(x) == "Available_Period" ],
               FUN = function(p) {
                  ## picking the start and end timestamps into variables
                  start_ts    <- as.POSIXct(p$timeInterval$start[[ 1L ]],
                                            tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                           "%Y-%m-%dT%H:%M:%SZ"),
                                            tz = "UTC")
                  end_ts      <- as.POSIXct(p$timeInterval$end[[ 1L ]],
                                            tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                           "%Y-%m-%dT%H:%M:%SZ"),
                                            tz = "UTC")
                  ## translating the resolution value into seconds
                  multip      <- data.table::fcase(p$resolution[[ 1L ]] == "PT60M",      60L*60L,
                                                   p$resolution[[ 1L ]] == "PT30M",      30L*60L,
                                                   p$resolution[[ 1L ]] == "PT15M",      15L*60L,
                                                   p$resolution[[ 1L ]] == "PT1M",        1L*60L,
                                                   p$resolution[[ 1L ]] == "P1D",    24L*60L*60L,
                                                   p$resolution[[ 1L ]] == "P7D", 7L*24L*60L*60L)
                  ## building tibble from the list of Point subvalues (position, quantity)
                  point_table <- lapply(X   = p[ names(x = p) == "Point" ],
                                        FUN = function(p) {
                                            tibble::as_tibble_row(unlist(p))
                                        }) %>%
                                   dplyr::bind_rows()
                  point_table$position <- as.numeric(point_table$position)
                  point_table$quantity <- as.numeric(point_table$quantity)
                  ## calculating start & end timestamp from start_ts, multip & end_ts values
                  point_table <- tibble::add_column(point_table,
                                                    start = start_ts + (point_table$position - 1L)*multip,
                                                    .before = "position")
                  point_table <- tibble::add_column(point_table,
                                                    end = data.table::shift(x = point_table$start, type = "lead"),
                                                    .before = "position")
                  point_table$end[ nrow(point_table) ] <- end_ts
                  ## adding corresponding resolution value
                  point_table <- tibble::add_column(point_table,
                                                    resolution = p$resolution[[ 1L ]],
                                                    .before = "position")
                  return(point_table)
                }) %>% dplyr::bind_rows()

  ## inserting available period table as nested table
  ap_not$available_period <- list(ap)

  return(ap_not)
}


#' Clean data for outages
#'
#' @param out_df Outages data.frame
#' @param new_style If TRUE, some less useless descriptive columns are replaced by more useful ones,
#' furthermore it gives back all the rows not only the last ones.
#'
#' @export
#'
en_outages_clean <- function(out_df,new_style = FALSE){

  if(any("resource_mrid" == names(x = out_df))) {
    if(isTRUE(new_style)) {
      out_df <- out_df %>%
        dplyr::rename(business_type = businessType) %>%
        merge(y     = StandardStatusTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(doc_status = CODE, doc_status_def = DEFINITION),
              by    = "doc_status",
              all.x = TRUE) %>%
        merge(x     = .,
              y     = StandardAssetTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(resource_psr_type = CODE, resource_psr_type_def = DEFINITION),
              by    = "resource_psr_type",
              all.x = TRUE) %>%
        merge(y     = StandardBusinessTypeList[, c("CODE", "DEFINITION")] %>%
                dplyr::rename(business_type = CODE, business_type_def = DEFINITION),
              by    = "business_type",
              all.x = TRUE) %>%
        dplyr::select(base::intersect( x = c("type", "doc_status", "doc_status_def", "business_type",
                                             "business_type_def", "mkt_doc_mrid", "bidding_zone_mrid",
                                             "resource_mrid", "resource_name", "resource_location_name",
                                             "resource_psr_type", "resource_psr_type_def",
                                             "resource_psr_type_mrid", "resource_psr_type_name",
                                             "revision_number", "dt_created", "dt_start", "dt_end",
                                             "reason_code", "resolution", "position", "start", "end", "quantity",
                                             "quantity_measure_unit", "resource_psr_type_capacity"),
                                       y = names(.))) %>%
        dplyr::arrange(dt_created,dt_start, start)
    } else {
      out_df <- out_df %>%
        dplyr::select(!doc_status) %>%
        #dplyr::mutate(dt_created = lubridate::ymd_hms(dt_created, tz = "UTC")) %>% << moved into _helper_tidy() functions
        dplyr::group_by_at(base::intersect( x = c("mkt_doc_mrid", "resource_mrid",
                                                  "resource_name","resource_location_name",
                                                  "resource_psr_type","resource_psr_type_capacity",
                                                  "resource_psr_type_mrid", "resource_psr_type_name",
                                                  "revision_number", "dt_created", "dt_start", "dt_end"),
                                            y = names(.))) %>%
        dplyr::summarise_all(dplyr::last) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(dt_created))
    }
  } else {
    warning("Table has not cleaned, only the 'tidy_output' version\nof the outage table can be cleaned with this function!")
  }
  return(out_df)

}


#' Get outages for transmission units.
#'
#' @param in_domain Energy Identification Code of the bidding zone area
#' @param out_domain Energy Identification Code of the bidding zone area
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_start_update Period start update, POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end_update Period end update, POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param doc_status Document status. "A05" for active, "A09" for cancelled and "A13" for withdrawn.
#' @param business_type Defaults to NULL. "A53" for planned maintenance. "A54" for unplanned outage.
#' @param tidy_output flatten nested tables
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  fr_de <- en_outages_transmission_infrastructure(in_domain = "10YFR-RTE------C",
#'                                                  out_domain = "10Y1001A1001A82H")
#'
en_outages_transmission_infrastructure <- function(in_domain,
                                                   out_domain,
                                                   period_start = lubridate::ymd(Sys.Date(), tz = "CET"),
                                                   period_end = lubridate::ymd(Sys.Date() + 3, tz = "CET"),
                                                   period_start_update = NULL,
                                                   period_end_update = NULL,
                                                   doc_status = "A05",
                                                   business_type = NULL,
                                                   tidy_output = TRUE,
                                                   security_token = Sys.getenv("ENTSOE_PAT")) {

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(length(in_domain) > 1){
    stop("This wrapper only supports one in_domain per request.")
  }
  if(length(out_domain) > 1){
    stop("This wrapper only supports one out_domain per request.")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A78",
    "&in_Domain=", in_domain,
    "&out_domain=", out_domain,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )
  if(!is.null(doc_status)){
    url <- paste0(url, "&docStatus=", doc_status)
  }
  if(!is.null(business_type)){
    url <- paste0(url, "&businessType=", business_type)
  }
  if(!is.null(period_start_update) & !is.null(period_end_update)){
    url <- paste0(url,
                  "&periodStartUpdate=", url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  en_content <- api_req_zip(url, file_type = "transmission")

  if(isTRUE(tidy_output) & nrow(en_content) > 0L){
    en_content <- outages_transmission_helper_tidy(en_content)
    en_content$type <- "transmission"
    en_content$doc_status <- doc_status

    # en_content <- en_content %>%
    #   dplyr::group_by(mrid, revision_number) %>%
    #   dplyr::filter(dt_created == max(dt_created, na.rm = TRUE)) %>%
    #   dplyr::summarise_all(dplyr::last) %>%
    #   dplyr::ungroup()
  }

  en_content
}



outages_transmission_helper <- function(x){

  # ap_not <- x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) != "Available_Period"]
  # ap_not <- tibble::as_tibble(lapply(ap_not, unlist, recursive = FALSE), .name_repair = "minimal")
  # ap_not$Reason <- NULL
  # ap_not$mRID <- NULL
  # ap_not$mkt_doc_mrid <- x$Unavailability_MarketDocument$mRID[[1]]
  # ap_not$mrid <- x$Unavailability_MarketDocument$mRID[[1]]
  # ap_not$asset_registered_resource_mrid <- ap_not$Asset_RegisteredResource$mRID[[1]]
  # ap_not$asset_registered_resource_name <- ap_not$Asset_RegisteredResource$name[[1]]
  # ap_not$asset_registered_resource_psrtype <- ap_not$Asset_RegisteredResource$asset_PSRType.psrType[[1]]
  # ap_not$asset_registered_resource_location_name <- ap_not$Asset_RegisteredResource$location.name[[1]]
  # ap_not$Asset_RegisteredResource <- NULL
  # ap_not$revisionNumber <- x$Unavailability_MarketDocument$revisionNumber[[1]]
  # ap_not$createdDateTime <- x$Unavailability_MarketDocument$createdDateTime[[1]]
  # ap_not$reason_code <- tryCatch(x$Unavailability_MarketDocument$Reason$code[[1]], error = function(error){return(as.character(NA))})
  # ap_not$reason_text <- tryCatch(paste(x$Unavailability_MarketDocument$Reason$text[[1]], collapse = " "), error = function(error){return(as.character(NA))})
  #
  # ap <- unname(x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) == "Available_Period"])
  #
  # start <- unlist(purrr::map(ap, ~.x$timeInterval$start[[1]]))
  # end <- unlist(purrr::map(ap, ~.x$timeInterval$end[[1]]))
  # resolution <- unlist(purrr::map(ap, ~.x$resolution))
  # position <- unlist(purrr::map(ap, ~.x$Point$position[[1]]))
  # quantity <- unlist(purrr::map(ap, ~.x$Point$quantity[[1]]))
  #
  # ap_not$available_period <- list(tibble::tibble(start, end, resolution, position, quantity))

  ## drilling one level deeper
  x                  <- x$Unavailability_MarketDocument

  ## picking main metadata of Unavailability_MarketDocument
  Reason             <- unlist(x$Reason)
  mkt_doc_mrid       <- x$mRID[[ 1L ]]
  revisionNumber     <- x$revisionNumber[[ 1L ]]
  createdDateTime    <- x$createdDateTime[[ 1L ]]

  ## drilling one level deeper
  x                  <- x$TimeSeries

  ## picking main metadata of TimeSeries branch
  ## and creating a tibble from those
  ap_not             <- x[ !names(x) %in% c("Available_Period", "Reason", "Asset_RegisteredResource") ]
  ap_not             <- lapply(X   = ap_not,
                               FUN = unlist,
                               recursive = FALSE) %>%
                            tibble::as_tibble(.name_repair = "minimal")

  ## unnesting Asset_RegisteredResource branch into a tibble
  arr                <- lapply(X   = x[ names(x) == "Asset_RegisteredResource" ],
                               FUN = unlist,
                               recursive = FALSE) %>%
                            dplyr::bind_rows()
  ## renaming the tibble columns according to our needs
  names(x = arr)     <- paste0("Asset_RegisteredResource.", names(x = arr))

  ## columnwise appending the Asset_RegisteredResource tibble to ap_not
  ap_not             <- ap_not %>% dplyr::bind_cols(arr)

  ## adding already collected meta data to the tibble as new columns
  ap_not$mkt_doc_mrid    <- mkt_doc_mrid
  ap_not$revisionNumber  <- revisionNumber
  ap_not$createdDateTime <- createdDateTime
  ap_not$Reason.code     <- unique(Reason[ names(Reason) == "code" ]) %>% sort()

  ## iterating Available_Period branches thru and appending the result tables together
  ap <- lapply(X   = x[ names(x) == "Available_Period" ],
               FUN = function(p) {
                  ## picking the start and end timestamps into variables
                  start_ts    <- as.POSIXct(p$timeInterval$start[[ 1L ]],
                                            tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                           "%Y-%m-%dT%H:%M:%SZ"),
                                            tz = "UTC")
                  end_ts      <- as.POSIXct(p$timeInterval$end[[ 1L ]],
                                            tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                           "%Y-%m-%dT%H:%M:%SZ"),
                                            tz = "UTC")
                  ## translating the resolution value into seconds
                  multip      <- data.table::fcase(p$resolution[[ 1L ]] == "PT60M",      60L*60L,
                                                   p$resolution[[ 1L ]] == "PT30M",      30L*60L,
                                                   p$resolution[[ 1L ]] == "PT15M",      15L*60L,
                                                   p$resolution[[ 1L ]] == "PT1M",        1L*60L,
                                                   p$resolution[[ 1L ]] == "P1D",    24L*60L*60L,
                                                   p$resolution[[ 1L ]] == "P7D", 7L*24L*60L*60L)
                  ## building tibble from the list of Point subvalues (position, quantity)
                  point_table <- lapply(X   = p[ names(x = p) == "Point" ],
                                        FUN = function(p) {
                                            tibble::as_tibble_row(unlist(p))
                                        }) %>%
                                   dplyr::bind_rows()
                  point_table$position <- as.numeric(point_table$position)
                  point_table$quantity <- as.numeric(point_table$quantity)
                  ## calculating start & end timestamp from start_ts, multip & end_ts values
                  point_table <- tibble::add_column(point_table,
                                                    start = start_ts + (point_table$position - 1L)*multip,
                                                    .before = "position")
                  point_table <- tibble::add_column(point_table,
                                                    end = data.table::shift(x = point_table$start, type = "lead"),
                                                    .before = "position")
                  point_table$end[ nrow(point_table) ] <- end_ts
                  ## adding corresponding resolution value
                  point_table <- tibble::add_column(point_table,
                                                    resolution = p$resolution[[ 1L ]],
                                                    .before = "position")
                  return(point_table)
                }) %>% dplyr::bind_rows()

  ## inserting available period table as nested table
  ap_not$available_period <- list(ap)

  return(ap_not)
}


outages_transmission_helper_tidy <- function(out_trans_df){

  out_trans_df <- out_trans_df %>%
    dplyr::mutate(dt_created = lubridate::ymd_hms(createdDateTime, tz = "UTC")) %>%
    dplyr::select(-createdDateTime) %>%
    dplyr::mutate(dt_start = lubridate::ymd_hm(paste0(start_DateAndOrTime.date, " ", stringr::str_sub(start_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-start_DateAndOrTime.time, -start_DateAndOrTime.date) %>%
    dplyr::mutate(dt_end = lubridate::ymd_hm(paste0(end_DateAndOrTime.date, " ", stringr::str_sub(end_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-end_DateAndOrTime.time, -end_DateAndOrTime.date) %>%
    dplyr::rename(in_domain_mrid = in_Domain.mRID,
                  out_domain_mrid = out_Domain.mRID,
                  quantity_measure_unit = quantity_Measure_Unit.name,
                  curve_type = curveType,
                  resource_location_name = Asset_RegisteredResource.location.name,
                  resource_mrid = Asset_RegisteredResource.mRID,
                  resource_name = Asset_RegisteredResource.name,
                  resource_psr_type = Asset_RegisteredResource.asset_PSRType.psrType,
                  revision_number = revisionNumber) %>%
                  # asset_name = asset_registered_resource_name,
                  # reason = reason_code) %>%
    dplyr::mutate(revision_number = as.integer(revision_number)) %>%
    # dplyr::arrange(asset_registered_resource_mrid, dt_start, dt_end) %>%
    dplyr::arrange(resource_psr_type, dt_start, resource_mrid) %>%
    # tidyr::unnest(available_period) %>%
    tidyr::unnest(data = ., cols = vapply(X = ., FUN = is.list, FUN.VALUE = TRUE) %>% which() %>% names()) %>%
    {
      reason_cols <- grep(pattern = "^Reason|^reason", x = names(.), value = TRUE);
      if(length(x = reason_cols) > 0L) {
        tidyr::unite(data = ., col = "reason_code", reason_cols, sep = "|", na.rm = TRUE)
      } else {
        .
      }
    }
    # dplyr::mutate(start = lubridate::ymd_hm(start, tz = "UTC"),
    #               end = lubridate::ymd_hm(end, tz = "UTC"),
    #               dt_created = lubridate::ymd_hms(dt_created, tz = "UTC"),
    #               position = as.integer(position),
    #               quantity = as.numeric(quantity))

  return(out_trans_df)
}

read_xml_from_path_out_gen <- function(xml_path){

  en_content <- lapply(X   = dir(xml_path, full.names = TRUE),
                       FUN = function(x){
                         xml_doc   <- xml2::read_xml(x)
                         xml_list  <- xml2::as_list(xml_doc)
                         xml_table <- outages_gen_helper(xml_list)
                         xml_table
                       }) %>%
                  dplyr::bind_rows()

  en_content
}

read_xml_from_path_out_tran <- function(xml_path){

  en_content <- lapply(X   = dir(xml_path, full.names = TRUE),
                       FUN = function(x){
                         xml_doc   <- xml2::read_xml(x)
                         xml_list  <- xml2::as_list(xml_doc)
                         xml_table <- outages_transmission_helper(xml_list)
                         xml_table
                       }) %>%
                  dplyr::bind_rows()

  en_content
}


#' save and unzip the zip file with xml data.
#'
#' @param url the pre-orchestrated URL of the API request
#' @param file_type "generation", "production" or "transmission"
#'
api_req_zip <- function(url, file_type){
  on.exit(try(fs::dir_delete(temp_file_path)))

  #temp_file_path <- paste0("~/temp-entsoe")
  temp_file_path <- fs::path(tempdir(), "temp-entsoe")

  folder_res <- api_zip_folder_prep(temp_file_path)

  req <- httr::GET(url, httr::write_disk(path = fs::path(temp_file_path, "file", ext = "zip"), overwrite = TRUE))

  if(httr::status_code(req) != "200"){

    req_cont_reason <- xml2::as_list(httr::content(req, encoding = "utf-8"))$Acknowledgement_MarketDocument$Reason$text[[1]]

    if(stringr::str_detect(req_cont_reason, "The amount of requested data exceeds allowed limit.")){
      docs_allowed <- as.integer(stringr::str_extract(stringr::str_extract(req_cont_reason, "allowed: [0-9]{1,8}"), "[0-9]{1,8}"))
      docs_requested <- as.integer(stringr::str_extract(stringr::str_extract(req_cont_reason, "requested: [0-9]{1,8}"), "[0-9]{1,8}"))

      total_api_reqs <- docs_requested%/%docs_allowed + ceiling(docs_requested %% docs_allowed / docs_allowed)

      folder_res <- api_zip_folder_prep(temp_file_path)

      res_list <- vector(mode = "list", total_api_reqs)

      message("Requested documents: ", docs_requested, ".")

      for(i in 1L:total_api_reqs){

        message("Request no: ", i, ". Total requests: ", total_api_reqs, "   '&offset=", (i - 1L) * docs_allowed, "'")

        url_offset <- paste0(url, "&offset=", (i - 1L) * docs_allowed)

        req <- httr::GET(url_offset, httr::write_disk(path = fs::path(temp_file_path, "file", ext = "zip"), overwrite = TRUE))

        if(httr::status_code(req) != 200){
          stop(xml2::as_list(httr::content(req, encoding = "utf-8"))$Acknowledgement_MarketDocument$Reason$text[[1]])
        }

        api_unzip_result <- api_unzip_res(temp_file_path)

        if(isTRUE(api_unzip_result)) {
          if(file_type == "generation"){
            res_list[[i]] <- read_xml_from_path_out_gen(xml_path = temp_file_path)
          } else if(file_type == "production"){
            res_list[[i]] <- read_xml_from_path_out_gen(xml_path = temp_file_path)
          } else if(file_type == "transmission"){
            res_list[[i]] <- read_xml_from_path_out_tran(xml_path = temp_file_path)
          }
        } else {
          warning("There was no XML file to consume in the response ZIP file.", call. = FALSE)
          res_list[[i]] <- tibble::tibble()
        }

        message(nrow(res_list[[ i ]]), " rows downloaded")
        folder_res <- api_zip_folder_prep(temp_file_path)

      }

      df <- dplyr::bind_rows(res_list)

    } else {
      stop(httr::content(req, encoding = "UTF-8"))
    }
  } else {

    api_unzip_result <- api_unzip_res(temp_file_path)

    if(isTRUE(api_unzip_result)) {
      if(file_type == "generation"){
        df <- read_xml_from_path_out_gen(xml_path = temp_file_path)
      } else if(file_type == "production"){
        df <- read_xml_from_path_out_gen(xml_path = temp_file_path)
      } else if(file_type == "transmission"){
        df <- read_xml_from_path_out_tran(xml_path = temp_file_path)
      }
    } else {
      warning("There was no XML file to consume in the response ZIP file.", call. = FALSE)
      df <- tibble::tibble()
    }
  }

  unique(df)
}

api_unzip_res <- function(temp_file_path){

  # unzip file
  unzip(zipfile = fs::path(temp_file_path, "file", ext = "zip"), exdir = temp_file_path)

  # setting a boolean return value
  # according to the number of unzipped xml files
  if(length(list.files(path = temp_file_path,
                          pattern = "xml",
                          ignore.case = TRUE)) > 0L) {
    api_unzip_result <- TRUE
  } else {
    api_unzip_result <- FALSE
  }

  # remove zip file so it's not read later
  if(fs::file_exists(fs::path(temp_file_path, "file", ext = "zip"))){
    fs::file_delete(fs::path(temp_file_path, "file", ext = "zip"))
  }

  return(api_unzip_result)
}

api_zip_folder_prep <- function(temp_file_path){

  if(fs::dir_exists(temp_file_path)) fs::dir_delete(temp_file_path)
  fs::dir_create(temp_file_path)
  if(fs::dir_exists(temp_file_path)){
    return(TRUE)
  } else {
    stop("Could not create dir.")
  }
}

