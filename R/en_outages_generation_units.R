
#' Get outages for Generation units.
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param period_start_update Period start udpate.
#' @param period_end_update Period end update.
#' @param doc_status Document status. A05 for active or A09 for Cancelled.
#' @param business_type Defaults to NULL. A53 for planned. A54 for unplanned.
#' @param security_token Security token
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
                       period_start = lubridate::ymd(Sys.Date(), tz = "CET"), period_end = lubridate::ymd(Sys.Date() + 3, tz = "CET"),
                       period_start_update = NULL, period_end_update = NULL,
                       doc_status = "A05", business_type = NULL, tidy_output = TRUE, security_token = NULL){

  en_df_gen <- try(en_outages_generation_units(eic = eic,
                                               period_start = period_start,
                                               period_end = period_end,
                                               period_start_update = period_start_update,
                                               period_end_update = period_end_update,
                                               doc_status = doc_status,
                                               business_type = business_type,
                                               security_token = security_token))
  en_df_pro <- try(en_outages_production_units(eic = eic,
                                               period_start = period_start,
                                               period_end = period_end,
                                               period_start_update = period_start_update,
                                               period_end_update = period_end_update,
                                               doc_status = doc_status,
                                               business_type = business_type,
                                               security_token = security_token))

  if(inherits(en_df_gen, "try-error")){
    message(paste("Outages Generation unit error. Try calling the function en_outages_generation_units() to see the error message."))
    en_df_gen <- NULL
  }

  if(inherits(en_df_pro, "try-error")){
    message(paste("Outages Production unit error. Try calling the function en_outages_production_units() to see the error message."))
    en_df_pro <- NULL
  }

  if(!is.null(en_df_gen) | !is.null(en_df_pro)){
    en_df <- dplyr::bind_rows(en_df_gen, en_df_pro)
    en_df <- dplyr::mutate(en_df, dt_created = lubridate::ymd_hms(dt_created, tz = "UTC"))
  } else {
    return(NULL)
  }

  en_df
}



#' Get outages for Generation units.
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param period_start_update Period start udpate.
#' @param period_end_update Period end update.
#' @param doc_status Document status. A05 for active or A09 for Cancelled.
#' @param business_type Defaults to NULL. A53 for planned. A54 for unplanned.
#' @param security_token Security token
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
en_outages_generation_units <- function(eic, period_start = lubridate::ymd(Sys.Date(), tz = "CET"),
                                        period_end = lubridate::ymd(Sys.Date() + 3, tz = "CET"),
                                        period_start_update = NULL, period_end_update = NULL,
                                        doc_status = "A05", business_type = NULL, tidy_output = TRUE, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

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
    url <- paste0(url, "&periodStartUpdate=",url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  en_content <- api_req_zip(url, "generation")

  if(tidy_output){
    en_content <- outages_gen_helper_tidy(en_content)
    en_content$type <- "generation units"
  }

  en_content
}


#' Get outages for production units.
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param period_start_update Period start udpate.
#' @param period_end_update Period end update.
#' @param doc_status Document status. A05 for active or A09 for Cancelled.
#' @param business_type Defaults to NULL. A53 for planned. A54 for unplanned.
#' @param security_token Security token
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
en_outages_production_units <- function(eic, period_start = lubridate::ymd(Sys.Date(), tz = "CET"),
                                        period_end = lubridate::ymd(Sys.Date() + 3, tz = "CET"),
                                        period_start_update = NULL, period_end_update = NULL,
                                        doc_status = "A05", business_type = NULL, tidy_output = TRUE, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  if(length(eic) > 1){
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
    url <- paste0(url, "&periodStartUpdate=", url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  en_content <- api_req_zip(url, "production")

  if(tidy_output){
    en_content <- outages_prod_helper_tidy(en_content)
    en_content$type <- "production units"
  }

  en_content
}



outages_gen_helper_tidy <- function(out_gen_df){

  out_gen_df <-
    out_gen_df %>%
    dplyr::mutate(dt_start = lubridate::ymd_hm(paste0(start_DateAndOrTime.date, " ", stringr::str_sub(start_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-start_DateAndOrTime.time) %>%
    dplyr::mutate(dt_end = lubridate::ymd_hm(paste0(end_DateAndOrTime.date, " ", stringr::str_sub(end_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-end_DateAndOrTime.time, -start_DateAndOrTime.date, -end_DateAndOrTime.date) %>%
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
                  revision_number = revisionNumber,
                  dt_created = createdDateTime) %>%
    dplyr::mutate(revision_number = as.integer(revision_number),
                  resource_psr_type_mrid = dplyr::if_else(is.na(resource_psr_type_mrid), "none", resource_psr_type_mrid),
                  resource_psr_type_name = dplyr::if_else(is.na(resource_psr_type_name), "none", resource_psr_type_name)) %>%
    dplyr::arrange(resource_psr_type, dt_start, dt_end) %>%
    tidyr::unnest( data = ., cols = vapply( ., is.list, TRUE ) %>% which() %>% names() ) %>%
    { if( "Reason" %in% names(.) ) tidyr::unite( data = ., col = "Reason", grep( pattern = "^Reason", x = names( . ), value = TRUE ), na.rm = TRUE, sep = "|" ) else . } %>%
    dplyr::mutate(resource_psr_type_capacity = as.integer(resource_psr_type_capacity),
                  quantity = as.numeric(quantity),
                  start = lubridate::ymd_hm(start, tz = "UTC"),
                  end = lubridate::ymd_hm(end, tz = "UTC"))

  out_gen_df
}

outages_prod_helper_tidy <- function(out_gen_df){

  out_gen_df <-
    out_gen_df %>%
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
                  revision_number = revisionNumber,
                  dt_created = createdDateTime) %>%
    dplyr::mutate(revision_number = as.integer(revision_number)) %>%
    dplyr::arrange(resource_psr_type, dt_start, dt_end) %>%
    tidyr::unnest( data = ., cols = vapply( ., is.list, TRUE ) %>% which() %>% names() ) %>%
    { if( "Reason" %in% names(.) ) tidyr::unite( data = ., col = "Reason", grep( pattern = "^Reason", x = names( . ), value = TRUE ), na.rm = TRUE, sep = "|" ) else . } %>%
    dplyr::mutate(resource_psr_type_capacity = as.integer(resource_psr_type_capacity),
                  quantity = as.numeric(quantity),
                  start = lubridate::ymd_hm(start, tz = "UTC"),
                  end = lubridate::ymd_hm(end, tz = "UTC"))

  if(!"resource_psr_type_mrid" %in% names(out_gen_df)){
    out_gen_df$resource_psr_type_mrid <- "none"
  } else {
    out_gen_df$resource_psr_type_mrid <- dplyr::if_else(is.na(out_gen_df$resource_psr_type_mrid), "none", out_gen_df$resource_psr_type_mrid)
  }

  if(!"resource_psr_type_name" %in% names(out_gen_df)){
    out_gen_df$resource_psr_type_name <- "none"
  } else {
    out_gen_df$resource_psr_type_name <- dplyr::if_else(is.na(out_gen_df$resource_psr_type_name), "none", out_gen_df$resource_psr_type_name)
  }

  out_gen_df
}

#' Convert the outages to an hourly timeseries.
#'
#' The function removes tidy observations less than 59 minutes.
#'
#' @param out_gen_df Tidy data.frame of outages.
#'
#' @export
#'
en_outages_tidy_to_ts <- function(out_gen_df){

  # filtrer mindre end 15 minutters beskeder
  out_gen_df <-
    out_gen_df %>%
    dplyr::filter((end - start) > 0.25) %>%
    dplyr::select(mRID, businessType, mkt_doc_mrid, resource_psr_type, resource_psr_type_mrid,
                  resource_psr_type_name, resource_psr_type_capacity, revision_number,
                  resolution, dt_created, start, end, quantity)

  out_gen_df$ts <- lapply(seq_along(out_gen_df$resource_psr_type),
                          function(x){dt_seq_helper(out_gen_df$start[x], out_gen_df$end[x], out_gen_df$resolution[x], out_gen_df$quantity[x])})

  out_gen_df <-
    out_gen_df %>%
    tidyr::unnest(ts) %>%
    dplyr::mutate(qty_outage = resource_psr_type_capacity - qty)

  out_gen_df
}

outages_gen_helper <- function(x){

  #ap_not <- x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) != "Available_Period"]
  ap_not <- x$Unavailability_MarketDocument$TimeSeries[!names(x$Unavailability_MarketDocument$TimeSeries) %in% c( "Available_Period", "Reason" )]
  ap_not <- tibble::as_tibble(lapply(ap_not, unlist, recursive = FALSE), .name_repair = "minimal")
  #ap_not$Reason <- NULL
  rsn_outer <- x$Unavailability_MarketDocument[ names( x$Unavailability_MarketDocument ) == "Reason" ]
  rsn_inner <- x$Unavailability_MarketDocument$TimeSeries[ names( x$Unavailability_MarketDocument$TimeSeries ) == "Reason" ]
  rsn       <- unlist( c(  rsn_outer, rsn_inner ) )
  rsn       <- unique( rsn[ names( rsn ) == "Reason.code" ] ) %>% sort()
  ap_not$mkt_doc_mrid <- x$Unavailability_MarketDocument$mRID[[1]]
  ap_not$revisionNumber <- x$Unavailability_MarketDocument$revisionNumber[[1]]
  ap_not$createdDateTime <- x$Unavailability_MarketDocument$createdDateTime[[1]]

  #ap <- unname(x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) == "Available_Period"])
  ap <- lapply( X   = x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) == "Available_Period"],
                FUN = function( uap ) {
                  tibble::as_tibble_row( x = unlist( uap ),
                                         .name_repair = function( n ) {
                                           sub( pattern = "^.+\\.",
                                                replacement = "",
                                                x = n,
                                                perl = TRUE )
                                         } ) %>%
                    .[ , 1L:5L ]
                } ) %>%
    dplyr::bind_rows()

  #start <- unlist(purrr::map(ap, ~.x$timeInterval$start[[1]]))
  #end <- unlist(purrr::map(ap, ~.x$timeInterval$end[[1]]))
  #resolution <- unlist(purrr::map(ap, ~.x$resolution))
  #position <- unlist(purrr::map(ap, ~.x$Point$position[[1]]))
  #quantity <- unlist(purrr::map(ap, ~.x$Point$quantity[[1]]))

  #ap_not$available_period <- list(tibble::tibble(start, end, resolution, position, quantity))
  ap_not$available_period <- list( ap )

  for( i in seq_along( rsn ) ) ap_not[[ paste0( "Reason.", i ) ]] <- rsn[[ i ]]

  ap_not
}


#' Clean data for outages
#'
#' @param out_df Outages data.frame
#'
#' @export
#'
en_outages_clean <- function(out_df){

  if("resource_mrid" %in% names(x = out_df)) {
      out_df <- out_df %>%
        dplyr::group_by(mkt_doc_mrid, resource_mrid, resource_name, resource_location_name, resource_psr_type,
                        resource_psr_type_capacity, resource_psr_type_mrid, resource_psr_type_name,
                        revision_number, dt_created, dt_start, dt_end) %>%
        dplyr::summarise_all(dplyr::last) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(dt_created))
    } else {
      warning("Table has not cleaned, only the 'tidy_output' version\nof the outage table can be cleaned with this function!")
    }
  return( out_df )

}


#' Get outages for transmission units.
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param period_start_update Period start udpate.
#' @param period_end_update Period end update.
#' @param doc_status Document status. A05 for active or A09 for Cancelled.
#' @param business_type Defaults to NULL. A53 for planned. A54 for unplanned.
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  fr_de <- df <- en_outages_transmission_infrastructure(in_domain = "10YFR-RTE------C", out_domain = "10Y1001A1001A82H", period_start = lubridate::ymd(Sys.Date() - 300, tz = "CET"))
#'
en_outages_transmission_infrastructure <- function(in_domain, out_domain, period_start = lubridate::ymd(Sys.Date(), tz = "CET"),
                                        period_end = lubridate::ymd(Sys.Date() + 3, tz = "CET"),
                                        period_start_update = NULL, period_end_update = NULL,
                                        doc_status = "A05", business_type = NULL, tidy_output = TRUE, security_token = NULL){

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
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
    url <- paste0(url, "&periodStartUpdate=", url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  en_content <- api_req_zip(url, "transmission")

  if(tidy_output){
    en_content <- outages_transmission_helper_tidy(en_content)
    en_content$type <- "transmission"

    en_content <-
      en_content %>%
      dplyr::group_by(mrid, revision_number) %>%
      dplyr::filter(dt_created == max(dt_created, na.rm = TRUE)) %>%
      dplyr::summarise_all(dplyr::last) %>%
      dplyr::ungroup()
  }

  en_content
}

outages_transmission_helper <- function(x){

  ap_not <- x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) != "Available_Period"]
  ap_not <- tibble::as_tibble(lapply(ap_not, unlist, recursive = FALSE), .name_repair = "minimal")
  ap_not$Reason <- NULL
  ap_not$mRID <- NULL
  ap_not$mkt_doc_mrid <- x$Unavailability_MarketDocument$mRID[[1]]
  ap_not$mrid <- x$Unavailability_MarketDocument$mRID[[1]]
  ap_not$asset_registered_resource_mrid <- ap_not$Asset_RegisteredResource$mRID[[1]]
  ap_not$asset_registered_resource_name <- ap_not$Asset_RegisteredResource$name[[1]]
  ap_not$asset_registered_resource_psrtype <- ap_not$Asset_RegisteredResource$asset_PSRType.psrType[[1]]
  ap_not$asset_registered_resource_location_name <- ap_not$Asset_RegisteredResource$location.name[[1]]
  ap_not$Asset_RegisteredResource <- NULL
  ap_not$revisionNumber <- x$Unavailability_MarketDocument$revisionNumber[[1]]
  ap_not$createdDateTime <- x$Unavailability_MarketDocument$createdDateTime[[1]]
  ap_not$reason_code <- tryCatch(x$Unavailability_MarketDocument$Reason$code[[1]], error = function(error){return(as.character(NA))})
  ap_not$reason_text <- tryCatch(paste(x$Unavailability_MarketDocument$Reason$text[[1]], collapse = " "), error = function(error){return(as.character(NA))})

  ap <- unname(x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) == "Available_Period"])

  start <- unlist(purrr::map(ap, ~.x$timeInterval$start[[1]]))
  end <- unlist(purrr::map(ap, ~.x$timeInterval$end[[1]]))
  resolution <- unlist(purrr::map(ap, ~.x$resolution))
  position <- unlist(purrr::map(ap, ~.x$Point$position[[1]]))
  quantity <- unlist(purrr::map(ap, ~.x$Point$quantity[[1]]))

  ap_not$available_period <- list(tibble::tibble(start, end, resolution, position, quantity))

  ap_not
}


outages_transmission_helper_tidy <- function(out_gen_df){

  out_gen_df <-
    out_gen_df %>%
    dplyr::mutate(dt_start = lubridate::ymd_hm(paste0(start_DateAndOrTime.date, " ", stringr::str_sub(start_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-start_DateAndOrTime.time, -start_DateAndOrTime.date) %>%
    dplyr::mutate(dt_end = lubridate::ymd_hm(paste0(end_DateAndOrTime.date, " ", stringr::str_sub(end_DateAndOrTime.time, 1, 5)), tz = "UTC")) %>%
    dplyr::select(-end_DateAndOrTime.time, -end_DateAndOrTime.date) %>%
    dplyr::rename(in_domain_mrid = in_Domain.mRID,
                  out_domain_mrid = out_Domain.mRID,
                  quantity_measure_unit = quantity_Measure_Unit.name,
                  curve_type = curveType,
                  revision_number = revisionNumber,
                  dt_created = createdDateTime) %>%
    dplyr::mutate(revision_number = as.integer(revision_number)) %>%
    dplyr::arrange(asset_registered_resource_mrid, dt_start, dt_end) %>%
    tidyr::unnest(available_period) %>%
    dplyr::mutate(start = lubridate::ymd_hm(start, tz = "UTC"),
                  end = lubridate::ymd_hm(end, tz = "UTC"),
                  dt_created = lubridate::ymd_hms(dt_created, tz = "UTC"),
                  position = as.integer(position),
                  quantity = as.numeric(quantity))

  out_gen_df
}

read_xml_from_path_out_gen <- function(xml_path){

  en_content <- lapply(dir(xml_path, full.names = TRUE),
                       function(x){
                         xml_file <- xml2::read_xml(x)
                         xml_file <- xml2::as_list(xml_file)
                         xml_file
                       })

  en_content <- dplyr::bind_rows(lapply(en_content, outages_gen_helper))

  en_content
}

read_xml_from_path_out_tran <- function(xml_path){

  en_content <- lapply(dir(xml_path, full.names = TRUE),
                       function(x){
                         xml_file <- xml2::read_xml(x)
                         xml_file <- xml2::as_list(xml_file)
                         xml_file
                       })

  en_content <- dplyr::bind_rows(lapply(en_content, outages_transmission_helper))

  en_content
}


#' save and unzip the zip file with xml data.
#'
#' @param url url.
#' @param file_type transmission or generation.
#'
api_req_zip <- function(url, file_type){
  on.exit(try(unlink(temp_file_path, recursive = TRUE)))

  temp_file_path <- paste0("~/temp-entsoe")

  folder_res <- api_zip_folder_prep(temp_file_path)

  req <- httr::GET(url, httr::write_disk(path = paste0(temp_file_path, "/file.zip"), overwrite = TRUE))

  if(httr::status_code(req) != "200"){

    req_cont_reason <- xml2::as_list(httr::content(req, encoding = "utf-8"))$Acknowledgement_MarketDocument$Reason$text[[1]]

    if(stringr::str_detect(req_cont_reason, "The amount of requested data exceeds allowed limit.")){
      docs_allowed <- as.integer(stringr::str_extract(stringr::str_extract(req_cont_reason, "allowed: [0-9]{1,8}"), "[0-9]{1,8}"))
      docs_requested <- as.integer(stringr::str_extract(stringr::str_extract(req_cont_reason, "requested: [0-9]{1,8}"), "[0-9]{1,8}"))

      total_api_reqs <- docs_requested%/%docs_allowed + ceiling( docs_requested %% docs_allowed / docs_allowed )

      folder_res <- api_zip_folder_prep(temp_file_path = temp_file_path)

      res_list <- vector(mode = "list", total_api_reqs)

      message("Requested documents: ", docs_requested, ".")

      for(i in 1L:total_api_reqs){

        message("Request no: ", i, ". Total requests: ", total_api_reqs, "   '&offset=", (i - 1L) * docs_allowed, "'" )

        url_offset <- paste0(url, "&offset=", (i - 1L) * docs_allowed )

        req <- httr::GET(url_offset, httr::write_disk(path = paste0(temp_file_path, "/file.zip"), overwrite = TRUE))

        if(httr::status_code(req) != 200){
          stop(xml2::as_list(httr::content(req, encoding = "utf-8"))$Acknowledgement_MarketDocument$Reason$text[[1]])
        }

        api_unzip_result <- api_unzip_res(temp_file_path)

        if(file_type == "generation"){
          res_list[[i]] <- read_xml_from_path_out_gen(xml_path = temp_file_path)
        } else if(file_type == "production"){
          res_list[[i]] <- read_xml_from_path_out_gen(xml_path = temp_file_path)
        } else if(file_type == "transmission"){
          res_list[[i]] <- read_xml_from_path_out_tran(xml_path = temp_file_path)
        }

        message( nrow( res_list[[ i ]] ), " rows downloaded" )
        folder_res <- api_zip_folder_prep(temp_file_path = temp_file_path)

      }

      df <- dplyr::bind_rows(res_list)

    } else {
      stop(httr::content(req, encoding = "UTF-8"))
    }
  } else {

    api_unzip_result <- api_unzip_res(temp_file_path)

    if(file_type == "generation"){
      df <- read_xml_from_path_out_gen(xml_path = temp_file_path)
    } else if(file_type == "production"){
      df <- read_xml_from_path_out_gen(xml_path = temp_file_path)
    } else if(file_type == "transmission"){
      df <- read_xml_from_path_out_tran(xml_path = temp_file_path)
    }
  }

  unique( df )
}

api_unzip_res <- function(temp_file_path){

  # unzip file
  unzip(zipfile = paste0(temp_file_path, "/file.zip"), exdir = temp_file_path)

  # setting a boolean return value
  # according to the number of unzipped xml files
  if( length( list.files( path = temp_file_path,
                          pattern = "xml",
                          ignore.case = TRUE ) ) > 0L ) {
    api_unzip_result <- TRUE
  } else {
    api_unzip_result <- FALSE
  }

  # remove zip file so it's not read later
  if(file.exists(paste0(temp_file_path, "/file.zip"))){
    file.remove(paste0(temp_file_path, "/file.zip"))
  }

  return( api_unzip_result )
}

api_zip_folder_prep <- function(temp_file_path){

  if(!dir.exists(temp_file_path)){
    dir.create(temp_file_path)
  } else {
    un_res <- unlink(x = temp_file_path, recursive = TRUE)
    if(un_res == 0){
      dir.create(temp_file_path)
    } else {
      stop("Could not create dir.")
    }
  }

  TRUE
}

