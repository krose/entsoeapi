
#' Get outages for Generation units.
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param period_start_update Period start udpate.
#' @param period_end_update Period end update.
#' @param doc_status Document status. A05 for active or A09 for Cancelled.
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
                       doc_status = "A05", tidy_output = TRUE, security_token = NULL){

  en_df_gen <- try(en_outages_generation_units(eic = eic,
                                               period_start = period_start,
                                               period_end = period_end,
                                               period_start_update = period_start_update,
                                               period_end_update = period_end_update, doc_status = doc_status))
  en_df_pro <- try(en_outages_production_units(eic = eic,
                                               period_start = period_start,
                                               period_end = period_end,
                                               period_start_update = period_start_update,
                                               period_end_update = period_end_update, doc_status = doc_status))

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
                                        doc_status = "A05", tidy_output = TRUE, security_token = NULL){
  on.exit(try(unlink(xml_path, recursive = TRUE)))

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
    "&businessType=A53",
    "&biddingZone_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )
  if(!is.null(doc_status)){
    url <- paste0(url, "&docStatus=", doc_status)
  }
  if(!is.null(period_start_update) & !is.null(period_end_update)){
    url <- paste0(url, "&periodStartUpdate=",url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  xml_path <- api_req_zip(url)

  en_content <- lapply(dir(xml_path, full.names = TRUE),
                       function(x){
                         xml_file <- xml2::read_xml(x)
                         xml_file <- xml2::as_list(xml_file)
                         xml_file
                       })

  en_content <- dplyr::bind_rows(lapply(en_content, outages_gen_helper))

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
                                        doc_status = "A05", tidy_output = TRUE, security_token = NULL){
  on.exit(try(unlink(xml_path, recursive = TRUE)))

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
    "&businessType=A53",
    "&biddingZone_Domain=", eic,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )
  if(!is.null(doc_status)){
    url <- paste0(url, "&docStatus=", doc_status)
  }
  if(!is.null(period_start_update) & !is.null(period_end_update)){
    url <- paste0(url, "&periodStartUpdate=", url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  xml_path <- api_req_zip(url)

  en_content <- lapply(dir(xml_path, full.names = TRUE),
                       function(x){
                         xml_file <- xml2::read_xml(x)
                         xml_file <- xml2::as_list(xml_file)
                         xml_file
                       })

  en_content <- dplyr::bind_rows(lapply(en_content, outages_gen_helper))

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
    dplyr::mutate(revision_number = as.integer(revision_number)) %>%
    dplyr::arrange(resource_psr_type, dt_start, dt_end) %>%
    tidyr::unnest(available_period) %>%
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
    tidyr::unnest(available_period) %>%
    dplyr::mutate(resource_psr_type_capacity = as.integer(resource_psr_type_capacity),
                  quantity = as.numeric(quantity),
                  start = lubridate::ymd_hm(start, tz = "UTC"),
                  end = lubridate::ymd_hm(end, tz = "UTC"))

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
    dplyr::filter((end - start) > 59) %>%
    dplyr::select(resource_psr_type, resource_psr_type_mrid, resource_psr_type_name, resource_psr_type_capacity, revision_number, resolution, dt_created, start, end, quantity)

  out_gen_df$ts <- lapply(seq_along(out_gen_df$resource_psr_type),
                          function(x){dt_seq_helper(out_gen_df$start[x], out_gen_df$end[x], out_gen_df$resolution[x], out_gen_df$quantity[x])})

  out_gen_df <-
    out_gen_df %>%
    tidyr::unnest(ts)

  out_gen_df
}

outages_gen_helper <- function(x){

  ap_not <- x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) != "Available_Period"]
  ap_not <- tibble::as_tibble(lapply(ap_not, unlist, recursive = FALSE), .name_repair = "minimal")
  ap_not$Reason <- NULL
  ap_not$revisionNumber <- x$Unavailability_MarketDocument$revisionNumber[[1]]
  ap_not$createdDateTime <- x$Unavailability_MarketDocument$createdDateTime[[1]]

  ap <- unname(x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) == "Available_Period"])

  start <- unlist(purrr::map(ap, ~.x$timeInterval$start[[1]]))
  end <- unlist(purrr::map(ap, ~.x$timeInterval$end[[1]]))
  resolution <- unlist(purrr::map(ap, ~.x$resolution))
  position <- unlist(purrr::map(ap, ~.x$Point$position[[1]]))
  quantity <- unlist(purrr::map(ap, ~.x$Point$quantity[[1]]))

  ap_not$available_period <- list(tibble::tibble(start, end, resolution, position, quantity))

  ap_not
}


#' Clean data for outages
#'
#' @param out_df Outages data.frame
#'
#' @export
#'
en_outages_clean <- function(out_df){

  out_df <-
    out_df %>%
    dplyr::group_by(resource_mrid, resource_name, resource_location_name, resource_psr_type,
                    resource_psr_type_capacity, resource_psr_type_mrid, resource_psr_type_name,
                    revision_number, resource_psr_type_capacity, dt_created, dt_start, dt_end) %>%
    dplyr::summarise_all(dplyr::last) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(dt_created))

  out_df
}


#' Get outages for transmission units.
#'
#' @param eic Energy Identification Code
#' @param period_start POSIXct
#' @param period_end POSIXct
#' @param period_start_update Period start udpate.
#' @param period_end_update Period end update.
#' @param doc_status Document status. A05 for active or A09 for Cancelled.
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
                                        doc_status = "A05", tidy_output = TRUE, security_token = NULL){
  on.exit(try(unlink(xml_path, recursive = TRUE)))

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if(is.null(security_token)){
    security_token <- Sys.getenv("ENTSOE_PAT")
  }

  url <- paste0(
    "https://transparency.entsoe.eu/api",
    "?documentType=A78",
    "&businessType=A53",
    "&in_Domain=", in_domain,
    "&out_domain=", out_domain,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )
  if(!is.null(doc_status)){
    url <- paste0(url, "&docStatus=", doc_status)
  }
  if(!is.null(period_start_update) & !is.null(period_end_update)){
    url <- paste0(url, "&periodStartUpdate=", url_posixct_format(period_start_update),
                  "&periodEndUpdate=", url_posixct_format(period_end_update))
  }

  xml_path <- api_req_zip(url)

  en_content <- lapply(dir(xml_path, full.names = TRUE),
                       function(x){
                         xml_file <- xml2::read_xml(x)
                         xml_file <- xml2::as_list(xml_file)
                         xml_file
                       })

  en_content <- dplyr::bind_rows(lapply(en_content, outages_transmission_helper))

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
