

#' Get outages for Generation units.
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
#'  library(tidyverse)
#'  library(entsoeapi)
#'
#'  france <- en_outages_generation_units(eic = "10YFR-RTE------C", period_start = lubridate::ymd("2019-11-12", tz = "CET"), period_end = lubridate::ymd("2019-11-13", tz = "CET"))
#'
en_outages_generation_units <- function(eic, period_start, period_end, doc_status = "A05", security_token = NULL){
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

  xml_path <- api_req_zip(url)

  en_content <- lapply(dir(xml_path, full.names = TRUE),
                       function(x){
                         xml_file <- xml2::read_xml(x)
                         xml_file <- xml2::as_list(xml_file)
                         xml_file
                       })

  en_content <- dplyr::bind_rows(lapply(en_content, outages_gen_helper))

  en_content
}


outages_gen_helper <- function(x){

  ap_not <- x$Unavailability_MarketDocument$TimeSeries[names(x$Unavailability_MarketDocument$TimeSeries) != "Available_Period"]
  ap_not <- tibble::as_tibble(lapply(ap_not, unlist, recursive = FALSE))
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
