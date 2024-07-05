

## converter from list tree to table
ts_agg_gen_helper <- function(ts, tidy_output = FALSE, mktpsr_add = TRUE) {

  ## extract the content of "Period" subbranch
  points <- ts$Period[names(ts$Period) == "Point"] |>
    purrr::map(~unlist(.x) |> tibble::as_tibble_row()) |>
    purrr::list_rbind() |>
    tibble::as_tibble()
  start_date_time <- dt_helper(tz_start = unlist(ts$Period$timeInterval$start, use.names = FALSE),
                               tz_resolution = unlist(ts$Period$resolution, use.names = FALSE),
                               tz_position = points$position)
  resolution <- unlist(ts$Period[names(ts$Period) == "resolution"], use.names = FALSE)
  points <- points |>
    tibble::add_column(StartDateTime = start_date_time, resolution = resolution)
  points <- ts$Period[names(ts$Period) == "timeInterval"] |>
    purrr::map(~unlist(.x) |> tibble::as_tibble_row()) |>
    purrr::list_rbind() |>
    dplyr::rename_with(~paste0("timeInterval.", .x)) |>
    dplyr::bind_cols(points) |>
    dplyr::rename_with(~paste0("TimeSeries.Period.", .x))

  ## extract the content of "MktPSRType" subbranch if possible
  if (!is.null(ts[["MktPSRType"]])) {
    mktpsrtype <- tibble::tibble(psrType = unlist(ts$MktPSRType$psrType))
    if (mktpsr_add) {
      mktpsr <- tibble::as_tibble_row(unlist(ts$MktPSRType$PowerSystemResources))
      mktpsrtype <- dplyr::bind_cols(mktpsrtype, mktpsr)
    }
    mktpsrtype <- mktpsrtype |>
      dplyr::rename_with(~paste0("TimeSeries.PowerSystemResources.", .x))
  } else {
    mktpsrtype <- NULL
  }

  ## remove the "Period" and the"MktPSRType" subbranches
  ts$Period <- NULL
  ts$MktPSRType <- NULL

  ## convert the remaining parts to table
  ts <- purrr::map(ts, unlist) |> tibble::as_tibble_row() |>
    dplyr::rename_with(~paste0("TimeSeries.", .x))

  ## if we have "MktPSRType" data, then we add as a new column
  if (!is.null(mktpsrtype)) {
    if (nrow(mktpsrtype) > 0) {
      ts <- dplyr::bind_cols(ts, mktpsrtype)
    }
  }

  ## column-wise append the parts
  if (tidy_output) {
    res_tbl <- dplyr::bind_cols(ts, points)
  } else {
    res_tbl <- ts |>
      tibble::add_column(periods = list(points))
  }
  return(res_tbl)

}


## convert xml content to table
xml_to_table <- function(xml_content, tidy_output = FALSE) {

  # pick those children which length is below 3
  ridge_ind <- which(xml2::xml_children(xml_content) |> xml2::xml_length() <= 2)
  ridge_children <- xml2::xml_children(xml_content)[ridge_ind]

  # unpack each ridge child and bind them as columns
  ridge_tbl <- purrr::map(ridge_children,
                          unpack_xml, parent_name = NULL) |>
    dplyr::bind_cols()

  # drop the not necessary sender/reveiver market participant columns
  ridge_tbl <- ridge_tbl |>
    dplyr::select(!dplyr::starts_with(match = "sender_MarketParticipant")) |>
    dplyr::select(!dplyr::starts_with(match = "receiver_MarketParticipant"))

  # convert datetime-like columns to POSIXct and numeric-like columns to numeric
  try_formats <- c("%Y-%m-%dT%H:%MZ",
                   "%Y-%m-%dT%H:%M:%SZ")
  ridge_tbl <- ridge_tbl |>
    dplyr::mutate(dplyr::across(tidyselect::matches("time$|start$|end$"),
                                ~as.POSIXct(x = .x, tryFormats = try_formats, tz = "UTC"))) |>
    dplyr::mutate(dplyr::across(tidyselect::matches("number$|position$|quantity$|ts_mrid"),
                                ~as.numeric(x = .x)))

  # pick those children which length is above 2
  detailed_ind <- which(xml2::xml_children(xml_content) |> xml2::xml_length() > 2)
  detailed_children <- xml2::xml_children(xml_content)[detailed_ind]

  # unpack each detailed child and bind them as rows
  detailed_tbl <- purrr::map(detailed_children,
                             \(detailed_child) {
                               detailed_grand_children <- xml2::xml_children(detailed_child)
                               purrr::map(detailed_grand_children,
                                          \(detailed_grand_child) {
                                            detailed_child_name <- xml2::xml_name(detailed_child)
                                            if (endsWith(x = xml2::xml_name(detailed_grand_child), suffix = "Period")) {
                                              detailed_grand_grand_children <- xml2::xml_children(detailed_grand_child)

                                              # detect which child NOT contains data points
                                              not_point_ind <- which(xml2::xml_name(detailed_grand_grand_children) != "Point")
                                              np_tbl <- purrr::map(detailed_grand_grand_children[not_point_ind],
                                                                   unpack_xml, parent_name = detailed_child_name) |>
                                                dplyr::bind_cols()

                                              # rename data point columns to snakecase
                                              names(np_tbl) <- names(np_tbl) |>
                                                stringr::str_replace_all(pattern = "mRID", replacement = "mrid") |>
                                                stringr::str_replace_all(pattern = "TimeSeries", replacement = "ts") |>
                                                stringr::str_remove(pattern = "^process") |>
                                                snakecase::to_snake_case()

                                              # convert datetime-like columns to POSIXct
                                              np_tbl <- np_tbl |>
                                                dplyr::mutate(dplyr::across(tidyselect::matches("Time$|start$|end$"),
                                                                            ~as.POSIXct(x = .x, tryFormats = try_formats, tz = "UTC")))

                                              # detect which child contains data points
                                              point_ind <- which(xml2::xml_name(detailed_grand_grand_children) == "Point")
                                              p_tbl <- purrr::map(detailed_grand_grand_children[point_ind],
                                                                  unpack_xml, parent_name = detailed_child_name) |>
                                                dplyr::bind_rows()

                                              # rename data point columns to snakecase
                                              names(p_tbl) <- names(p_tbl) |>
                                                stringr::str_replace_all(pattern = "mRID", replacement = "mrid") |>
                                                stringr::str_replace_all(pattern = "TimeSeries", replacement = "ts") |>
                                                stringr::str_remove(pattern = "^process") |>
                                                snakecase::to_snake_case()

                                              # convert all data point columns to numeric
                                              p_tbl <- p_tbl |>
                                                dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

                                              if (tidy_output == TRUE) {
                                                p_tbl <- dt_seq_helper(from = np_tbl$ts_time_interval_start,
                                                                       to = np_tbl$ts_time_interval_end - lubridate::hours(x = 1),
                                                                       seq_resolution = np_tbl$ts_resolution,
                                                                       qty = p_tbl$ts_point_quantity)
                                                names(p_tbl) <- c("ts_point_dt_start", "ts_point_quantity")
                                              } else {
                                                p_tbl <- p_tbl |>
                                                  tidyr::nest(.key = "TimeSeries.Point")
                                              }
                                              return(dplyr::bind_cols(np_tbl, p_tbl))
                                            } else {
                                              tbl <- unpack_xml(section = detailed_grand_child, parent_name = detailed_child_name)

                                              # rename data point columns to snakecase
                                              names(tbl) <- names(tbl) |>
                                                stringr::str_replace_all(pattern = "mRID", replacement = "mrid") |>
                                                stringr::str_replace_all(pattern = "TimeSeries", replacement = "ts") |>
                                                stringr::str_remove(pattern = "^process") |>
                                                snakecase::to_snake_case()

                                              # convert numeric-like columns to POSIXct
                                              tbl <- tbl |>
                                                dplyr::mutate(dplyr::across(tidyselect::matches("ts_mrid$"),
                                                                            as.numeric))

                                              return(tbl)
                                            }
                                          }) |>
                                 dplyr::bind_cols()
                             }) |>
    dplyr::bind_rows()

  ## compose the result table from ridge and detailed tables
  ec <- dplyr::bind_cols(ridge_tbl, detailed_tbl)

  # rename columns to snakecase
  names(ec) <- names(ec) |>
    stringr::str_replace_all(pattern = "mRID", replacement = "mrid") |>
    stringr::str_replace_all(pattern = "TimeSeries", replacement = "ts") |>
    stringr::str_remove(pattern = "^process") |>
    snakecase::to_snake_case()

  return(ec)

}




#' Get Installed generation capacity - aggregated per type from Entso-e (14.1.A)
#'
#' @param eic Energy Identification Code of the control area, bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' fr_2020 <- en_generation_inst_gen_cap_agg(eic = "10YFR-RTE------C",
#'                                           year=2020)
#' View(fr_2020)
#'
en_generation_inst_gen_cap_agg <- function(eic,
                                           year,
                                           security_token = Sys.getenv("ENTSOE_PAT")) {
  ## check if only one eic provided
  if (!exists("eic")) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")

  ## check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  ## convert year to date
  period_start <- lubridate::ymd(paste0(year, "-01-01"), tz = "CET")
  period_end <- lubridate::ymd(paste0(year, "-01-02"), tz = "CET")

  ## convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  ## compose GET request url for the denoted year
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A68",
    "&processType=A33",
    "&in_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  ## send GET request
  en_cont <- api_req_safe(request_url)

  ## extract the possible failure reason
  reason <- en_cont$error$message

  ## if valid content got
  if (is.null(reason)) {

    ## convert xml to table
    ec <- xml_to_table(xml_content = en_cont$result, tidy_output = TRUE)

    ## add definitions to codes
    ec <- merge(x = ec,
                y = StandardProcessTypeList |>
                  dplyr::select(CODE, DEFINITION) |>
                  dplyr::rename(process_type = CODE,
                                process_type_def = DEFINITION),
                by = "process_type",
                all.x = TRUE)
    ec <- merge(x = ec,
                y = StandardDocumentTypeList |>
                  dplyr::select(CODE, DEFINITION) |>
                  dplyr::rename(type = CODE,
                                type_def = DEFINITION),
                by = "type",
                all.x = TRUE)
    ec <- merge(x = ec,
                y = StandardBusinessTypeList |>
                  dplyr::select(CODE, DEFINITION) |>
                  dplyr::rename(ts_business_type = CODE,
                                ts_business_type_def = DEFINITION),
                by = "ts_business_type",
                all.x = TRUE)
    ec <- merge(x = ec,
                y = StandardAssetTypeList |>
                  dplyr::select(CODE, DEFINITION) |>
                  dplyr::rename(ts_mkt_psr_type_psr_type = CODE,
                                ts_mkt_psr_type_psr_type_def = DEFINITION),
                by = "ts_mkt_psr_type_psr_type",
                all.x = TRUE)
    ec <- merge(x = ec,
                y = en_eic() |>
                  dplyr::select(AreaCode,
                                AreaName) |>
                  dplyr::rename(ts_in_bidding_zone_domain_mrid = AreaCode,
                                ts_in_bidding_zone_domain_name = AreaName) |>
                  dplyr::group_by(ts_in_bidding_zone_domain_mrid) |>
                  dplyr::mutate(ts_in_bidding_zone_domain_name = stringr::str_c(ts_in_bidding_zone_domain_name, collapse = " - ")) |>
                  dplyr::ungroup() |>
                  unique(),
                by = "ts_in_bidding_zone_domain_mrid",
                all.x = TRUE)
    ec <- tibble::as_tibble(ec)

    ## reorder columns and rows
    needed_cols <- c("ts_in_bidding_zone_domain_mrid", "ts_in_bidding_zone_domain_name",
                     "type", "type_def", "process_type", "process_type_def",
                     "ts_business_type", "ts_business_type_def",  "ts_mkt_psr_type_psr_type",
                     "ts_mkt_psr_type_psr_type_def", "created_date_time", "revision_number",
                     "ts_resolution", "ts_time_interval_start", "ts_time_interval_end", "ts_mrid",
                     "ts_point_dt_start", "ts_point_quantity",
                     "ts_quantity_measure_unit_name")
    needed_cols <- base::intersect(x = needed_cols,
                                   y = names(ec))
    result_tbl <- ec |>
      dplyr::select(dplyr::all_of(needed_cols))

    ## reorder the rows
    sort_cols <- base::intersect(x = c("ts_business_type", "ts_mkt_psr_type_psr_type",
                                       "ts_time_interval_start", "ts_point_dt_start"),
                                 y = names(ec))
    data.table::setorderv(x = result_tbl, cols = sort_cols)

  } else {

    stop(reason)
    result_tbl <- tibble::tibble()

  }

  return(result_tbl)

}



#' Get aggregated generation per type from Entso-e (16.1.B&C)
#'
#' @param eic Energy Identification Code of the control area, bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param gen_type Defaults to NULL, otherwise list of generation type codes from StandardAssetTypeList table
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
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
#'                                             period_end = lubridate::ymd("2020-03-01", tz = "CET"),
#'                                             gen_type       = NULL,
#'                                             tidy_output    = TRUE)
#' View(fr_202002)
#'
en_generation_agg_gen_per_type <- function(eic,
                                           period_start   = lubridate::ymd(Sys.Date() - 1L, tz = "CET"),
                                           period_end     = lubridate::ymd(Sys.Date(), tz = "CET"),
                                           gen_type       = NULL,
                                           tidy_output    = TRUE,
                                           security_token = Sys.getenv("ENTSOE_PAT")) {
  ## check if only one eic provided
  if (!exists("eic")) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")

  ## check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  ## convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  ## check if target period not longer than 1 year
  period_range <- difftime(time1 = strptime(x = period_end, format = "%Y%m%d%H%M", tz = "UTC") |> as.POSIXct(tz = "UTC"),
                           time2 = strptime(x = period_start, format = "%Y%m%d%H%M", tz = "UTC") |> as.POSIXct(tz = "UTC"),
                           units = "days")
  if (period_range > 366L) stop("One year range limit should be applied!")

  ## compose GET request url for a (maximum) 1 year long period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A75",               # <- Actual generation per type - A document providing the actual generation per generation type for a period.
    "&processType=A16",                # <- Realised - The process for the treatment of realised data as opposed to forecast data
    "&in_Domain=", eic,                # <- reflects Generation values
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    {if (!is.null(gen_type)) paste0("&psrType=", gen_type)},
    "&securityToken=", security_token
  )

  ## send GET request and extract result content and remove null contents
  en_cont <- unname(obj = request_url) |>
    purrr::map(api_req_safe) |>
    purrr::map("result") |>
    purrr::compact()

  ## extract possible failure reason
  reason <- en_cont |>
    purrr::map(xml2::as_list) |>
    purrr::map("Acknowledgement_MarketDocument") |>
    unlist(recursive = FALSE) |>
    purrr::pluck("Reason") |>
    unlist(recursive = FALSE)

  ## if valid contents left
  if (is.null(reason)) {

    ## compose a result table
    ec <- en_cont |>
      purrr::map(xml2::as_list) |>
      purrr::map("GL_MarketDocument") |>
      unlist(recursive = FALSE)
    main <- dplyr::bind_cols(tibble::tibble(document.mRID = unlist(ec[names(ec) == "mRID"],
                                                                   use.names = FALSE),
                                            revisionNumber = unlist(ec[names(ec) == "revisionNumber"],
                                                                    use.names = FALSE),
                                            process.processType = unlist(ec[names(ec) == "process.processType"],
                                                                         use.names = FALSE),
                                            createdDateTime = unlist(ec[names(ec) == "createdDateTime"],
                                                                     use.names = FALSE)),
                             dplyr::bind_rows(unlist(ec[names(ec) == "time_Period.timeInterval"])))
    ts <- ec[names(ec) == "TimeSeries"] |>
      purrr::map(ts_agg_gen_helper, tidy_output = tidy_output, mktpsr_add = FALSE) |>
      purrr::list_rbind()
    periodly <- dplyr::bind_cols(main, ts)

    ## if the output should be tidy
    if (tidy_output) {

      ## remove not necessary columns
      periodly <- periodly |>
        dplyr::select(-time_Period.timeInterval.start,
                      -time_Period.timeInterval.end)

      ## rename the columns
      names(periodly) <- names(periodly) |>
            stringr::str_replace_all(pattern = "\\.mRID", replacement = "_mrid") |>
            stringr::str_replace_all(pattern = "Type", replacement = "_type") |>
            stringr::str_replace_all(pattern = "revisionNumber", replacement = "revision_number") |>
            stringr::str_replace_all(pattern = "createdDateTime", replacement = "dt_created") |>
            stringr::str_replace_all(pattern = "objectAggregation", replacement = "object_aggregation") |>
            stringr::str_replace_all(pattern = "TimeSeries_mrid", replacement = "ts_mrid") |>
            stringr::str_replace_all(pattern = "TimeSeries\\.", replacement = "") |>
            stringr::str_replace_all(pattern = "quantity_Measure_Unit.name", replacement = "quantity_measure_unit") |>
            stringr::str_replace_all(pattern = "process\\.", replacement = "") |>
            stringr::str_replace_all(pattern = "PowerSystemResources\\.psr_type", replacement = "resource_psr_type") |>
            stringr::str_replace_all(pattern = "PowerSystemResources\\.", replacement = "resource_psr_type_") |>
            stringr::str_replace_all(pattern = "PowerSystemResources", replacement = "resource_psr_type") |>
            stringr::str_replace_all(pattern = "Period\\.", replacement = "") |>
            stringr::str_replace_all(pattern = "timeInterval\\.", replacement = "dt_") |>
            stringr::str_replace_all(pattern = "StartDateTime", replacement = "start") |>
            tolower()

      ## convert timestamp-like columns to timestamps
      ## add definitions to codes
      ## and reorder columns
      try_formats <- c("%Y-%m-%dT%H:%MZ", "%Y-%m-%dT%H:%M:%SZ")
      periodly <- periodly |>
        dplyr::mutate(dt_created = as.POSIXct(x = dt_created, tryFormats = try_formats, tz = "UTC"),
                      dt_start   = as.POSIXct(x = dt_start, tryFormats = try_formats, tz = "UTC"),
                      dt_end     = as.POSIXct(x = dt_end, tryFormats = try_formats, tz = "UTC")) |>
        merge(y = StandardProcessTypeList[, c("CODE", "DEFINITION")] |>
                dplyr::rename(process_type = CODE,
                              process_type_def = DEFINITION),
              by = "process_type",
              all.x = TRUE) |>
        merge(y = StandardBusinessTypeList[, c("CODE", "DEFINITION")] |>
                dplyr::rename(business_type = CODE,
                              business_type_def = DEFINITION),
              by = "business_type",
              all.x = TRUE) |>
        merge(y = StandardObjectAggregationTypeList[, c("CODE", "DEFINITION")] |>
                dplyr::rename(object_aggregation = CODE,
                              object_aggregation_def = DEFINITION),
              by = "object_aggregation",
              all.x = TRUE) |>
        merge(y = StandardCurveTypeList[, c("CODE", "DEFINITION")] |>
                dplyr::rename(curve_type = CODE,
                              curve_type_def = DEFINITION),
              by = "curve_type",
              all.x = TRUE) |>
        merge(y = StandardAssetTypeList[, c("CODE", "DEFINITION")] |>
                dplyr::rename(resource_psr_type = CODE,
                              resource_psr_type_def = DEFINITION),
              by = "resource_psr_type",
              all.x = TRUE)
      periodly <- periodly |>
        dplyr::select(base::intersect(x = c("process_type", "process_type_def",
                                            "curve_type", "curve_type_def",
                                            "object_aggregation", "object_aggregation_def",
                                            "business_type", "business_type_def",
                                            "document_mrid",
                                            "inbiddingzone_domain_mrid", "outbiddingzone_domain_mrid",
                                            "resource_mrid",
                                            "resource_psr_type", "resource_psr_type_def",
                                            "revision_number",
                                            "ts_mrid", "dt_created",
                                            "dt_start", "dt_end",
                                            "resolution", "position",
                                            "start", "quantity", "quantity_measure_unit"),
                                      y = names(periodly))) |>
        dplyr::arrange(dt_created, dt_start, start)

    }

    ## return with the periodly table
    return(tibble::as_tibble(periodly))

  } else {

    message(reason$code, ": ", reason$text)

    ## return with an empty table
    return(tibble::tibble())

  }
}



#' Get actual generation output per generation unit from Entso-e (16.1.A)
#'
#' @param eic Energy Identification Code of the control area or bidding zone
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format  One day range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format  One day range limit applies
#' @param gen_type Defaults to NULL, otherwise list of generation type codes from StandardAssetTypeList table
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' de_50Hertz_20200131 <- en_generation_act_gen_per_unit(eic = "10YDE-VE-------2",
#'                                                       period_start = lubridate::ymd("2020-01-31", tz = "CET"),
#'                                                       period_end = lubridate::ymd("2020-02-01", tz = "CET"),
#'                                                       gen_type = NULL,
#'                                                       tidy_output = TRUE)
#' View(de_50Hertz_20200131)
#'
en_generation_act_gen_per_unit <- function(eic,
                                           period_start = lubridate::ymd(Sys.Date() - 1L, tz = "CET"),
                                           period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
                                           gen_type = NULL,
                                           tidy_output = TRUE,
                                           security_token = Sys.getenv("ENTSOE_PAT")) {

  ## check if only one eic provided
  if (!exists("eic")) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")

  ## check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  ## convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  ## break time interval of period_start and period_end into 24 hour long parts
  to_time <- difftime(time1 = strptime(x = period_end, format = "%Y%m%d%H%M", tz = "UTC") |> as.POSIXct(tz = "UTC"),
                      time2 = strptime(x = period_start, format = "%Y%m%d%H%M", tz = "UTC") |> as.POSIXct(tz = "UTC"),
                      units = "days") |>
    ceiling() - 1L
  period_start_list <- strptime(x = period_start, format = "%Y%m%d%H%M", tz = "UTC") |>
    as.POSIXct(tz = "UTC") + seq(from = 0L, to = to_time) * 24L * 60L * 60L
  period_end_list <- data.table::shift(x    = period_start_list,
                                       type = "lead",
                                       fill = strptime(x = period_end, format = "%Y%m%d%H%M", tz = "UTC") |>
                                         as.POSIXct(tz = "UTC"))

  ## create list of to be combined parameter lists and remove empty combinations
  par_list <- list("psrType" = gen_type, "in_Domain" = eic) |>
    purrr::compact()

  ## create combination matrix
  par_matrix <- expand.grid(par_list,
                            stringsAsFactors = FALSE,
                            KEEP.OUT.ATTRS   = FALSE)

  ## convert timestamps into accepted format
  period_start_list <- url_posixct_format(period_start_list)
  period_end_list <- url_posixct_format(period_end_list)

  ## compose GET request url for a (maximum) 24 hours long period
  request_url_list <- seq_along(period_start_list) |>
    purrr::map(~paste0("https://web-api.tp.entsoe.eu/api",
                "?documentType=A73",               # <- Actual generation - A document providing the actual generation for a period.
                "&processType=A16",                # <- Realised - The process for the treatment of realised data as opposed to forecast data
                "&periodStart=", period_start_list[[.x]],
                "&periodEnd=", period_end_list[[.x]],
                {apply(X = unique(par_matrix),
                       MARGIN = 1L,
                       FUN = \(r) {
                         paste(vapply(X = colnames(par_matrix),
                                      FUN = \(cn) sprintf(fmt = "&%s=%s", cn, r[[cn]]),
                                      FUN.VALUE = "foo"),
                               collapse = "")
                       })},
                "&securityToken=", security_token))

  ## iterate (maximum) 24 hours long periods thru
  ## and append them into one tibble
  gnrtn <- request_url_list |>
    purrr::map_df(\(request_url) {

      ## send GET request and extract result content and remove null contents
      en_cont <- unname(obj = request_url) |>
        purrr::map(api_req_safe) |>
        purrr::map("result") |>
        purrr::compact()

      ## extract possible failure reason
      reason <- en_cont |>
        purrr::map(xml2::as_list) |>
        purrr::map("Acknowledgement_MarketDocument") |>
        unlist(recursive = FALSE) |>
        purrr::pluck("Reason") |>
        unlist(recursive = FALSE)

      ## if valid contents got
      if (is.null(reason)) {

        ## compose a result table
        ec <- en_cont |>
          purrr::map(xml2::as_list) |>
          purrr::map("GL_MarketDocument") |>
          unlist(recursive = FALSE)
        main <- c(tibble::tibble(document.mRID = unlist(ec[names(ec) == "mRID"],
                                                        use.names = FALSE),
                                 revisionNumber = unlist(ec[names(ec) == "revisionNumber"],
                                                         use.names = FALSE),
                                 process.processType = unlist(ec[names(ec) == "process.processType"],
                                                              use.names = FALSE),
                                 createdDateTime = unlist(ec[names(ec) == "createdDateTime"],
                                                          use.names = FALSE)),
                  dplyr::bind_rows(unlist(ec[names(ec) == "time_Period.timeInterval"]))) |>
          dplyr::bind_cols()
        ts <- ec[names(ec) == "TimeSeries"] |>
          purrr::map(ts_agg_gen_helper, tidy_output = tidy_output, mktpsr_add = TRUE) |>
          purrr::list_rbind()
        periodly <- dplyr::bind_cols(main, ts)

        ## if the output should be tidy
        if (tidy_output) {

          ## remove not necessary columns
          periodly <- periodly |>
            dplyr::select(-time_Period.timeInterval.start,
                          -time_Period.timeInterval.end)

          ## rename columns
          names(periodly) <- names(periodly) |>
              stringr::str_replace_all(pattern = "\\.mRID", replacement = "_mrid") |>
              stringr::str_replace_all(pattern = "Type", replacement = "_type") |>
              stringr::str_replace_all(pattern = "revisionNumber", replacement = "revision_number") |>
              stringr::str_replace_all(pattern = "createdDateTime", replacement = "dt_created") |>
              stringr::str_replace_all(pattern = "objectAggregation", replacement = "object_aggregation") |>
              stringr::str_replace_all(pattern = "registeredResource_mrid", replacement = "resource_mrid") |>
              stringr::str_replace_all(pattern = "TimeSeries_mrid", replacement = "ts_mrid") |>
              stringr::str_replace_all(pattern = "TimeSeries\\.", replacement = "") |>
              stringr::str_replace_all(pattern = "quantity_Measure_Unit\\.name", replacement = "quantity_measure_unit") |>
              stringr::str_replace_all(pattern = "process\\.", replacement = "") |>
              stringr::str_replace_all(pattern = "PowerSystemResources\\.psr_type", replacement = "resource_psr_type") |>
              stringr::str_replace_all(pattern = "PowerSystemResources\\.", replacement = "resource_psr_type_") |>
              stringr::str_replace_all(pattern = "PowerSystemResources", replacement = "resource_psr_type") |>
              stringr::str_replace_all(pattern = "Period\\.", replacement = "") |>
              stringr::str_replace_all(pattern = "timeInterval\\.", replacement = "dt_") |>
              stringr::str_replace_all(pattern = "StartDateTime", replacement = "start") |>
              tolower()

          ## convert timestamp-like columns to timestamps
          ## add definitions to codes
          periodly <- periodly |>
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
                                                  tz = "UTC")) |>
            merge(y = StandardProcessTypeList[, c("CODE", "DEFINITION")] |>
                    dplyr::rename(process_type = CODE,
                                  process_type_def = DEFINITION),
                  by = "process_type",
                  all.x = TRUE) |>
            merge(y = StandardBusinessTypeList[, c("CODE", "DEFINITION")] |>
                    dplyr::rename(business_type = CODE,
                                  business_type_def = DEFINITION),
                  by = "business_type",
                  all.x = TRUE) |>
            merge(y = StandardObjectAggregationTypeList[, c("CODE", "DEFINITION")] |>
                    dplyr::rename(object_aggregation = CODE,
                                  object_aggregation_def = DEFINITION),
                  by = "object_aggregation",
                  all.x = TRUE) |>
            merge(y = StandardCurveTypeList[, c("CODE", "DEFINITION")] |>
                    dplyr::rename(curve_type = CODE,
                                  curve_type_def = DEFINITION),
                  by = "curve_type",
                  all.x = TRUE) |>
            merge(y = StandardAssetTypeList[, c("CODE", "DEFINITION")] |>
                    dplyr::rename(resource_psr_type = CODE,
                                  resource_psr_type_def = DEFINITION),
                  by = "resource_psr_type",
                  all.x = TRUE)

          ## reorder columns
          periodly <- periodly |>
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
                                          y = names(periodly))) |>
            dplyr::arrange(dt_created, dt_start, start)
        }

        ## return with the periodly table
        return(tibble::as_tibble(periodly))

      } else {

        message(reason$code, ": ", reason$text)

        ## return with an empty table
        return(tibble::tibble())

      }
    })

  ## return with all the generation data
  return(gnrtn)
}



#' Get Day-ahead aggregated generation forecast from Entso-e. (14.1.C)
#' It is an estimate of the total scheduled generation per area for the following day
#'
#' @param eic Energy Identification Code of the control area, bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' fr_202002 <- en_generation_day_ahead_agg_gen(eic = "10YFR-RTE------C",
#'                                              period_start = lubridate::ymd("2020-02-01", tz = "CET"),
#'                                              period_end = lubridate::ymd("2020-03-01", tz = "CET"),
#'                                              tidy_output = TRUE)
#' View(fr_202002)
#'
en_generation_day_ahead_agg_gen <- function(eic,
                                            period_start,
                                            period_end,
                                            tidy_output = TRUE,
                                            security_token = Sys.getenv("ENTSOE_PAT")) {

  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  if (length(eic) > 1) {
    stop("This wrapper only supports one EIC per request.")
  }

  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A71",
    "&processType=A01",
    "&in_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  en_cont <- api_req_safe(request_url)
  en_cont <- xml2::as_list(en_cont$result)
  en_cont <- en_cont$GL_MarketDocument
  en_cont <- en_cont[names(en_cont) == "TimeSeries"]
  en_cont <- purrr::map(en_cont,
                        ts_agg_gen_helper, tidy_output = tidy_output, mktpsr_add = FALSE) |>
    dplyr::bind_rows()

  ## rename the columns
  names(en_cont) <- names(en_cont) |>
    stringr::str_replace_all(pattern = "TimeSeries\\.", replacement = "") |>
    stringr::str_replace_all(pattern = "\\.mRID", replacement = "_mrid") |>
    stringr::str_replace_all(pattern = "quantity_Measure_Unit.name", replacement = "quantity_measure_unit") |>
    stringr::str_replace_all(pattern = "Period\\.", replacement = "") |>
    stringr::str_replace_all(pattern = "StartDateTime", replacement = "start") |>
    tolower()

  ## reorder columns and keep only the needed ones
  needed_cols <- base::intersect(x = c("inbiddingzone_domain_mrid", "quantity_measure_unit",
                                       "periods", "start", "quantity", "resolution"),
                                 y = names(en_cont))
  en_cont <- dplyr::select(en_cont, all_of(needed_cols))

  return(en_cont)
}



#' Get Generation Forecasts for Wind and Solar from Entso-e (14.1.D)
#' It is a successor of en_generation_day_ahead_gen_forecast_ws() function.
#' From now on the the elements of the result list are representing the related market.
#'
#' @param eic Energy Identification Code of the control area, bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' fr_202002 <- en_generation_forecast_ws(eic = "10YFR-RTE------C",
#'                                        period_start = lubridate::ymd("2020-02-01", tz = "CET"),
#'                                        period_end = lubridate::ymd("2020-03-01", tz = "CET"),
#'                                        tidy_output = TRUE)
#' View(fr_202002$`Day-ahead`)
#' View(fr_202002$`Intraday`)
#' View(fr_202002$`Current`)
#'
en_generation_forecast_ws <- function(eic,
                                      period_start,
                                      period_end,
                                      tidy_output = TRUE,
                                      security_token = Sys.getenv("ENTSOE_PAT")) {

  ## check if only one eic provided
  if (!exists("eic")) stop("One control area/bidding zone/country EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one EIC per request.")

  ## check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided.")

  ## convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  ## compose GET request urls for a (minimum) 24 hours long period
  process_type <- c("Day-ahead" = "A01", "Intraday" = "A40", "Current" = "A18")
  request_url_list <- purrr::map(process_type,
                                 ~paste0("https://web-api.tp.entsoe.eu/api",
                                         "?documentType=A69",
                                         "&processType=", .x,
                                         "&in_Domain=", eic,
                                         "&periodStart=", period_start,
                                         "&periodEnd=", period_end,
                                         "&securityToken=", security_token))

  # iterate over request url list
  result_tbl_list <- purrr::map(request_url_list,
                                \(request_url) {

                                  ## send the GET request against the endpoint
                                  en_cont <- api_req_safe(request_url)

                                  ## extract the possible failure reason
                                  reason <- en_cont$error$message

                                  ## if valid content got
                                  if (is.null(reason)) {

                                    ## convert xml to table
                                    ec <- xml_to_table(xml_content = en_cont$result, tidy_output = tidy_output)

                                    ## add definitions to codes
                                    ec <- merge(x = ec,
                                                y = StandardProcessTypeList |>
                                                  dplyr::select(CODE, DEFINITION) |>
                                                  dplyr::rename(process_type = CODE,
                                                                process_type_def = DEFINITION),
                                                by = "process_type",
                                                all.x = TRUE)
                                    ec <- merge(x = ec,
                                                y = StandardDocumentTypeList |>
                                                  dplyr::select(CODE, DEFINITION) |>
                                                  dplyr::rename(type = CODE,
                                                                type_def = DEFINITION),
                                                by = "type",
                                                all.x = TRUE)
                                    ec <- merge(x = ec,
                                                y = StandardBusinessTypeList |>
                                                  dplyr::select(CODE, DEFINITION) |>
                                                  dplyr::rename(ts_business_type = CODE,
                                                                ts_business_type_def = DEFINITION),
                                                by = "ts_business_type",
                                                all.x = TRUE)
                                    ec <- merge(x = ec,
                                                y = StandardAssetTypeList |>
                                                  dplyr::select(CODE, DEFINITION) |>
                                                  dplyr::rename(ts_mkt_psr_type_psr_type = CODE,
                                                                ts_mkt_psr_type_psr_type_def = DEFINITION),
                                                by = "ts_mkt_psr_type_psr_type",
                                                all.x = TRUE)
                                    ec <- merge(x = ec,
                                                y = en_eic() |>
                                                  dplyr::select(AreaCode,
                                                                AreaName) |>
                                                  dplyr::rename(ts_in_bidding_zone_domain_mrid = AreaCode,
                                                                ts_in_bidding_zone_domain_name = AreaName) |>
                                                  dplyr::group_by(ts_in_bidding_zone_domain_mrid) |>
                                                  dplyr::mutate(ts_in_bidding_zone_domain_name = stringr::str_c(ts_in_bidding_zone_domain_name, collapse = " - ")) |>
                                                  dplyr::ungroup() |>
                                                  unique(),
                                                by = "ts_in_bidding_zone_domain_mrid",
                                                all.x = TRUE)
                                    ec <- tibble::as_tibble(ec)

                                    ## select and reorder the columns
                                    needed_cols <- c("ts_in_bidding_zone_domain_mrid", "ts_in_bidding_zone_domain_name",
                                                     "type", "type_def", "process_type", "process_type_def",
                                                     "ts_business_type", "ts_business_type_def",  "ts_mkt_psr_type_psr_type",
                                                     "ts_mkt_psr_type_psr_type_def", "created_date_time", "revision_number",
                                                     "ts_resolution", "ts_time_interval_start", "ts_time_interval_end", "ts_mrid",
                                                     "ts_point", "ts_point_dt_start", "ts_point_quantity",
                                                     "ts_quantity_measure_unit_name")
                                    needed_cols <- base::intersect(x = needed_cols,
                                                                   y = names(ec))
                                    result_tbl <- ec |>
                                      dplyr::select(dplyr::all_of(needed_cols))

                                    ## reorder the rows
                                    sort_cols <- base::intersect(x = c("ts_business_type", "ts_mkt_psr_type_psr_type",
                                                                       "ts_time_interval_start", "ts_point_dt_start"),
                                                                 y = names(ec))
                                    data.table::setorderv(x = result_tbl, cols = sort_cols)

                                    return(result_tbl)

                                  } else {

                                    warning(reason)
                                    return(tibble::tibble())

                                  }

                               })

  return(result_tbl_list)
}
