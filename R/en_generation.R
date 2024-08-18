#' @title
#' Get Installed Generation Capacity per Production Type (14.1.A)
#'
#' @description
#' The sum of installed net generation capacity (MW) for all
#' existing production units equalling to or exceeding 1 MW
#' installed generation capacity, per production type.
#'
#' @param eic Energy Identification Code of the control area,
#'            bidding zone or country
#' @param psr_type Defaults to NULL, otherwise list of generation type
#'                 codes from asset_types table
#' @param year YYYY format
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' df <- gen_installed_capacity_per_pt(eic      = "10YFR-RTE------C",
#'                                     psr_type = "B05",
#'                                     year     = 2020)
#' str(df)
#'
gen_installed_capacity_per_pt <- function(
  eic = NULL,
  psr_type = NULL,
  year = lubridate::year(Sys.Date()),
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided!")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided!")

  # check if year is an integer number or not
  if (year %% 1 > 0) stop("A valid integer year value should be provided!")

  # convert year into the accepted format
  period_start <- paste0(year, "01010000")
  period_end <- paste0(year + 1L, "01010000")

  # compose GET request url for the denoted year
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A68",
    "&processType=A33",
    "&in_Domain=", eic,
    {if (!is.null(psr_type)) paste0("&psrType=", psr_type)},
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = TRUE))
}



#' @title
#' Get Installed Generation Capacity per Production Unit (14.1.B)
#'
#' @description
#' The installed generation capacities (MW) at the beginning of the year
#' for all the production units, including the planned ones.
#'
#' @param eic Energy Identification Code of the control area,
#'            bidding zone or country
#' @param psr_type Defaults to NULL, otherwise list of generation type
#'                 codes from asset_types table
#' @param year YYYY format Cannot be shown more than 3 years ahead
#'             as required by the law.
#' @param security_token Security token
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' df <- gen_installed_capacity_per_pu(eic      = "10YDE-VE-------2",
#'                                     year     = 2020,
#'                                     psr_type = "B05")
#' str(df)
#'
gen_installed_capacity_per_pu <- function(
  eic = NULL,
  year = lubridate::year(Sys.Date()),
  psr_type = NULL,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided!")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided!")

  # check if year is an integer number or not
  if (year %% 1 > 0) stop("A valid integer year value should be provided!")

  # check if year is within the legal limit
  if (year > lubridate::year(x = Sys.Date()) + 3L) {
    stop("Cannot be shown more than 3 years ahead as required by the law!")
  }

  # convert year into the accepted format
  period_start <- paste0(year, "01010000")
  period_end <- paste0(year + 1L, "01010000")

  # compose GET request url for the denoted year
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A71",
    "&processType=A33",
    "&in_Domain=", eic,
    {if (!is.null(psr_type)) paste0("&psrType=", psr_type)},
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = TRUE))
}



#' @title
#' Get Aggregated Generation per Production Type (16.1.B&C)
#'
#' @description
#' Actual aggregated net generation output (MW) or consumption
#' per market time unit and per production type.
#'
#' @param eic Energy Identification Code of the control area,
#'            bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     Maximum one year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   Maximum one year range limit applies
#' @param gen_type Defaults to NULL, otherwise list of generation type
#'                 codes from asset_types table
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df <- gen_per_prod_type(eic          = "10YFR-RTE------C",
#'                         period_start = ymd(x = "2020-02-01", tz = "CET"),
#'                         period_end   = ymd(x = "2020-03-01", tz = "CET"),
#'                         gen_type     = NULL,
#'                         tidy_output  = TRUE)
#' str(df)
#'
gen_per_prod_type <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  gen_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided!")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided!")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # check if target period not longer than 1 year
  period_range <- difftime(time1 = strptime(x = period_end,
                                            format = "%Y%m%d%H%M",
                                            tz = "UTC") |>
                             as.POSIXct(tz = "UTC"),
                           time2 = strptime(x = period_start,
                                            format = "%Y%m%d%H%M",
                                            tz = "UTC") |>
                             as.POSIXct(tz = "UTC"),
                           units = "days")
  if (period_range > 366L) stop("One year range limit should be applied!")

  # compose GET request url for a (maximum) 1 year long period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A75",
    "&processType=A16",
    "&in_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    {if (!is.null(gen_type)) paste0("&psrType=", gen_type)},
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Weekly Average Filling Rate of Water Reservoirs
#' and Hydro Storage Plants (16.1.D)
#'
#' @description
#' Aggregated weekly average filling rate of all water reservoir
#' and hydro storage plants (MWh) per area, including the same
#' week value of the previous year.
#'
#' @param eic Energy Identification Code of the control area,
#'            bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     Maximum 380 days range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   Maximum 380 days range limit applies
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df <- gen_storage_mean_filling_rate(eic          = "10YFR-RTE------C",
#'                                     period_start = ymd(x = "2020-02-01",
#'                                                        tz = "CET"),
#'                                     period_end   = ymd(x = "2021-02-15",
#'                                                        tz = "CET"),
#'                                     tidy_output  = TRUE)
#' str(df)
#'
gen_storage_mean_filling_rate <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided!")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided!")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # check if target period not longer than 1 year
  period_range <- difftime(time1 = strptime(x = period_end,
                                            format = "%Y%m%d%H%M",
                                            tz = "UTC") |>
                             as.POSIXct(tz = "UTC"),
                           time2 = strptime(x = period_start,
                                            format = "%Y%m%d%H%M",
                                            tz = "UTC") |>
                             as.POSIXct(tz = "UTC"),
                           units = "days")
  if (period_range > 380L) {
    stop("Maximum 380 days range limit should be applied!")
  }

  # compose GET request url for a (maximum) 1 year long period
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A72",
    "&processType=A16",
    "&in_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}


#' @title
#' Get Aggregated Generation per Generation Unit (16.1.A)
#'
#' @description
#' Actual net generation output (MW) and optionally
#' consumption data from all generation units.
#' Data are aggregated as an average of generation outputs
#' or consumption.
#'
#' @param eic Energy Identification Code of the control area
#'            or bidding zone
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param gen_type Defaults to NULL, otherwise list of generation type
#'                 codes from asset_types table
#' @param tidy_output Defaults to TRUE. If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df <- gen_per_gen_unit(eic          = "10YDE-VE-------2",
#'                        period_start = ymd(x = "2020-01-31", tz = "CET"),
#'                        period_end   = ymd(x = "2020-02-06", tz = "CET"),
#'                        gen_type     = NULL,
#'                        tidy_output  = TRUE)
#' str(df)
#'
gen_per_gen_unit <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  gen_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) stop("One control area EIC should be provided!")
  if (length(eic) > 1L) {
    stop("This wrapper only supports one control area EIC per request!")
  }

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided!")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # break time interval of period_start and period_end into 24 hour long parts
  to_time <- difftime(time1 = strptime(x = period_end,
                                       format = "%Y%m%d%H%M",
                                       tz = "UTC") |>
                        as.POSIXct(tz = "UTC"),
                      time2 = strptime(x = period_start,
                                       format = "%Y%m%d%H%M",
                                       tz = "UTC") |>
                        as.POSIXct(tz = "UTC"),
                      units = "days") |>
    ceiling() - 1L
  period_start_list <- as.POSIXct(x = period_start, format = "%Y%m%d%H%M",
                                  tz = "UTC") +
    seq(from = 0L, to = to_time) * 24L * 60L * 60L
  period_end_list <- data.table::shift(x    = period_start_list,
                                       type = "lead",
                                       fill = as.POSIXct(x = period_end,
                                                         format = "%Y%m%d%H%M",
                                                         tz = "UTC"))

  # convert timestamps into accepted format
  period_start_list <- url_posixct_format(period_start_list)
  period_end_list <- url_posixct_format(period_end_list)

  # create list of to be combined parameter lists and remove empty combinations
  par_list <- list("psrType" = gen_type, "in_Domain" = eic) |>
    purrr::compact()

  # create combination matrix dataframe
  par_matrix <- expand.grid(par_list,
                            stringsAsFactors = FALSE,
                            KEEP.OUT.ATTRS   = FALSE)

  # create the corresponding part of the request URL from the par matrix
  par_part <- par_matrix |>
    purrr::pmap(~list(...) |>
                  purrr::imap(~sprintf(fmt = "&%s=%s", .y, .x)) |>
                  paste(collapse = "")) |>
    unlist()

  # compose GET request url for a (maximum) 24 hours long period
  request_url_list <- seq_along(period_start_list) |>
    purrr::map(~paste0("https://web-api.tp.entsoe.eu/api",
                       "?documentType=A73",
                       "&processType=A16",
                       "&periodStart=", period_start_list[[.x]],
                       "&periodEnd=", period_end_list[[.x]],
                       par_part,
                       "&securityToken=", security_token))

  # iterate (maximum) 24 hours long periods thru
  # and append them into one tibble
  result_tbl_appended <- request_url_list |>
    purrr::map(\(request_url) {

      # send GET request
      en_cont_list <- api_req_safe(request_url)

      # return with the extracted the response
      extract_response(
        content = en_cont_list,
        tidy_output = tidy_output
      )

    }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  # return with all the generation data
  return(result_tbl_appended)

}



#' @title
#' Get Day-Ahead Generation Forecast. (14.1.C)
#'
#' @description
#' It is an estimate of the total scheduled net generation (MW)
#' per area and market time unit of the following day.
#'
#' @param eic Energy Identification Code of the control area,
#'            bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df <- gen_day_ahead(eic          = "10YFR-RTE------C",
#'                     period_start = ymd(x = "2020-02-01", tz = "CET"),
#'                     period_end   = ymd(x = "2020-03-01", tz = "CET"),
#'                     tidy_output  = TRUE)
#' str(df)
#'
gen_day_ahead <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 1L),
                                tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) {
    stop("One control area/bidding zone/country EIC should be provided!")
  }
  if (length(eic) > 1L) stop("This wrapper only supports one EIC per request!")

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided!")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request url
  request_url <- paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=A71",
    "&processType=A01",
    "&in_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

  # send GET request
  en_cont_list <- api_req_safe(request_url)

  # return with the extracted the response
  return(extract_response(content = en_cont_list, tidy_output = tidy_output))
}



#' @title
#' Get Generation Forecasts for Wind & Solar (14.1.D)
#'
#' @description
#' A respective forecast of wind and solar power net
#' generation (MW) per area and each market time unit
#' of the following/current day.
#' The elements of the result list are representing
#' each related forecast time range.
#'
#' @param eic Energy Identification Code of the control area,
#'            bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#' library(lubridate)
#'
#' df_list <- gen_wind_solar_forecasts(eic          = "10YFR-RTE------C",
#'                                     period_start = ymd(x = "2020-02-01",
#'                                                        tz = "CET"),
#'                                     period_end   = ymd(x = "2020-03-01",
#'                                                        tz = "CET"),
#'                                     tidy_output  = TRUE)
#' str(df_list$`Day-ahead`)
#' str(df_list$`Intraday`)
#' str(df_list$`Current`)
#'
gen_wind_solar_forecasts <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date(), tz = "CET") -
    lubridate::days(x = 1L),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  # check if only one eic provided
  if (is.null(eic)) {
    stop("One control area/bidding zone/country EIC should be provided!")
  }
  if (length(eic) > 1L) stop("This wrapper only supports one EIC per request!")

  # check if valid security token is provided
  if (security_token == "") stop("Valid security token should be provided!")

  # convert timestamps into accepted format
  period_start <- url_posixct_format(period_start)
  period_end <- url_posixct_format(period_end)

  # compose GET request urls for a (minimum) 24 hours long period
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

                                  # send the GET request against the endpoint
                                  en_cont_list <- api_req_safe(request_url)

                                  # return with the extracted the response
                                  extract_response(
                                    content = en_cont_list,
                                    tidy_output = tidy_output
                                  )

                                })

  # return with all the data
  return(result_tbl_list)
}
