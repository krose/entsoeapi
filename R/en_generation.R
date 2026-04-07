globalVariables(
  names = c(
    "periodEnd",
    "periodStart",
    "psrType"
  )
)


#' @title
#' Get Installed Generation Capacity per Production Type (14.1.A)
#'
#' @description
#' The sum of installed net generation capacity (MW) for all
#' existing production units equal to or exceeding 1 MW
#' installed generation capacity, per production type.
#'
#' @param eic Energy Identification Code of the control area,
#'            bidding zone or country
#' @param psr_type Defaults to NULL, otherwise list of generation type
#'                 codes from asset_types table
#' @param year YYYY format
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token
#'
#' @family generation endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd year
#' @importFrom checkmate assert_integerish
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::gen_installed_capacity_per_pt(
#'   eic = "10YFR-RTE------C",
#'   psr_type = "B05",
#'   year = 2020
#' )
#'
#' dplyr::glimpse(df)
#'
gen_installed_capacity_per_pt <- function(
  eic = NULL,
  psr_type = NULL,
  year = year(Sys.Date()),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  assert_integerish(year, len = 1L)

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A68",
      "&processType=A33",
      "&in_Domain=", eic,
      if (!is.null(psr_type)) paste0("&psrType=", psr_type),
      "&periodStart=", paste0(year, "01010000"),
      "&periodEnd=", paste0(year + 1L, "01010000")
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token
#'
#' @family generation endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd year
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::gen_installed_capacity_per_pu(
#'   eic = "10YDE-VE-------2",
#'   year = 2020,
#'   psr_type = "B05"
#' )
#'
#' dplyr::glimpse(df)
#'
gen_installed_capacity_per_pu <- function(
  eic = NULL,
  year = year(Sys.Date()),
  psr_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  assert_integerish(year, len = 1L)

  # check if year is within the legal limit
  if (year > year(x = Sys.Date()) + 3L) {
    cli_abort(
      "Cannot be shown more than 3 years ahead as required by the law!"
    )
  }

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A71",
      "&processType=A33",
      "&in_Domain=", eic,
      if (!is.null(psr_type)) paste0("&psrType=", psr_type),
      "&periodStart=", paste0(year, "01010000"),
      "&periodEnd=", paste0(year + 1L, "01010000")
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param gen_type Defaults to NULL, otherwise list of generation type
#'                 codes from asset_types table
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family generation endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::gen_per_prod_type(
#'   eic          = "10YFR-RTE------C",
#'   period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
#'   gen_type     = NULL,
#'   tidy_output  = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
gen_per_prod_type <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  gen_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A75",
      "&processType=A16",
      "&in_Domain=", eic,
      if (!is.null(gen_type)) paste0("&psrType=", gen_type),
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#' @family generation endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::gen_storage_mean_filling_rate(
#'   eic          = "10YFR-RTE------C",
#'   period_start = lubridate::ymd(x = "2020-01-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2020-01-31", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
gen_storage_mean_filling_rate <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 7L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "380 days"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A72",
      "&processType=A16",
      "&in_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
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
#' @family generation endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom dplyr mutate lead bind_rows
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::gen_per_gen_unit(
#'   eic          = "10YDE-VE-------2",
#'   period_start = lubridate::ymd(x = "2020-01-31", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2020-02-06", tz = "CET"),
#'   gen_type     = c("B04", "B05"),
#'   tidy_output  = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
gen_per_gen_unit <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  gen_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end
  )

  # break time interval of period_start into 24 hour long parts
  to_time <- difftime(
    time1 = as.POSIXct(x = period$end, format = "%Y%m%d%H%M", tz = "UTC"),
    time2 = as.POSIXct(x = period$start, format = "%Y%m%d%H%M", tz = "UTC"),
    units = "days"
  ) |>
    ceiling() - 1L
  period_start_list <- as.POSIXct(
    x = period$start,
    format = "%Y%m%d%H%M",
    tz = "UTC"
  ) + seq(from = 0L, to = to_time) * 24L * 60L * 60L

  # create a named list of generation types and remove every empty elements
  par_list <- list(
    "psrType" = gen_type,
    "periodStart" = period_start_list
  ) |>
    Filter(f = length)

  # create combination matrix dataframe
  par_matrix <- expand.grid(
    par_list,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS   = FALSE
  )

  # calculate period end for each period start
  par_matrix <- par_matrix |>
    mutate(
      periodEnd = lead(
        x = periodStart,
        default = as.POSIXct(x = period$end, format = "%Y%m%d%H%M", tz = "UTC")
      ),
      .by = if (!is.null(gen_type)) "psrType"
    )

  # convert the timestamps into accepted format
  par_matrix <- par_matrix |>
    mutate(
      periodStart = url_posixct_format(periodStart),
      periodEnd = url_posixct_format(periodEnd)
    )

  # create the corresponding part of the request URL from the par matrix
  # vectorised: paste each column as "&name=value",
  # then collapse across columns
  par_part <- do.call(
    what = paste0,
    args = lapply(
      names(par_matrix),
      \(nm) sprintf(fmt = "&%s=%s", nm, par_matrix[[nm]])
    )
  )

  # compose GET request URL list for a (maximum) 24 hours long period
  query_string_list <- paste0(
    "In_Domain=", eic,
    "&documentType=A73",
    "&processType=A16",
    par_part
  )

  # iterate (maximum) 24 hours long periods thru
  # and append them into one tibble
  lapply(
    query_string_list,
    \(query_string) {
      # send GET request and return with the extracted response
      run_api_query(
        query_string = query_string,
        security_token = security_token,
        tidy_output = tidy_output
      )
    }
  ) |>
    Filter(f = length) |>
    bind_rows()
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
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family generation endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::gen_day_ahead_forecast(
#'   eic          = "10YFR-RTE------C",
#'   period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
#'   tidy_output  = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
gen_day_ahead_forecast <- function(
  eic = NULL,
  period_start = ymd(Sys.Date() - days(x = 1L), tz = "CET"),
  period_end = ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A71",
      "&processType=A01",
      "&in_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}


#' @title
#' Get Generation Forecasts for Wind & Solar (14.1.D)
#'
#' @description
#' A respective forecast of wind and solar power net
#' generation (MW) per area and each market time unit
#' of the following/current day.
#'
#' @param eic Energy Identification Code of the control area,
#'            bidding zone or country
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format
#'                     One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format
#'                   One year range limit applies
#' @param process_type type of process
#'                     "A01" Day-ahead
#'                     "A18" Current
#'                     "A40" Intraday
#'                     Defaults to "A18"
#' @param tidy_output Defaults to TRUE.
#'                    If TRUE, then flatten nested tables.
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @family generation endpoints
#'
#' @return A [tibble::tibble()] with the queried data.
#'
#' @importFrom lubridate ymd days
#' @importFrom checkmate assert_choice
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' df <- entsoeapi::gen_wind_solar_forecasts(
#'   eic          = "10YFR-RTE------C",
#'   period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
#'   period_end   = lubridate::ymd(x = "2020-03-01", tz = "CET"),
#'   process_type = "A01",
#'   tidy_output  = TRUE
#' )
#'
#' dplyr::glimpse(df)
#'
gen_wind_solar_forecasts <- function(
  eic = NULL,
  period_start = ymd(Sys.Date(), tz = "CET") - days(x = 1L),
  period_end = ymd(Sys.Date(), tz = "CET"),
  process_type = "A18",
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  assert_eic(eic = eic)
  assert_choice(x = process_type, choices = c("A01", "A18", "A40"))
  check_sec_token(security_token = security_token)
  period <- check_period(
    period_start = period_start,
    period_end = period_end,
    period_length = "1 year"
  )

  # send GET request and return with the extracted response
  run_api_query(
    query_string = paste0(
      "documentType=A69",
      "&processType=", process_type,
      "&in_Domain=", eic,
      "&periodStart=", period$start,
      "&periodEnd=", period$end
    ),
    security_token = security_token,
    tidy_output = tidy_output
  )
}
