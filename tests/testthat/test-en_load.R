testthat::test_that(
  desc = "load_actual_total() works",
  code = {
    testthat::expect_no_error(
      object = load_actual_total(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = load_actual_total(
        eic = NULL,
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = load_actual_total(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = load_actual_total(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = load_actual_total(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 390),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "load_day_ahead_total_forecast() works",
  code = {
    testthat::expect_no_error(
      object = load_day_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = load_day_ahead_total_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = load_day_ahead_total_forecast(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request.!"
    )
    testthat::expect_error(
      object = load_day_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = load_day_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 390),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "load_week_ahead_total_forecast() works",
  code = {
    testthat::expect_no_error(
      object = load_week_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = load_week_ahead_total_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = load_week_ahead_total_forecast(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = load_week_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = load_week_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2018-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "load_month_ahead_total_forecast() works",
  code = {
    testthat::expect_no_error(
      object = load_month_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = load_month_ahead_total_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = load_month_ahead_total_forecast(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "his wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = load_month_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = load_month_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "load_year_ahead_total_forecast() works",
  code = {
    testthat::expect_no_error(
      object = load_year_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = load_year_ahead_total_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = load_year_ahead_total_forecast(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = load_year_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = load_year_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-11-30",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "load_year_ahead_forecast_margin() works",
  code = {
    testthat::expect_no_error(
      object = load_year_ahead_forecast_margin(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-12-31",
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = load_year_ahead_forecast_margin(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-12-31",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided.!"
    )
    testthat::expect_error(
      object = load_year_ahead_forecast_margin(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-12-31",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = load_year_ahead_forecast_margin(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2019-12-31",
          tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = load_year_ahead_forecast_margin(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-12-31",
          tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!!"
    )
  }
)
