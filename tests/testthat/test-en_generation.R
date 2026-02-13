testthat::test_that(
  desc = "gen_installed_capacity_per_pt() works",
  code = {
    testthat::expect_no_error(
      object = gen_installed_capacity_per_pt(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      )
    )
    testthat::expect_no_error(
      object = gen_installed_capacity_per_pt(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        psr_type = "B01"
      )
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pt(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()) - 3.4,
        psr_type = NULL
      ),
      info = "One valid integer year value should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pt(
        eic = NULL,
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pt(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pt(
        eic = c("10YFR-RTE------C", "10YDE-VE-------2"),
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pt(
        eic = "10YFR-RTE------C",
        year = c(
          lubridate::year(x = Sys.Date()),
          lubridate::year(x = Sys.Date()) - 1L
        ),
        psr_type = NULL
      ),
      info = "One valid integer year value should be provided!"
    )
  }
)



testthat::test_that(
  desc = "gen_installed_capacity_per_pu() works",
  code = {
    testthat::expect_no_error(
      object = gen_installed_capacity_per_pu(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      )
    )
    testthat::expect_no_error(
      object = gen_installed_capacity_per_pu(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()) + 3L,
        psr_type = NULL
      )
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pu(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()) + 1.4,
        psr_type = NULL
      ),
      info = "One valid integer year value should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pu(
        eic = "10YFR-RTE------C",
        year = c(
          lubridate::year(x = Sys.Date()),
          lubridate::year(x = Sys.Date()) - 1L
        ),
        psr_type = NULL
      ),
      info = "One valid integer year value should be provided!"
    )
    testthat::expect_no_error(
      object = gen_installed_capacity_per_pu(
        eic = "10YFR-RTE------C",
        year = 1492,
        psr_type = NULL
      )
    ) |>
      testthat::expect_warning() |>
      testthat::expect_warning()
    testthat::expect_error(
      object = gen_installed_capacity_per_pu(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()) + 4L,
        psr_type = NULL
      ),
      info = paste(
        "Cannot be shown more than 3 years ahead",
        "as required by the law!"
      )
    )
    testthat::expect_no_error(
      object = gen_installed_capacity_per_pu(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        psr_type = "B01"
      )
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pu(
        eic = NULL,
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pu(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity_per_pu(
        eic = c("10YFR-RTE------C", "10YDE-VE-------2"),
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      ),
      info = "One control area EIC should be provided!"
    )
  }
)



testthat::test_that(
  desc = "gen_storage_mean_filling_rate() works",
  code = {
    testthat::expect_no_error(
      object = gen_storage_mean_filling_rate(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = gen_storage_mean_filling_rate(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd_hms(
          x = "2024-03-31 00:00:00",
          tz = "CET"
        ),
        period_end = lubridate::ymd_hms(
          x = "2024-03-31 16:00:00",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = gen_storage_mean_filling_rate(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = gen_storage_mean_filling_rate(
        eic = c("10YFR-RTE------C", "10YDE-VE-------2"),
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_storage_mean_filling_rate(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_storage_mean_filling_rate(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = gen_storage_mean_filling_rate(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2022-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "Maximum 380 days range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "gen_per_prod_type() works",
  code = {
    testthat::expect_no_error(
      object = gen_per_prod_type(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = gen_per_prod_type(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        gen_type = "B01",
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = gen_per_prod_type(
        eic = c("10YFR-RTE------C", "10YDE-VE-------2"),
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_per_prod_type(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_per_prod_type(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = gen_per_prod_type(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2022-03-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "gen_per_gen_unit() works",
  code = {
    testthat::expect_no_error(
      object = gen_per_gen_unit(
        eic = "10YDE-VE-------2",
        period_start = lubridate::ymd(
          x = "2020-01-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = gen_per_gen_unit(
        eic = "10YDE-VE-------2",
        period_start = lubridate::ymd(
          x = "2020-01-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
          tz = "CET"
        ),
        gen_type = c("B04", "B05"),
        tidy_output = TRUE
      )
    )
    testthat::expect_warning(
      object = gen_per_gen_unit(
        eic = "10YDE-VE-------2",
        period_start = lubridate::ymd(
          x = "2020-01-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
          tz = "CET"
        ),
        gen_type = c("B03"),
        tidy_output = TRUE
      ),
      info = "No additional type names added!"
    ) |>
      testthat::expect_warning(info = "No additional eic names added!") |>
      testthat::expect_warning(info = "No additional type names added!") |>
      testthat::expect_warning(info = "No additional eic names added!")
    testthat::expect_error(
      object = gen_per_gen_unit(
        eic = "10YDE-VE-------2",
        period_start = lubridate::ymd(
          x = "2020-01-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = gen_per_gen_unit(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2020-01-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_per_gen_unit(
        eic = c("10YDE-VE-------2", "10YFR-RTE------C"),
        period_start = lubridate::ymd(
          x = "2020-01-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = gen_per_gen_unit(
        eic = "10YDE-VE-------2",
        period_start = lubridate::ymd(
          x = "2020-01-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        gen_type = NULL,
        tidy_output = TRUE,
        security_token = NULL
      ),
      info = "alid security token should be provided!"
    )
  }
)



testthat::test_that(
  desc = "gen_day_ahead_forecast() works",
  code = {
    testthat::expect_no_error(
      object = gen_day_ahead_forecast(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = gen_day_ahead_forecast(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = gen_day_ahead_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One control area/bidding zone/country EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_day_ahead_forecast(
        eic = c("10YFR-RTE------C", "10YDE-VE-------2"),
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one EIC per request!"
    )
    testthat::expect_error(
      object = gen_day_ahead_forecast(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2021-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "gen_wind_solar_forecasts() works",
  code = {
    testthat::expect_no_error(
      object = gen_wind_solar_forecasts(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = gen_wind_solar_forecasts(
        eic = c("10YFR-RTE------C", "10YDE-VE-------2"),
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one EIC per request!"
    )
    testthat::expect_error(
      object = gen_wind_solar_forecasts(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One control area/bidding zone/country EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_wind_solar_forecasts(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = gen_wind_solar_forecasts(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2021-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)
