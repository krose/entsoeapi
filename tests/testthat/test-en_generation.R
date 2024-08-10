testthat::test_that(
  desc = "gen_installed_capacity() works",
  code = {
    testthat::expect_no_error(
      object = gen_installed_capacity(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      )
    )
    testthat::expect_no_error(
      object = gen_installed_capacity(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        psr_type = "B01"
      )
    )
    testthat::expect_error(
      object = gen_installed_capacity(
        eic = NULL,
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity(
        eic = "10YFR-RTE------C",
        year = lubridate::year(x = Sys.Date()),
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = gen_installed_capacity(
        eic = c("10YFR-RTE------C", "10YDE-VE-------2"),
        year = lubridate::year(x = Sys.Date()),
        psr_type = NULL
      ),
      info = "One control area EIC should be provided!"
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
  desc = "gen_day_ahead() works",
  code = {
    testthat::expect_no_error(
      object = gen_day_ahead(
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
      object = gen_day_ahead(
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
      object = gen_day_ahead(
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
      object = gen_day_ahead(
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
  }
)

