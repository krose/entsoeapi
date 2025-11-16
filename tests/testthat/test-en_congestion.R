testthat::test_that(
  desc = "redispatching_x_border() works",
  code = {
    testthat::expect_no_error(
      object = redispatching_x_border(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YDE-EON------1",
        period_start = lubridate::ymd(
          x = "2024-03-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-03-11",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = redispatching_x_border(
        eic_in = "10YNL----------L",
        eic_out = "10YNO-0--------C",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ) |>
        expect_warning() |>
        expect_warning()
    )
    testthat::expect_error(
      object = redispatching_x_border(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YDE-EON------1",
        period_start = lubridate::ymd(
          x = "2023-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-01-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = paste(
        "Provided time interval (...)) is larger than maximum ",
        "allowed period 'P1Y' for ",
        "'REDISPATCHING_CROSS_BORDER_R3:XML' export."
      )
    )
    testthat::expect_error(
      object = redispatching_x_border(
        eic_in = NULL,
        eic_out = "10YDE-EON------1",
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
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = redispatching_x_border(
        eic_in = "10YDE-VE-------2",
        eic_out = NULL,
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
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = redispatching_x_border(
        eic_in = c("10YNL----------L", "10YNO-0--------C"),
        eic_out = "10YDE-VE-------2",
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
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = redispatching_x_border(
        eic_in = "10YDE-VE-------2",
        eic_out = c("10YNL----------L", "10YNO-0--------C"),
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
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = redispatching_x_border(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YDE-EON------1",
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



testthat::test_that(
  desc = "redispatching_internal() works",
  code = {
    testthat::expect_no_error(
      object = redispatching_internal(
        eic = "10YNO-0--------C",
        period_start = lubridate::ymd(
          x = "2024-03-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-03-11",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = redispatching_internal(
        eic = "10YNO-0--------C",
        period_start = lubridate::ymd(
          x = "2023-05-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-04-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = redispatching_internal(
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
      object = redispatching_internal(
        eic = c("10YNL----------L", "10YNO-0--------C"),
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
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = redispatching_internal(
        eic = "10YNO-0--------C",
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



testthat::test_that(
  desc = "countertrading() works",
  code = {
    testthat::expect_no_error(
      object = countertrading(
        eic_in = "10YNL----------L",
        eic_out = "10YNO-0--------C",
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
    ) |>
      expect_warning() |>
      expect_warning()
    testthat::expect_no_error(
      object = countertrading(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YDE-EON------1",
        period_start = lubridate::ymd(
          x = "2024-03-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-08-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    ) |>
      testthat::expect_warning() |>
      testthat::expect_warning()
    testthat::expect_error(
      object = countertrading(
        eic_in = NULL,
        eic_out = "10YDE-EON------1",
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
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = countertrading(
        eic_in = "10YDE-VE-------2",
        eic_out = NULL,
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
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = countertrading(
        eic_in = c("10YNL----------L", "10YNO-0--------C"),
        eic_out = "10YDE-VE-------2",
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
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = countertrading(
        eic_in = "10YDE-VE-------2",
        eic_out = c("10YNL----------L", "10YNO-0--------C"),
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
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = countertrading(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YDE-EON------1",
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



testthat::test_that(
  desc = "costs_of_congestion_management() works",
  code = {
    testthat::expect_no_error(
      object = costs_of_congestion_management(
        eic          = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2017-01-01", tz = "CET"),
        tidy_output  = TRUE
      )
    ) |>
      testthat::expect_warning() |>
      testthat::expect_warning()
    testthat::expect_no_error(
      object = costs_of_congestion_management(
        eic          = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2017-01-01", tz = "CET"),
        event_nature = "A46",
        tidy_output  = TRUE
      )
    ) |>
      testthat::expect_warning() |>
      testthat::expect_warning()
    testthat::expect_error(
      object = costs_of_congestion_management(
        eic          = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2017-01-01", tz = "CET"),
        event_nature = "B99",
        tidy_output  = TRUE
      ),
      info = paste(
        "The 'event_nature' parameter should be",
        "'A46', 'B03', 'B04' or NULL!"
      )
    )
    testthat::expect_no_error(
      object = costs_of_congestion_management(
        eic = "10YNO-0--------C",
        period_start = lubridate::ymd(
          x = "2024-03-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-04-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_no_error(
      object = costs_of_congestion_management(
        eic = "10YNO-0--------C",
        period_start = lubridate::ymd(
          x = "2024-03-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-04-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = costs_of_congestion_management(
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
      object = costs_of_congestion_management(
        eic = c("10YNL----------L", "10YNO-0--------C"),
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
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = costs_of_congestion_management(
        eic = "10YNO-0--------C",
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
