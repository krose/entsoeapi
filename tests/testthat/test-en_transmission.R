testthat::test_that(
  desc = "cross_border_physical_flows() works",
  code = {
    testthat::expect_no_error(
      object = cross_border_physical_flows(
        eic_in = "10Y1001A1001A83F",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2020-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-01-02",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = cross_border_physical_flows(
        eic_in = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        eic_out = c("10YCZ-CEPS-----N", "10Y1001A1001A83F"),
        period_start = lubridate::ymd(
          x = "2020-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-01-02",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = cross_border_physical_flows(
        eic_in = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2020-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-01-02",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = cross_border_physical_flows(
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2020-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-01-02",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = cross_border_physical_flows(
        eic_in = "10Y1001A1001A83F",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2020-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-01-02",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = cross_border_physical_flows(
        eic_in = "10Y1001A1001A83F",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2020-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-01-02",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Unauthorized. Missing or invalid security token!"
    )
    testthat::expect_error(
      object = cross_border_physical_flows(
        eic_in = "10Y1001A1001A83F",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2020-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2021-01-02",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "day_ahead_commercial_sched() works",
  code = {
    testthat::expect_no_error(
      object = day_ahead_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = day_ahead_commercial_sched(
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = day_ahead_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = day_ahead_commercial_sched(
        eic_in = c("10YCZ-CEPS-----N", "10YSK-SEPS-----K"),
        eic_out = c("10YSK-SEPS-----K", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = day_ahead_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = day_ahead_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Unauthorized. Missing or invalid security token!"
    )
    testthat::expect_error(
      object = day_ahead_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "total_commercial_sched() works",
  code = {
    testthat::expect_no_error(
      object = total_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = total_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = total_commercial_sched(
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = total_commercial_sched(
        eic_in = c("10YCZ-CEPS-----N", "10YSK-SEPS-----K"),
        eic_out = c("10YSK-SEPS-----K", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = total_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = total_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Unauthorized. Missing or invalid security token!"
    )
    testthat::expect_error(
      object = total_commercial_sched(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "forecasted_transfer_capacities works",
  code = {
    testthat::expect_no_error(
      object = forecasted_transfer_capacities(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = forecasted_transfer_capacities(
        eic_in = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = forecasted_transfer_capacities(
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = forecasted_transfer_capacities(
        eic_in = c("10YCZ-CEPS-----N", "10YSK-SEPS-----K"),
        eic_out = c("10YSK-SEPS-----K", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = forecasted_transfer_capacities(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = forecasted_transfer_capacities(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Unauthorized. Missing or invalid security token!"
    )
    testthat::expect_error(
      object = forecasted_transfer_capacities(
        eic_in = "10YCZ-CEPS-----N",
        eic_out = "10YSK-SEPS-----K",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-12-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "redispatching_cross_border() works",
  code = {
    testthat::expect_no_error(
      object = redispatching_cross_border(
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
      object = redispatching_cross_border(
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
      object = redispatching_cross_border(
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
      info = "The number of instances (N) exceeds the allowed maximum (100)"
    )
    testthat::expect_error(
      object = redispatching_cross_border(
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
      object = redispatching_cross_border(
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
      object = redispatching_cross_border(
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
      object = redispatching_cross_border(
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
      object = redispatching_cross_border(
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
    testthat::expect_error(
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
      ),
      info = "The number of instances (N) exceeds the allowed maximum (100)"
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
        eic_in = "10YFR-RTE------C",
        eic_out = "10YES-REE------0",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-15",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = countertrading(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YES-REE------0",
        period_start = lubridate::ymd(
          x = "2024-03-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-08-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "The number of instances (N) exceeds the allowed maximum (100)"
    )
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
        eic          = "10YBE----------2",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2016-12-31", tz = "CET"),
        tidy_output  = TRUE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_no_error(
      object = costs_of_congestion_management(
        eic          = "10YBE----------2",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2016-12-31", tz = "CET"),
        event_nature = "A46",
        tidy_output  = TRUE
      )
    ) |>
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
      info = "This wrapper only supports one EIC per request!"
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
    testthat::expect_error(
      object = costs_of_congestion_management(
        eic          = "10YBE----------2",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2020-01-01", tz = "CET"),
        tidy_output  = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)
