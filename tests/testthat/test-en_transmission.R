testthat::test_that(
  desc = "cross_border_physical_flows() validates inputs",
  code = {
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
      )
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
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "cross_border_physical_flows() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
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
    )
  }
)


testthat::test_that(
  desc = "day_ahead_commercial_sched() validates inputs",
  code = {
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
      )
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
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "day_ahead_commercial_sched() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
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
    )
  }
)


testthat::test_that(
  desc = "total_commercial_sched() validates inputs",
  code = {
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
      )
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
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "total_commercial_sched() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
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
    )
  }
)


testthat::test_that(
  desc = "forecasted_transfer_capacities validates inputs",
  code = {
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
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
      )
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
      )
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
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "forecasted_transfer_capacities works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
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
    )
  }
)


testthat::test_that(
  desc = "redispatching_cross_border() validates inputs",
  code = {
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
      )
    )
  }
)


testthat::test_that(
  desc = "redispatching_cross_border() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
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
      )
    )
  }
)


testthat::test_that(
  desc = "redispatching_internal() validates inputs",
  code = {
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
        tidy_output = TRUE,
        security_token = "dummy_token"
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
      )
    )
  }
)


testthat::test_that(
  desc = "redispatching_internal() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
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
  }
)


testthat::test_that(
  desc = "countertrading() validates inputs",
  code = {
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
      )
    )
  }
)


testthat::test_that(
  desc = "countertrading() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
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
  }
)


testthat::test_that(
  desc = "costs_of_congestion_management() validates inputs",
  code = {
    testthat::expect_error(
      object = costs_of_congestion_management(
        eic          = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2017-01-01", tz = "CET"),
        event_nature = "B99",
        tidy_output  = TRUE,
        security_token = "dummy_token"
      )
    )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
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
      )
    )
    testthat::expect_error(
      object = costs_of_congestion_management(
        eic          = "10YBE----------2",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2020-01-01", tz = "CET"),
        tidy_output  = TRUE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "costs_of_congestion_management() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(
      object = costs_of_congestion_management(
        eic          = "10YBE----------2",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2016-12-31", tz = "CET"),
        tidy_output  = TRUE
      )
    )
    testthat::expect_no_error(
      object = costs_of_congestion_management(
        eic          = "10YBE----------2",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end   = lubridate::ymd(x = "2016-12-31", tz = "CET"),
        event_nature = "A46",
        tidy_output  = TRUE
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
    )
  }
)


testthat::test_that(
  desc = "expansion_and_dismantling_project() validates inputs",
  code = {
    testthat::expect_error(
      object = expansion_and_dismantling_project(
        eic_in = NULL,
        eic_out = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2023-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-01-02",
          tz = "CET"
        ),
        business_type = "B01",
        doc_status = "A05",
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = expansion_and_dismantling_project(
        eic_in = "10YSK-SEPS-----K",
        eic_out = NULL,
        period_start = lubridate::ymd(
          x = "2023-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-01-02",
          tz = "CET"
        ),
        business_type = "B01",
        doc_status = "A05",
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = expansion_and_dismantling_project(
        eic_in = c("10YSK-SEPS-----K", "10YHU-MAVIR----U"),
        eic_out = c("10YHU-MAVIR----U", "10YSK-SEPS-----K"),
        period_start = lubridate::ymd(
          x = "2023-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-01-02",
          tz = "CET"
        ),
        business_type = "B01",
        doc_status = "A05",
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = expansion_and_dismantling_project(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2023-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-01-02",
          tz = "CET"
        ),
        business_type = "B01",
        doc_status = "A05",
        tidy_output = FALSE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = expansion_and_dismantling_project(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2023-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-01-02",
          tz = "CET"
        ),
        business_type = "B99",
        doc_status = "A05",
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = expansion_and_dismantling_project(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2023-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-01-02",
          tz = "CET"
        ),
        business_type = "B01",
        doc_status = "X99",
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "expansion_and_dismantling_project() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(
      object = expansion_and_dismantling_project(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2023-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-01-02",
          tz = "CET"
        ),
        business_type = "B01",
        doc_status = "A05",
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "intraday_cross_border_transfer_limits() validates inputs",
  code = {
    testthat::expect_error(
      object = intraday_cross_border_transfer_limits(
        eic_in = NULL,
        eic_out = "11Y0-0000-0265-K",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = intraday_cross_border_transfer_limits(
        eic_in = "10YFR-RTE------C",
        eic_out = NULL,
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = intraday_cross_border_transfer_limits(
        eic_in = c("10YFR-RTE------C", "11Y0-0000-0265-K"),
        eic_out = c("11Y0-0000-0265-K", "10YFR-RTE------C"),
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = intraday_cross_border_transfer_limits(
        eic_in = "10YFR-RTE------C",
        eic_out = "11Y0-0000-0265-K",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = intraday_cross_border_transfer_limits(
        eic_in = "10YFR-RTE------C",
        eic_out = "11Y0-0000-0265-K",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      )
    )
    testthat::expect_error(
      object = intraday_cross_border_transfer_limits(
        eic_in = "10YFR-RTE------C",
        eic_out = "11Y0-0000-0265-K",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "intraday_cross_border_transfer_limits() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(
      object = intraday_cross_border_transfer_limits(
        eic_in = "10YFR-RTE------C",
        eic_out = "11Y0-0000-0265-K",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "expansion_and_dismantling_project() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = expansion_and_dismantling_project(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YDE-VE-------2",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "intraday_cross_border_transfer_limits() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = intraday_cross_border_transfer_limits(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YDE-VE-------2",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "forecasted_transfer_capacities() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = forecasted_transfer_capacities(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YDE-VE-------2",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "day_ahead_commercial_sched() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = day_ahead_commercial_sched(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YDE-VE-------2",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "total_commercial_sched() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = total_commercial_sched(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YDE-VE-------2",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "cross_border_physical_flows() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = cross_border_physical_flows(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YDE-VE-------2",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "redispatching_cross_border() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = redispatching_cross_border(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YDE-VE-------2",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "redispatching_internal() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = redispatching_internal(
        eic = "10YFR-RTE------C",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "countertrading() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = countertrading(
        eic_in = "10YFR-RTE------C",
        eic_out = "10YDE-VE-------2",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "costs_of_congestion_management() covers happy path with mock",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Service Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = costs_of_congestion_management(
        eic = "10YFR-RTE------C",
        period = lubridate::ymd("2020-01-01", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)
