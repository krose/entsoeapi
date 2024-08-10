testthat::test_that(
  desc = "balancing_accepted_aggr_offers() works",
  code = {
    testthat::expect_no_error(
      object = balancing_accepted_aggr_offers(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = balancing_accepted_aggr_offers(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = balancing_accepted_aggr_offers(
        eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = balancing_accepted_aggr_offers(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2021-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = balancing_accepted_aggr_offers(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
  }
)



testthat::test_that(
  desc = "balancing_activated_reserves() works",
  code = {
    testthat::expect_no_error(
      object = balancing_activated_reserves(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = balancing_activated_reserves(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = balancing_activated_reserves(
        eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = balancing_activated_reserves(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2021-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = balancing_activated_reserves(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"),
        reserve_type = "A96",
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
  }
)
