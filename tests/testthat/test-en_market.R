testthat::test_that(
  desc = "day_ahead_prices() works",
  code = {
    testthat::expect_no_error(
      object = day_ahead_prices(
        eic = "10YCZ-CEPS-----N",
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
      object = day_ahead_prices(
        eic = NULL,
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
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = day_ahead_prices(
        eic = c("10YCZ-CEPS-----N", "10Y1001A1001A82H"),
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
      info = "This wrapper only supports one in and one out EIC per request.!"
    )
    testthat::expect_error(
      object = day_ahead_prices(
        eic = "10YCZ-CEPS-----N",
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
      object = day_ahead_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-12-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "total_nominated_capacity() works",
  code = {
    testthat::expect_no_error(
      object = total_nominated_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = NULL,
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = NULL,
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = c("10YDE-VE-------2", "10YCZ-CEPS-----N"),
        eic_out = c("10YDE-VE-------2", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "his wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "already_allocated_total_capacity() works",
  code = {
    testthat::expect_no_error(
      object = already_allocated_total_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2024-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-02-02",
          tz = "CET"
        ),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = NULL,
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-02-02",
          tz = "CET"
        ),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = NULL,
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-02-02",
          tz = "CET"
        ),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        eic_out = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-02-02",
          tz = "CET"
        ),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-02-02",
          tz = "CET"
        ),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2019-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2021-02-02",
          tz = "CET"
        ),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)
