testthat::test_that(
  desc = "transm_x_border_phys_flow() works",
  code = {
    testthat::expect_no_error(
      object = transm_x_border_phys_flow(
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
    testthat::expect_error(
      object = transm_x_border_phys_flow(
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
      object = transm_x_border_phys_flow(
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
      object = transm_x_border_phys_flow(
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
      object = transm_x_border_phys_flow(
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
      object = transm_x_border_phys_flow(
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
  }
)



testthat::test_that(
  desc = "transm_day_ahead_transf_cap() works",
  code = {
    testthat::expect_no_error(
      object = transm_day_ahead_transf_cap(
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
    testthat::expect_error(
      object = transm_day_ahead_transf_cap(
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
      object = transm_day_ahead_transf_cap(
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
      object = transm_day_ahead_transf_cap(
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
      object = transm_day_ahead_transf_cap(
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
      object = transm_day_ahead_transf_cap(
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
  }
)



testthat::test_that(
  desc = "transm_day_ahead_comm_sched() works",
  code = {
    testthat::expect_no_error(
      object = transm_day_ahead_comm_sched(
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
    testthat::expect_error(
      object = transm_day_ahead_comm_sched(
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
      object = transm_day_ahead_comm_sched(
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
      object = transm_day_ahead_comm_sched(
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
      object = transm_day_ahead_comm_sched(
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
      object = transm_day_ahead_comm_sched(
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
      info = "!"
    )
  }
)



testthat::test_that(
  desc = "transm_total_comm_sched() works",
  code = {
    testthat::expect_no_error(
      object = transm_total_comm_sched(
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
    testthat::expect_error(
      object = transm_total_comm_sched(
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
      object = transm_total_comm_sched(
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
      object = transm_total_comm_sched(
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
      object = transm_total_comm_sched(
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
      object = transm_total_comm_sched(
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
  }
)



testthat::test_that(
  desc = "transm_day_ahead_prices() works",
  code = {
    testthat::expect_no_error(
      object = transm_day_ahead_prices(
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
    )
    testthat::expect_error(
      object = transm_day_ahead_prices(
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
      object = transm_day_ahead_prices(
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
      object = transm_day_ahead_prices(
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
  }
)



testthat::test_that(
  desc = "transm_total_nominated_cap() works",
  code = {
    testthat::expect_no_error(
      object = transm_total_nominated_cap(
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
    )
    testthat::expect_error(
      object = transm_total_nominated_cap(
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
      object = transm_total_nominated_cap(
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
      object = transm_total_nominated_cap(
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
      object = transm_total_nominated_cap(
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
      object = transm_total_nominated_cap(
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
  desc = "transm_already_allocated_cap() works",
  code = {
    testthat::expect_no_error(
      object = transm_already_allocated_cap(
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
        auction_type = "A02",
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = transm_already_allocated_cap(
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
        auction_type = "A02",
        contract_type = "A01",
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = transm_already_allocated_cap(
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
        auction_type = "A02",
        contract_type = "A01",
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = transm_already_allocated_cap(
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
        auction_type = "A02",
        contract_type = "A01",
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = transm_already_allocated_cap(
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
        auction_type = "A02",
        contract_type = "A01",
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = transm_already_allocated_cap(
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
        auction_type = "A02",
        contract_type = "A01",
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)
