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



testthat::test_that(
  desc = "implicit_offered_transfer_capacity() works",
  code = {
    testthat::expect_no_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        contract_type = "A01",
        tidy_output = FALSE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = NULL,
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = NULL,
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = c("10Y1001A1001A82H", "10YDK-1--------W"),
        eic_out = c("10YDK-1--------W", "10Y1001A1001A82H"),
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        contract_type = "A99",
        tidy_output = FALSE
      ),
      info = "The 'contract_type' parameter should be 'A01' or 'A07'!"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
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
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
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
      ),
      info = "Unauthorized. Missing or invalid security token!"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "explicit_offered_transfer_capacity() works",
  code = {
    testthat::expect_no_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-18",
          tz = "CET"
        ),
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = NULL,
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = NULL,
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = c("10YBE----------2", "10YGB----------A"),
        eic_out = c("10YGB----------A", "10YBE----------2"),
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-17",
          tz = "CET"
        ),
        contract_type = "A99",
        tidy_output = FALSE
      ),
      info = paste(
        "The 'contract_type' parameter should be ",
        "'A01', 'A02', 'A03', 'A04', 'A06', 'A07' or 'A08'!"
      )
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
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
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
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
      ),
      info = "Unauthorized. Missing or invalid security token!"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(
          x = "2023-08-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-08-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "continuous_offered_transfer_capacity() works",
  code = {
    testthat::expect_no_error(
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(
          x = "2024-05-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-05-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = NULL,
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(
          x = "2024-05-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-05-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = NULL,
        period_start = lubridate::ymd(
          x = "2024-05-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-05-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = c("10YNL----------L", "10YBE----------2"),
        eic_out = c("10YBE----------2", "10YNL----------L"),
        period_start = lubridate::ymd(
          x = "2024-05-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-05-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(
          x = "2024-05-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-05-17",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(
          x = "2024-05-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-05-17",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Unauthorized. Missing or invalid security token!"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(
          x = "2024-05-16",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-05-17",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)



testthat::test_that(
  desc = "flow_based_allocations() works",
  code = {
    testthat::expect_no_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(
          x = "2018-12-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        process_type = "A32",
        archive = TRUE,
        tidy_output = FALSE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = flow_based_allocations(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2018-12-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = c("10YDOM-REGION-1V", "10Y1001A1001A91G"),
        period_start = lubridate::ymd(
          x = "2018-12-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one EIC per request!"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(
          x = "2018-12-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        process_type = "A99",
        tidy_output = FALSE
      ),
      info = paste(
        "The 'process_type' parameter should be",
        "'A32', 'A33', 'A43' or 'A44'!"
      )
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(
          x = "2018-12-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        archive = "yes",
        tidy_output = FALSE
      ),
      info = "The 'archive' argument should be TRUE or FALSE!"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(
          x = "2018-12-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(
          x = "2018-12-31",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-01-01",
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
  desc = "auction_revenue() works",
  code = {
    testthat::expect_no_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(
          x = "2023-08-25",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-26",
          tz = "CET"
        ),
        contract_type = "A01",
        tidy_output = FALSE
      )
    ) |>
      testthat::expect_warning()
    testthat::expect_error(
      object = auction_revenue(
        eic_in = NULL,
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(
          x = "2023-08-25",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-26",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'in' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = NULL,
        period_start = lubridate::ymd(
          x = "2023-08-25",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-26",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One 'out' control area EIC should be provided!"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = c("10YBA-JPCC-----D", "10YHR-HEP------M"),
        eic_out = c("10YHR-HEP------M", "10YBA-JPCC-----D"),
        period_start = lubridate::ymd(
          x = "2023-08-25",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-26",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in and one out EIC per request!"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(
          x = "2023-08-25",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-26",
          tz = "CET"
        ),
        contract_type = "A99",
        tidy_output = FALSE
      ),
      info = paste(
        "The 'contract_type' parameter should be",
        "'A01', 'A02', 'A03', 'A04', 'A06', 'A07' or 'A08'!"
      )
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(
          x = "2023-08-25",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-26",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(
          x = "2023-08-25",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2023-08-26",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Unauthorized. Missing or invalid security token!"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(
          x = "2023-08-25",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-08-26",
          tz = "CET"
        ),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
  }
)
