testthat::test_that(
  desc = "energy_prices() validates inputs",
  code = {
    testthat::expect_error(
      object = energy_prices(
        eic = NULL,
        period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = energy_prices(
        eic = c("10YCZ-CEPS-----N", "10Y1001A1001A82H"),
        period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = energy_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = energy_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-12-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "1 year range limit should be applied"
    )
  }
)


testthat::test_that(
  desc = "energy_prices() works",
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
      object = energy_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2019-11-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-12-01", tz = "CET"),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "total_nominated_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = NULL,
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = total_nominated_capacity(
        eic_in = c("10YDE-VE-------2", "10YCZ-CEPS-----N"),
        eic_out = c("10YDE-VE-------2", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
  }
)


testthat::test_that(
  desc = "total_nominated_capacity() works",
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
      object = total_nominated_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-03-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "already_allocated_total_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = NULL,
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-02-02", tz = "CET"),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-02-02", tz = "CET"),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        eic_out = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-02-02", tz = "CET"),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = already_allocated_total_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2019-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-02-02", tz = "CET"),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE,
        security_token = rep(x = "z", 36L) |> paste(collapse = "")
      ),
      regexp = "The `security_token` should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "already_allocated_total_capacity() works",
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
      object = already_allocated_total_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-02-02", tz = "CET"),
        auction_category = "A04",
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "implicit_offered_transfer_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = NULL,
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = c("10Y1001A1001A82H", "10YDK-1--------W"),
        eic_out = c("10YDK-1--------W", "10Y1001A1001A82H"),
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        contract_type = "A99",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be element of set"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "implicit_offered_transfer_capacity() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "implicit_offered_transfer_capacity() works",
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
      object = implicit_offered_transfer_capacity(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "explicit_offered_transfer_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = NULL,
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = c("10YBE----------2", "10YGB----------A"),
        eic_out = c("10YGB----------A", "10YBE----------2"),
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        contract_type = "A99",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be element of set"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "explicit_offered_transfer_capacity() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "explicit_offered_transfer_capacity() works",
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
      object = explicit_offered_transfer_capacity(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-18", tz = "CET"),
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "continuous_offered_transfer_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = NULL,
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = c("10YNL----------L", "10YBE----------2"),
        eic_out = c("10YBE----------2", "10YNL----------L"),
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "continuous_offered_transfer_capacity() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "continuous_offered_transfer_capacity() works",
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
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "flow_based_allocations() validates inputs",
  code = {
    testthat::expect_error(
      object = flow_based_allocations(
        eic = NULL,
        period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = c("10YDOM-REGION-1V", "10Y1001A1001A91G"),
        period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
        process_type = "A99",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be element of set"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
        archive = "yes",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'logical flag', not 'character'"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "flow_based_allocations() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "flow_based_allocations() works",
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
      object = flow_based_allocations(
        eic = "10YDOM-REGION-1V",
        period_start = lubridate::ymd(x = "2018-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2019-01-01", tz = "CET"),
        process_type = "A32",
        archive = TRUE,
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "auction_revenue() validates inputs",
  code = {
    testthat::expect_error(
      object = auction_revenue(
        eic_in = NULL,
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = c("10YBA-JPCC-----D", "10YHR-HEP------M"),
        eic_out = c("10YHR-HEP------M", "10YBA-JPCC-----D"),
        period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
        contract_type = "A99",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be element of set"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "auction_revenue() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "auction_revenue() works",
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
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(x = "2023-08-25", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-26", tz = "CET"),
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "net_positions() validates inputs",
  code = {
    testthat::expect_error(
      object = net_positions(
        eic = NULL,
        period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = net_positions(
        eic = c("10YCZ-CEPS-----N", "10Y1001A1001A82H"),
        period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = net_positions(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        contract_type = "A99",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be element of set"
    )
    testthat::expect_error(
      object = net_positions(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "net_positions() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = net_positions(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "net_positions() works",
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
      object = net_positions(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2015-12-31", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "congestion_income() validates inputs",
  code = {
    testthat::expect_error(
      object = congestion_income(
        eic = NULL,
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = congestion_income(
        eic = c("10YDOM-1001A083J", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = congestion_income(
        eic = "10YDOM-1001A083J",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        contract_type = "A99",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be element of set"
    )
    testthat::expect_error(
      object = congestion_income(
        eic = "10YDOM-1001A083J",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "congestion_income() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = congestion_income(
        eic = "10YDOM-1001A083J",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "congestion_income() works",
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
      object = congestion_income(
        eic = "10YDOM-1001A083J",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        contract_type = "A01",
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "allocated_transfer_capacities_3rd_countries() validates inputs",
  code = {
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = NULL,
        eic_out = "10YUA-WEPS-----0",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'eic_in' failed: ",
        "Must be of type 'string', not 'NULL'."
      )
    )
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = "10YSK-SEPS-----K",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'eic_out' failed:",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = c("10YSK-SEPS-----K", "10YUA-WEPS-----0"),
        eic_out = c("10YUA-WEPS-----0", "10YSK-SEPS-----K"),
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic_in' failed: Must have length 1."
    )
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YUA-WEPS-----0",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        contract_type = "A99",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'contract_type' failed:",
        "Must be element of set",
        "\\{'A01','A02','A03','A04','A06','A07','A08'\\}, but is 'A99'"
      )
    )
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YUA-WEPS-----0",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        auction_category = "A99",
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'auction_category'",
        "failed: Must be element of set \\{'A01','A02','A03','A04'\\},",
        "but is 'A99'"
      )
    )
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YUA-WEPS-----0",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        position = 0L,
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'position' failed: Must be >= 1"
    )
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YUA-WEPS-----0",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "allocated_transfer_capacities_3rd_countries() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YUA-WEPS-----0",
        period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2016-01-02", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "allocated_transfer_capacities_3rd_countries() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    df <- allocated_transfer_capacities_3rd_countries(
      eic_in = "10YSK-SEPS-----K",
      eic_out = "10YUA-WEPS-----0",
      period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
      period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
      contract_type = "A01",
      auction_category = "A04",
      position = 2L,
      tidy_output = FALSE
    )
    cts <- c("A01", "A02", "A03", "A04", "A06", "A07", "A08")
    ats <- c("A01", "A02", "A03", "A04")
    for (ct in cts) {
      for (at in ats) {
        testthat::expect_no_error(
          object = allocated_transfer_capacities_3rd_countries(
            eic_in = "10YSK-SEPS-----K",
            eic_out = "10YUA-WEPS-----0",
            period_start = lubridate::ymd(x = "2016-01-01", tz = "CET"),
            period_end = lubridate::ymd(x = "2016-01-01", tz = "CET"),
            contract_type = ct,
            auction_category = at,
            position = 1L,
            tidy_output = FALSE
          )
        )
      }
    }
    testthat::expect_match(
      object = df$reason_code,
      regexp = "999"
    )
    testthat::expect_error(
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YUA-WEPS-----0",
        period_start = lubridate::ymd(x = "2015-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2015-05-01", tz = "CET"),
        contract_type = "A01",
        auction_category = "A04",
        position = 1L,
        tidy_output = FALSE
      ),
      regexp = "exceeds the allowed maximum \\(100\\) for data item"
    )
  }
)


testthat::test_that(
  desc = "implicit_offered_transfer_capacities() validates inputs",
  code = {
    testthat::expect_error(
      object = implicit_offered_transfer_capacities(
        eic_in = NULL,
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacities(
        eic_in = "10Y1001A1001A82H",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacities(
        eic_in = c("10Y1001A1001A82H", "10YDK-1--------W"),
        eic_out = c("10YDK-1--------W", "10Y1001A1001A82H"),
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacities(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "implicit_offered_transfer_capacities() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = implicit_offered_transfer_capacities(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "implicit_offered_transfer_capacities() works",
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
      object = implicit_offered_transfer_capacities(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "explicit_offered_transfer_capacities() validates inputs",
  code = {
    testthat::expect_error(
      object = explicit_offered_transfer_capacities(
        eic_in = NULL,
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacities(
        eic_in = "10YBE----------2",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacities(
        eic_in = c("10YBE----------2", "10YGB----------A"),
        eic_out = c("10YGB----------A", "10YBE----------2"),
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacities(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "explicit_offered_transfer_capacities() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = explicit_offered_transfer_capacities(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "explicit_offered_transfer_capacities() works",
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
      object = explicit_offered_transfer_capacities(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "continuous_offered_transfer_capacities() validates inputs",
  code = {
    testthat::expect_error(
      object = continuous_offered_transfer_capacities(
        eic_in = NULL,
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacities(
        eic_in = "10YNL----------L",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacities(
        eic_in = c("10YNL----------L", "10YBE----------2"),
        eic_out = c("10YBE----------2", "10YNL----------L"),
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacities(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "continuous_offered_transfer_capacities() validates inputs 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_error(
      object = continuous_offered_transfer_capacities(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "continuous_offered_transfer_capacities() works",
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
      object = continuous_offered_transfer_capacities(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "intraday_prices() validates inputs",
  code = {
    testthat::expect_error(
      object = intraday_prices(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = intraday_prices(
        eic = c("10YPL-AREA-----S", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = intraday_prices(
        eic = "10YPL-AREA-----S",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "aggregated_bids() validates inputs",
  code = {
    testthat::expect_error(
      object = aggregated_bids(
        eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = aggregated_bids(
        eic = c("10YPL-AREA-----S", "10YCZ-CEPS-----N"),
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must have length 1"
    )
    testthat::expect_error(
      object = aggregated_bids(
        eic = "10YPL-AREA-----S",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "Must be element of set"
    )
    testthat::expect_error(
      object = aggregated_bids(
        eic = "10YPL-AREA-----S",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "intraday_prices() covers happy path with mock",
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
      object = intraday_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "aggregated_bids() covers happy path with mock",
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
      object = aggregated_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        tidy_output = FALSE,
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "implicit_offered_transfer_capacity() covers happy path with mock",
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
      object = implicit_offered_transfer_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      )
    )
  }
)


testthat::test_that(
  desc = "explicit_offered_transfer_capacity() covers happy path with mock",
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
      object = explicit_offered_transfer_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "continuous_offered_transfer_capacity() covers happy path with mock",
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
      object = continuous_offered_transfer_capacity(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "flow_based_allocations() covers happy path with mock",
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
      object = flow_based_allocations(
        eic = "10Y1001A1001A91G",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        process_type = "A43",
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "auction_revenue() covers happy path with mock",
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
      object = auction_revenue(
        eic_in = "10YBA-JPCC-----D",
        eic_out = "10YHR-HEP------M",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "total_nominated_capacity() covers happy path with mock",
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
      object = total_nominated_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "already_allocated_total_capacity() covers happy path with mock",
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
      object = already_allocated_total_capacity(
        eic_in = "10YDE-VE-------2",
        eic_out = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "energy_prices() covers happy path with mock",
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
      object = energy_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        contract_type = "A01",
        security_token = .test_token
      ),
      regexp = "503"
    )
  }
)


testthat::test_that(
  desc = "net_positions() covers happy path with mock",
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
      object = net_positions(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "congestion_income() covers happy path with mock",
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
      object = congestion_income(
        eic = "10YDOM-1001A083J",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = paste(
    "allocated_transfer_capacities_3rd_countries()",
    "covers happy path with mock"
  ),
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
      object = allocated_transfer_capacities_3rd_countries(
        eic_in = "10YSK-SEPS-----K",
        eic_out = "10YUA-WEPS-----0",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "implicit_offered_transfer_capacities() covers happy path with mock",
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
      object = implicit_offered_transfer_capacities(
        eic_in = "10Y1001A1001A82H",
        eic_out = "10YDK-1--------W",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "explicit_offered_transfer_capacities() covers happy path with mock",
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
      object = explicit_offered_transfer_capacities(
        eic_in = "10YBE----------2",
        eic_out = "10YGB----------A",
        period_start = lubridate::ymd(x = "2023-08-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-08-17", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "continuous_offered_transfer_capacities() covers happy path with mock",
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
      object = continuous_offered_transfer_capacities(
        eic_in = "10YNL----------L",
        eic_out = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-05-16", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-05-17", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)
