testthat::test_that(
  desc = "elastic_demands() validates inputs",
  code = {
    testthat::expect_error(
      object = elastic_demands(
        eic = NULL,
        process_type = "A47",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = elastic_demands(
        eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
        process_type = "A47",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = elastic_demands(
        eic = "10YHU-MAVIR----U",
        process_type = "A47",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = elastic_demands(
        eic = "10YHU-MAVIR----U",
        process_type = NULL,
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'process_type' failed: ",
        "Must be a subset of \\{'A47','A51'\\}, not 'NULL'"
      )
    )
    testthat::expect_error(
      object = elastic_demands(
        eic = "10YHU-MAVIR----U",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'process_type' failed: ",
        "Must be element of set \\{'A47','A51'\\}, but is 'INVALID'"
      )
    )
  }
)


testthat::test_that(
  desc = "elastic_demands() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    result_full <- tryCatch(
      elastic_demands(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-12-01", tz = "CET"),
        process_type = "A47",
        tidy_output = TRUE
      ),
      error = function(e) {
        testthat::skip(paste("API unavailable:", conditionMessage(e)))
      }
    )
    testthat::expect_contains(
      object = names(result_full),
      expected = c(
        "bid_ts_mrid",
        "bid_ts_auction_mrid",
        "bid_ts_flow_direction",
        "bid_ts_point_quantity",
        "bid_ts_point_energy_price_amount"
      )
    )
    tryCatch(
      elastic_demands(
        eic = "10YCZ-CEPS-----N",
        process_type = "A47",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      error = function(e) {
        testthat::skip(paste("API unavailable:", conditionMessage(e)))
      }
    )
  }
)


testthat::test_that(
  desc = "netted_volumes() validates inputs",
  code = {
    testthat::expect_error(
      object = netted_volumes(
        eic = NULL,
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-02-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = netted_volumes(
        eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-02-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = netted_volumes(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2021-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "1 day range limit should be applied"
    )
    testthat::expect_error(
      object = netted_volumes(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-02-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "netted_volumes() works",
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
      object = netted_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_contains(
      object = netted_volumes(
        eic = "10YDE-VE-------2",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ) |>
        names(),
      expected = c(
        "ts_connecting_domain_mrid",
        "ts_acquiring_domain_mrid",
        "ts_point_quantity"
      )
    )
  }
)


testthat::test_that(
  desc = "exchanged_volumes() validates inputs",
  code = {
    testthat::expect_error(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Must be a subset of \\{'A51','A60','A61'\\}"
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A264537254",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Must be element of set \\{'A51','A60','A61'\\}"
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-02-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
        process_type = "A61",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-02-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = "10YHU-MAVIR----U",
        process_type = "A60",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2021-03-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "1 day range limit should be applied"
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = "10YHU-MAVIR----U",
        process_type = "A60",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-02-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "exchanged_volumes() works",
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
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A60",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_contains(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A60",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ) |>
        names(),
      expected = c(
        "ts_connecting_domain_mrid",
        "ts_acquiring_domain_mrid",
        "ts_point_quantity"
      )
    )
  }
)


testthat::test_that(
  desc = "balancing_border_cap_limit() validates inputs",
  code = {
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A264537254",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'process_type' failed:",
        "Must be element of set \\{'A47','A51','A63'\\}"
      )
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = c("10YDE-RWENET---I", "10Y1001A1001A83F"),
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-02-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic_in' failed: Must have length 1"
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = c("10YBE----------2", "10Y1001A1001A83F"),
        process_type = "A51",
        period_start = lubridate::ymd(x = "2020-02-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2020-02-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic_out' failed: Must have length 1"
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = "ABC"
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = NULL,
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'eic_in' failed: ",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'eic_out' failed: ",
        "Must be of type 'string', not 'NULL'"
      )
    )
  }
)


testthat::test_that(
  desc = "balancing_border_cap_limit() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    for (pt in c("A47", "A51", "A63")) {
      testthat::expect_no_error(
        object = balancing_border_cap_limit(
          eic_in = "10YDE-RWENET---I",
          eic_out = "10YBE----------2",
          process_type = "A51",
          period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
          period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
          tidy_output = TRUE
        )
      )
    }
    testthat::expect_no_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A47",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_contains(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ) |>
        names(),
      expected = c(
        "ts_in_domain_mrid",
        "ts_out_domain_mrid",
        "ts_point_quantity"
      )
    )
  }
)


testthat::test_that(
  desc = "exchanged_volumes_per_border() validates inputs",
  code = {
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = NULL,
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'acquiring_eic' failed: ",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'acquiring_eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'process_type' failed:",
        "Must be element of set \\{'A51','A60','A61'\\}"
      )
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-05", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "1 day range limit should be applied"
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'connecting_eic' failed:",
        "Must be of type 'string', not 'NULL'."
      )
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = c("10YSK-SEPS-----K", "10YDE-VE-------2"),
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'connecting_eic' failed: Must have length 1"
    )
  }
)


testthat::test_that(
  desc = "exchanged_volumes_per_border() works",
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
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "hvdc_link_constrains() validates inputs",
  code = {
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = NULL,
        eic_out = "10YDE-RWENET---I",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'eic_in' failed: ",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'eic_out' failed: ",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = c("10YAT-APG------L", "10YDE-VE-------2"),
        eic_out = "10YDE-RWENET---I",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic_in' failed: Must have length 1"
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = "10YDE-RWENET---I",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = "10YDE-RWENET---I",
        eic_ic = c("AAAA", "BBBB"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic_ic' failed: Must have length 1."
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = "10YDE-RWENET---I",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'process_type' failed:",
        "Must be element of set \\{'A47','A51','A63'\\}, but is 'INVALID'"
      )
    )
  }
)


testthat::test_that(
  desc = "hvdc_link_constrains() works",
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
      object = hvdc_link_constrains(
        eic_in = "10YDK-1--------W",
        eic_out = "10YNL----------L",
        period_start = lubridate::ymd(x = "2021-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2021-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "changes_to_bid_availability() validates inputs",
  code = {
    testthat::expect_error(
      object = changes_to_bid_availability(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = changes_to_bid_availability(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = changes_to_bid_availability(
        eic = "10YCZ-CEPS-----N",
        business_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'business_type' failed: Must be element of set",
        "\\{'C40','C41','C42','C43','C44','C45','C46'\\}, but is 'INVALID'"
      )
    )
    testthat::expect_error(
      object = changes_to_bid_availability(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "changes_to_bid_availability() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    for (bt in paste0("C", 40L:46L)) {
      testthat::expect_no_error(
        object = changes_to_bid_availability(
          eic = "10YCZ-CEPS-----N",
          business_type = bt,
          period_start = lubridate::ymd(x = "2023-09-24", tz = "CET"),
          period_end = lubridate::ymd(x = "2023-09-25", tz = "CET"),
          tidy_output = TRUE
        ),
        message = bt
      )
    }
    testthat::expect_no_error(
      object = changes_to_bid_availability(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2023-09-24", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-09-25", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "current_balancing_state() validates inputs",
  code = {
    testthat::expect_error(
      object = current_balancing_state(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = current_balancing_state(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = current_balancing_state(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-06-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "100 days range limit should be applied"
    )
    testthat::expect_error(
      object = current_balancing_state(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "current_balancing_state() works",
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
      object = current_balancing_state(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "balancing_energy_bids() validates inputs",
  code = {
    testthat::expect_error(
      object = balancing_energy_bids(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = balancing_energy_bids(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "balancing_energy_bids() works",
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
      object = balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-01-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "aggregated_balancing_energy_bids() validates inputs",
  code = {
    testthat::expect_error(
      object = aggregated_balancing_energy_bids(
        eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = aggregated_balancing_energy_bids(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = aggregated_balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'process_type' failed: Must be element of set",
        "\\{'A51','A46','A47','A60','A61','A67','A68'\\}"
      )
    )
    testthat::expect_error(
      object = aggregated_balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "aggregated_balancing_energy_bids() works",
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
      object = aggregated_balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "procured_balancing_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'process_type' failed:",
        "Must be element of set \\{'A51','A52','A47'\\}"
      )
    )
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        market_agreement_type = c("A01", "A13"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'market_agreement_type' failed: Must be element of set",
        "\\{'A01','A02','A03','A04','A05','A06','A07','A13'\\}"
      )
    )
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "procured_balancing_capacity() works",
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
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        market_agreement_type = "A01",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "allocation_of_cross_zonal_balancing_cap() validates inputs",
  code = {
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = NULL,
        eic_connecting = "10YCH-SWISSGRIDZ",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'eic_acquiring' failed:",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'eic_connecting' failed:",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = c("10YAT-APG------L", "10YDE-VE-------2"),
        eic_connecting = "10YCH-SWISSGRIDZ",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic_acquiring' failed: Must have length 1"
    )
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = "10YCH-SWISSGRIDZ",
        market_agreement_type = c("A01", "A13"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'market_agreement_type' failed:",
        "Must be element of set \\{'A01','A02','A06'\\}"
      )
    )
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = "10YCH-SWISSGRIDZ",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "allocation_of_cross_zonal_balancing_cap() works",
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
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = "10YCH-SWISSGRIDZ",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = "10YCH-SWISSGRIDZ",
        market_agreement_type = "A01",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "contracted_reserves() validates inputs",
  code = {
    testthat::expect_error(
      object = contracted_reserves(
        eic = NULL,
        market_agreement_type = "A13",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        market_agreement_type = "A13",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        market_agreement_type = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'market_agreement_type' failed: Must be a subset of",
        "\\{'A01','A02','A03','A04','A06','A13'\\}"
      )
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        market_agreement_type = c("A01", "A13"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'market_agreement_type' failed: Must be element of set",
        "\\{'A01','A02','A03','A04','A06','A13'\\}"
      )
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        market_agreement_type = "A13",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "contracted_reserves() works",
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
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        market_agreement_type = "A13",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        market_agreement_type = "A13",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        market_agreement_type = "A13",
        psr_type = "A04",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "activated_balancing_prices() validates inputs",
  code = {
    testthat::expect_error(
      object = activated_balancing_prices(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = activated_balancing_prices(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = activated_balancing_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "activated_balancing_prices() works",
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
      object = activated_balancing_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "activated_balancing_prices() covers business_type branch with mock",
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
      object = activated_balancing_prices(
        eic = "10YCZ-CEPS-----N",
        business_type = "A63",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "imbalance_prices() validates inputs",
  code = {
    testthat::expect_error(
      object = imbalance_prices(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = imbalance_prices(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = imbalance_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "imbalance_prices() works",
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
      object = imbalance_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "imbalance_volumes() validates inputs",
  code = {
    testthat::expect_error(
      object = imbalance_volumes(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = imbalance_volumes(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = imbalance_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "imbalance_volumes() works",
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
      object = imbalance_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "financial_expenses_and_income() validates inputs",
  code = {
    testthat::expect_error(
      object = financial_expenses_and_income(
        eic = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = financial_expenses_and_income(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = financial_expenses_and_income(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "financial_expenses_and_income() works",
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
      object = financial_expenses_and_income(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "fcr_total_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = fcr_total_capacity(
        eic = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = fcr_total_capacity(
        eic = c("10YEU-CONT-SYNC0", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = fcr_total_capacity(
        eic = "10YEU-CONT-SYNC0",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "fcr_total_capacity() works",
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
      object = fcr_total_capacity(
        eic = "10YEU-CONT-SYNC0",
        period_start = lubridate::ymd(x = "2023-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "shares_of_fcr_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = c("10YDE-VE-------2", "10YDE-RWENET---I"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = "10YDE-VE-------2",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "shares_of_fcr_capacity() works",
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
      object = shares_of_fcr_capacity(
        eic = "10YCB-GERMANY--8",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-12-31", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "rr_and_frr_actual_capacity() validates inputs",
  code = {
    testthat::expect_error(
      object = rr_and_frr_actual_capacity(
        eic = NULL,
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = rr_and_frr_actual_capacity(
        eic = c("10YAT-APG------L", "10YDE-VE-------2"),
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = rr_and_frr_actual_capacity(
        eic = "10YAT-APG------L",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'process_type' failed:",
        "Must be element of set \\{'A56','A46'\\}"
      )
    )
    testthat::expect_error(
      object = rr_and_frr_actual_capacity(
        eic = "10YAT-APG------L",
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "rr_and_frr_actual_capacity() works",
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
      object = rr_and_frr_actual_capacity(
        eic = "10YAT-APG------L",
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "sharing_of_rr_and_frr_capacity() validates inputs",
  code = {
    # Note: [A26, A56, C22] combination is not available via the public API.
    # The happy path for sharing_of_rr_and_frr_capacity() cannot be exercised
    # without a mock.
    testthat::expect_error(
      object = sharing_of_rr_and_frr_capacity(
        eic_acquiring = NULL,
        eic_connecting = "10YAT-APG------L",
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'eic_acquiring' failed:",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = sharing_of_rr_and_frr_capacity(
        eic_acquiring = "10YCB-GERMANY--8",
        eic_connecting = NULL,
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'eic_connecting' failed:",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = sharing_of_rr_and_frr_capacity(
        eic_acquiring = c("10YCB-GERMANY--8", "10YDE-VE-------2"),
        eic_connecting = "10YAT-APG------L",
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic_acquiring' failed: Must have length 1"
    )
    testthat::expect_error(
      object = sharing_of_rr_and_frr_capacity(
        eic_acquiring = "10YCB-GERMANY--8",
        eic_connecting = "10YAT-APG------L",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'process_type' failed:",
        "Must be element of set \\{'A56','A46'\\}"
      )
    )
    testthat::expect_error(
      object = sharing_of_rr_and_frr_capacity(
        eic_acquiring = "10YCB-GERMANY--8",
        eic_connecting = "10YAT-APG------L",
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "netted_volumes_per_border() validates inputs",
  code = {
    testthat::expect_error(
      object = netted_volumes_per_border(
        acquiring_eic = NULL,
        connecting_eic = "10YFR-RTE------C",
        process_type = "A63",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste0(
        "Assertion on 'acquiring_eic' failed: ",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = netted_volumes_per_border(
        acquiring_eic = c("10YBE----------2", "10YDE-VE-------2"),
        connecting_eic = "10YFR-RTE------C",
        process_type = "A63",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'acquiring_eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = netted_volumes_per_border(
        acquiring_eic = "10YBE----------2",
        connecting_eic = NULL,
        process_type = "A63",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'connecting_eic' failed:",
        "Must be of type 'string', not 'NULL'."
      )
    )
    testthat::expect_error(
      object = netted_volumes_per_border(
        acquiring_eic = "10YBE----------2",
        connecting_eic = c("10YFR-RTE------C", "10YDE-VE-------2"),
        process_type = "A63",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'connecting_eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = netted_volumes_per_border(
        acquiring_eic = "10YBE----------2",
        connecting_eic = "10YFR-RTE------C",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = paste(
        "Assertion on 'process_type' failed: Must be element of set",
        "\\{'A51','A60','A61','A63'\\}"
      )
    )
    testthat::expect_error(
      object = netted_volumes_per_border(
        acquiring_eic = "10YBE----------2",
        connecting_eic = "10YFR-RTE------C",
        process_type = "A63",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-05", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "1 day range limit should be applied"
    )
    testthat::expect_error(
      object = netted_volumes_per_border(
        acquiring_eic = "10YBE----------2",
        connecting_eic = "10YFR-RTE------C",
        process_type = "A63",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "netted_volumes_per_border() works",
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
      object = netted_volumes_per_border(
        acquiring_eic = "10YBE----------2",
        connecting_eic = "10YFR-RTE------C",
        process_type = "A63",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "shares_of_fcr_capacity() validates inputs (corrected)",
  code = {
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must be of type 'string', not 'NULL'"
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = c("10YCB-GERMANY--8", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "Assertion on 'eic' failed: Must have length 1"
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = "10YCB-GERMANY--8",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "sharing_of_rr_and_frr_capacity() covers happy path with mock",
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
      object = sharing_of_rr_and_frr_capacity(
        eic_acquiring = "10YCB-GERMANY--8",
        eic_connecting = "10YAT-APG------L",
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "elastic_demands() covers happy path with mock",
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
      object = elastic_demands(
        eic = "10YCZ-CEPS-----N",
        process_type = "A47",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "netted_volumes() covers happy path with mock",
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
      object = netted_volumes(
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
  desc = "exchanged_volumes() covers happy path with mock",
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
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "balancing_border_cap_limit() covers happy path with mock",
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
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "exchanged_volumes_per_border() covers happy path with mock",
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
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YBE----------2",
        connecting_eic = "10YFR-RTE------C",
        process_type = "A60",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "hvdc_link_constrains() covers happy path with mock",
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
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = "10YDE-RWENET---I",
        process_type = "A63",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "changes_to_bid_availability() covers happy path with mock",
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
      object = changes_to_bid_availability(
        eic = "10YCZ-CEPS-----N",
        business_type = "C46",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "current_balancing_state() covers happy path with mock",
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
      object = current_balancing_state(
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
  desc = "balancing_energy_bids() covers happy path with mock",
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
      object = balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "A47",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "aggregated_balancing_energy_bids() covers happy path with mock",
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
      object = aggregated_balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "A47",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "procured_balancing_capacity() covers happy path with mock",
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
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        market_agreement_type = "A13",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "allocation_of_cross_zonal_balancing_cap covers happy path with mock",
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
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YCB-GERMANY--8",
        eic_connecting = "10YAT-APG------L",
        market_agreement_type = "A01",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "contracted_reserves() covers happy path with mock",
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
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        market_agreement_type = "A01",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "503"
    )
  }
)


testthat::test_that(
  desc = "imbalance_prices() covers happy path with mock",
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
      object = imbalance_prices(
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
  desc = "imbalance_volumes() covers happy path with mock",
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
      object = imbalance_volumes(
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
  desc = paste(
    "financial_expenses_and_income()",
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
      object = financial_expenses_and_income(
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
  desc = "fcr_total_capacity() covers happy path with mock",
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
      object = fcr_total_capacity(
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
  desc = "shares_of_fcr_capacity() covers happy path with mock",
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
      object = shares_of_fcr_capacity(
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
  desc = "shares_of_fcr_capacity() covers happy path with mock (corrected)",
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
      object = shares_of_fcr_capacity(
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
  desc = "rr_and_frr_actual_capacity() covers happy path with mock",
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
      object = rr_and_frr_actual_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A56",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "netted_volumes_per_border() covers happy path with mock",
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
      object = netted_volumes_per_border(
        acquiring_eic = "10YBE----------2",
        connecting_eic = "10YFR-RTE------C",
        process_type = "A63",
        period_start = lubridate::ymd(x = "2025-03-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-03-02", tz = "CET"),
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)
