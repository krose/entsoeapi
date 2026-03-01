testthat::test_that(
  desc = "elastic_demands() works",
  code = {
    testthat::expect_no_error(
      object = result_full <- elastic_demands(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-12-01",
          tz = "CET"
        ),
        process_type = "A47",
        tidy_output = TRUE
      ),
      message = "uses offsetting"
    )
    testthat::expect_contains(
      object = names(result_full),
      expected = c(
        "bid_ts_mrid",
        "bid_ts_auction_mrid",
        "bid_ts_flow_direction"
      )
    )
    testthat::expect_no_error(
      object = elastic_demands(
        eic = "10YCZ-CEPS-----N",
        process_type = "A47",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = elastic_demands(
        eic = NULL,
        process_type = "A47",
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
      object = elastic_demands(
        eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
        process_type = "A47",
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
      object = elastic_demands(
        eic = "10YHU-MAVIR----U",
        process_type = "A47",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2021-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = elastic_demands(
        eic = "10YHU-MAVIR----U",
        process_type = "A47",
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
      object = elastic_demands(
        eic = "10YHU-MAVIR----U",
        process_type = NULL,
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
      regexp = "The 'process_type' should be 'A47' or 'A51'."
    )
    testthat::expect_error(
      object = elastic_demands(
        eic = "10YHU-MAVIR----U",
        process_type = "INVALID",
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
      regexp = "The 'process_type' should be 'A47' or 'A51'."
    )
  }
)



testthat::test_that(
  desc = "netted_volumes() works",
  code = {
    testthat::expect_no_error(
      object = netted_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_contains(
      object = netted_volumes(
        eic = "10YDE-VE-------2",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ) |>
        names(),
      expected = c(
        "ts_connecting_domain_mrid",
        "ts_acquiring_domain_mrid",
        "ts_point_quantity"
      )
    )
    testthat::expect_error(
      object = netted_volumes(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = netted_volumes(
        eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = netted_volumes(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2021-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One day range limit should be applied!"
    )
    testthat::expect_error(
      object = netted_volumes(
        eic = "10YHU-MAVIR----U",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
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
  desc = "exchanged_volumes() works",
  code = {
    testthat::expect_error(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A264537254",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A60",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_contains(
      object = exchanged_volumes(
        eic = "10YCZ-CEPS-----N",
        process_type = "A60",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ) |>
        names(),
      expected = c(
        "ts_connecting_domain_mrid",
        "ts_acquiring_domain_mrid",
        "ts_point_quantity"
      )
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided!"
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
        process_type = "A61",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = "10YHU-MAVIR----U",
        process_type = "A60",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2021-03-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One day range limit should be applied!"
    )
    testthat::expect_error(
      object = exchanged_volumes(
        eic = "10YHU-MAVIR----U",
        process_type = "A60",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
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
  desc = "balancing_border_cap_limit() works",
  code = {
    for (pt in c("A47", "A51", "A63")) {
      testthat::expect_no_error(
        object = balancing_border_cap_limit(
          eic_in = "10YDE-RWENET---I",
          eic_out = "10YBE----------2",
          process_type = "A51",
          period_start = lubridate::ymd(
            x = "2024-01-01",
            tz = "CET"
          ),
          period_end = lubridate::ymd(
            x = "2024-01-02",
            tz = "CET"
          ),
          tidy_output = TRUE
        )
      )
    }
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A264537254",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A47",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_contains(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ) |>
        names(),
      expected = c(
        "ts_in_domain_mrid",
        "ts_out_domain_mrid",
        "ts_point_quantity"
      )
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = c("10YDE-RWENET---I", "10Y1001A1001A83F"),
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one in and one out EIC per request."
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = c("10YBE----------2", "10Y1001A1001A83F"),
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2020-02-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-02-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one in and one out EIC per request."
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        eic_interconnector = c("AAA", "BBB"),
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = paste(
        "None or one Transmission Asset (eic_interconnector)",
        "should be provided."
      )
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-01-02",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-12-02",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = NULL,
        eic_out = "10YBE----------2",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One 'in' control area EIC should be provided"
    )
    testthat::expect_error(
      object = balancing_border_cap_limit(
        eic_in = "10YDE-RWENET---I",
        eic_out = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One 'out' control area EIC should be provided"
    )
  }
)



testthat::test_that(
  desc = "exchanged_volumes_per_border() works",
  code = {
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
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = NULL,
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One acquiring EIC should be provided"
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "This wrapper only supports one acquiring EIC per request"
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "The 'process_type' should be 'A51', 'A60' or 'A61'."
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = "10YSK-SEPS-----K",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-05", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One day range limit should be applied"
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
      info = "Valid security token should be provided."
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One connecting EIC should be provided"
    )
    testthat::expect_error(
      object = exchanged_volumes_per_border(
        acquiring_eic = "10YCZ-CEPS-----N",
        connecting_eic = c("10YSK-SEPS-----K", "10YDE-VE-------2"),
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "This wrapper only supports one connecting EIC per request"
    )
  }
)



testthat::test_that(
  desc = "hvdc_link_constrains() works",
  code = {
    testthat::expect_no_error(
      object = hvdc_link_constrains(
        eic_in = "10YDK-1--------W",
        eic_out = "10YNL----------L",
        period_start = lubridate::ymd(x = "2021-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2021-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = NULL,
        eic_out = "10YDE-RWENET---I",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One 'in' domain EIC should be provided"
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One 'out' domain EIC should be provided"
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = c("10YAT-APG------L", "10YDE-VE-------2"),
        eic_out = "10YDE-RWENET---I",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "This wrapper only supports one in and one out EIC per request"
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = "10YDE-RWENET---I",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One year range limit should be applied"
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
      regexp = "Valid security token should be provided"
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = "10YDE-RWENET---I",
        eic_ic = c("AAAA", "BBBB"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "This wrapper only supports one interconnector EIC per request"
    )
    testthat::expect_error(
      object = hvdc_link_constrains(
        eic_in = "10YAT-APG------L",
        eic_out = "10YDE-RWENET---I",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "The 'process_type' should be 'A47', 'A51' or 'A63'"
    )
  }
)



testthat::test_that(
  desc = "changes_to_bid_availability() works",
  code = {
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
    testthat::expect_error(
      object = changes_to_bid_availability(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One control area EIC should be provided"
    )
    testthat::expect_error(
      object = changes_to_bid_availability(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "This wrapper only supports one control area EIC per request"
    )
    testthat::expect_error(
      object = changes_to_bid_availability(
        eic = "10YCZ-CEPS-----N",
        business_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = paste(
        "The 'business_type' should be 'C40', 'C41', 'C42',",
        "'C43', 'C44', 'C45', 'C46'"
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
      regexp = "Valid security token should be provided"
    )
    testthat::expect_error(
      object = changes_to_bid_availability(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One year range limit should be applied"
    )
  }
)



testthat::test_that(
  desc = "current_balancing_state() works",
  code = {
    testthat::expect_no_error(
      object = current_balancing_state(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = current_balancing_state(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided."
    )
    testthat::expect_error(
      object = current_balancing_state(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request."
    )
    testthat::expect_error(
      object = current_balancing_state(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-06-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "100 day range limit should be applied!"
    )
    testthat::expect_error(
      object = current_balancing_state(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
  }
)



testthat::test_that(
  desc = "balancing_energy_bids() works",
  code = {
    testthat::expect_no_error(
      object = balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-01-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = balancing_energy_bids(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One connecting domain EIC should be provided."
    )
    testthat::expect_error(
      object = balancing_energy_bids(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one connecting domain EIC per request."
    )
    testthat::expect_error(
      object = balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
  }
)



testthat::test_that(
  desc = "aggregated_balancing_energy_bids() works",
  code = {
    testthat::expect_no_error(
      object = aggregated_balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = aggregated_balancing_energy_bids(
        eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided."
    )
    testthat::expect_error(
      object = aggregated_balancing_energy_bids(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request."
    )
    testthat::expect_error(
      object = aggregated_balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "The 'process_type' should be 'A51', 'A46', 'A47', 'A60' or 'A61'."
    )
    testthat::expect_error(
      object = aggregated_balancing_energy_bids(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
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
      info = "Valid security token should be provided."
    )
  }
)



testthat::test_that(
  desc = "procured_balancing_capacity() works",
  code = {
    testthat::expect_no_error(
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = NULL,
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided."
    )
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        process_type = "A51",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request."
    )
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "The 'process_type' should be 'A51', 'A52' or 'A47'."
    )
    testthat::expect_error(
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        type_market_agreement = c("A01", "A13"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "None or one 'type_market_agreement' should be provided."
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
      info = "Valid security token should be provided."
    )
    testthat::expect_no_error(
      object = procured_balancing_capacity(
        eic = "10YCZ-CEPS-----N",
        process_type = "A51",
        type_market_agreement = "A01",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)



testthat::test_that(
  desc = "allocation_of_cross_zonal_balancing_cap() works",
  code = {
    testthat::expect_no_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = "10YCH-SWISSGRIDZ",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = NULL,
        eic_connecting = "10YCH-SWISSGRIDZ",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One acquiring domain EIC should be provided."
    )
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One connecting domain EIC should be provided."
    )
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = c("10YAT-APG------L", "10YDE-VE-------2"),
        eic_connecting = "10YCH-SWISSGRIDZ",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = paste(
        "This wrapper only supports one acquiring and",
        "one connecting EIC per request."
      )
    )
    testthat::expect_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = "10YCH-SWISSGRIDZ",
        type_market_agreement = c("A01", "A13"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "None or one 'type_market_agreement' should be provided."
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
      info = "Valid security token should be provided."
    )
    testthat::expect_no_error(
      object = allocation_of_cross_zonal_balancing_cap(
        eic_acquiring = "10YAT-APG------L",
        eic_connecting = "10YCH-SWISSGRIDZ",
        type_market_agreement = "A01",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)



testthat::test_that(
  desc = "contracted_reserves() works",
  code = {
    testthat::expect_no_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        type_market_agreement = "A13",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = NULL,
        type_market_agreement = "A13",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided."
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        type_market_agreement = "A13",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request."
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        type_market_agreement = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "A 'type_market_agreement' value should be provided."
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        type_market_agreement = c("A01", "A13"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "Only one 'type_market_agreement' value should be provided."
    )
    testthat::expect_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        type_market_agreement = "A13",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
    testthat::expect_no_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        type_market_agreement = "A13",
        process_type = "A51",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = contracted_reserves(
        eic = "10YCZ-CEPS-----N",
        type_market_agreement = "A13",
        psr_type = "A04",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
  }
)



testthat::test_that(
  desc = "activated_balancing_prices() works",
  code = {
    testthat::expect_no_error(
      object = activated_balancing_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = activated_balancing_prices(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided."
    )
    testthat::expect_error(
      object = activated_balancing_prices(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request."
    )
    testthat::expect_error(
      object = activated_balancing_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = activated_balancing_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
  }
)



testthat::test_that(
  desc = "imbalance_prices() works",
  code = {
    testthat::expect_no_error(
      object = imbalance_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = imbalance_prices(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided."
    )
    testthat::expect_error(
      object = imbalance_prices(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request."
    )
    testthat::expect_error(
      object = imbalance_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = imbalance_prices(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
  }
)



testthat::test_that(
  desc = "imbalance_volumes() works",
  code = {
    testthat::expect_no_error(
      object = imbalance_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = imbalance_volumes(
        eic = NULL,
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided."
    )
    testthat::expect_error(
      object = imbalance_volumes(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request."
    )
    testthat::expect_error(
      object = imbalance_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2025-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = imbalance_volumes(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
  }
)



testthat::test_that(
  desc = "financial_expenses_and_income_for_balancing() works",
  code = {
    testthat::expect_no_error(
      object = financial_expenses_and_income_for_balancing(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = financial_expenses_and_income_for_balancing(
        eic = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One control area EIC should be provided."
    )
    testthat::expect_error(
      object = financial_expenses_and_income_for_balancing(
        eic = c("10YCZ-CEPS-----N", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one control area EIC per request."
    )
    testthat::expect_error(
      object = financial_expenses_and_income_for_balancing(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = financial_expenses_and_income_for_balancing(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
  }
)



testthat::test_that(
  desc = "fcr_total_capacity() works",
  code = {
    testthat::expect_no_error(
      object = fcr_total_capacity(
        eic = "10YEU-CONT-SYNC0",
        period_start = lubridate::ymd(x = "2023-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = fcr_total_capacity(
        eic = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One area EIC should be provided"
    )
    testthat::expect_error(
      object = fcr_total_capacity(
        eic = c("10YEU-CONT-SYNC0", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "This wrapper only supports one area EIC per request"
    )
    testthat::expect_error(
      object = fcr_total_capacity(
        eic = "10YEU-CONT-SYNC0",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "One year range limit should be applied"
    )
    testthat::expect_error(
      object = fcr_total_capacity(
        eic = "10YEU-CONT-SYNC0",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      regexp = "Valid security token should be provided"
    )
  }
)



testthat::test_that(
  desc = "shares_of_fcr_capacity() works",
  code = {
    testthat::expect_no_error(
      object = shares_of_fcr_capacity(
        eic = "10YDE-VE-------2",
        business_type = "C23",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-01-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = NULL,
        business_type = "C23",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One area EIC should be provided."
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = c("10YDE-VE-------2", "10YDE-RWENET---I"),
        business_type = "C23",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one area EIC per request."
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = "10YDE-VE-------2",
        business_type = "INVALID",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "The 'business_type' should be 'C23' or 'B95'."
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = "10YDE-VE-------2",
        business_type = "C23",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2023-05-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = shares_of_fcr_capacity(
        eic = "10YDE-VE-------2",
        business_type = "C23",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
  }
)



testthat::test_that(
  desc = "rr_and_frr_actual_capacity() works",
  code = {
    testthat::expect_no_error(
      object = rr_and_frr_actual_capacity(
        eic = "10YAT-APG------L",
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = rr_and_frr_actual_capacity(
        eic = NULL,
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One area EIC should be provided."
    )
    testthat::expect_error(
      object = rr_and_frr_actual_capacity(
        eic = c("10YAT-APG------L", "10YDE-VE-------2"),
        process_type = "A56",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one area EIC per request."
    )
    testthat::expect_error(
      object = rr_and_frr_actual_capacity(
        eic = "10YAT-APG------L",
        process_type = "INVALID",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "The 'process_type' should be 'A56' (FRR) or 'A46' (RR)."
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
      info = "Valid security token should be provided."
    )
    testthat::expect_error(
      object = rr_and_frr_actual_capacity(
        eic = "10YAT-APG------L",
        process_type = "A56",
        business_type = "INVALID",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      ),
      regexp = "The 'business_type' should be 'C77', 'C78' or 'C79'."
    )
  }
)



testthat::test_that(
  desc = "rr_actual_capacity() works",
  code = {
    testthat::expect_no_error(
      object = rr_actual_capacity(
        eic = "10YCZ-CEPS-----N",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = rr_actual_capacity(
        eic = NULL,
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "One area EIC should be provided."
    )
    testthat::expect_error(
      object = rr_actual_capacity(
        eic = c("10YAT-APG------L", "10YDE-VE-------2"),
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE
      ),
      info = "This wrapper only supports one area EIC per request."
    )
    testthat::expect_error(
      object = rr_actual_capacity(
        eic = "10YAT-APG------L",
        period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2022-04-01", tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided."
    )
  }
)



# testthat::test_that(
#   desc = "sharing_of_frr_capacity() works",
#   code = {
#     # Note: [A26, A56, C22] combination is not available via the public API.
#     # The happy path for sharing_of_frr_capacity() cannot be exercised
#     # without a mock.
#     testthat::expect_error(
#       object = sharing_of_frr_capacity(
#         eic_acquiring = NULL,
#         eic_connecting = "10YAT-APG------L",
#         process_type = "A56",
#         period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#         period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#         tidy_output = TRUE
#       ),
#       info = "One acquiring domain EIC should be provided."
#     )
#     testthat::expect_error(
#       object = sharing_of_frr_capacity(
#         eic_acquiring = "10YCB-GERMANY--8",
#         eic_connecting = NULL,
#         process_type = "A56",
#         period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#         period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#         tidy_output = TRUE
#       ),
#       info = "One connecting domain EIC should be provided."
#     )
#     testthat::expect_error(
#       object = sharing_of_frr_capacity(
#         eic_acquiring = c("10YCB-GERMANY--8", "10YDE-VE-------2"),
#         eic_connecting = "10YAT-APG------L",
#         process_type = "A56",
#         period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#         period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#         tidy_output = TRUE
#       ),
#       info = paste(
#         "This wrapper only supports one acquiring and",
#         "one connecting EIC per request."
#       )
#     )
#     testthat::expect_error(
#       object = sharing_of_frr_capacity(
#         eic_acquiring = "10YCB-GERMANY--8",
#         eic_connecting = "10YAT-APG------L",
#         process_type = "INVALID",
#         period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#         period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#         tidy_output = TRUE
#       ),
#       info = "The 'process_type' should be 'A56' (FRR) or 'A46' (RR)."
#     )
#     testthat::expect_error(
#       object = sharing_of_frr_capacity(
#         eic_acquiring = "10YCB-GERMANY--8",
#         eic_connecting = "10YAT-APG------L",
#         process_type = "A56",
#         period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#         period_end = lubridate::ymd(x = "2023-05-01", tz = "CET"),
#         tidy_output = TRUE
#       ),
#       info = "One year range limit should be applied!"
#     )
#     testthat::expect_error(
#       object = sharing_of_frr_capacity(
#         eic_acquiring = "10YCB-GERMANY--8",
#         eic_connecting = "10YAT-APG------L",
#         process_type = "A56",
#         period_start = lubridate::ymd(x = "2022-01-01", tz = "CET"),
#         period_end = lubridate::ymd(x = "2022-01-02", tz = "CET"),
#         tidy_output = TRUE,
#         security_token = ""
#       ),
#       info = "Valid security token should be provided."
#     )
#   }
# )
