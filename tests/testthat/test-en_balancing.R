testthat::test_that(
  desc = "elastic_demands() works",
  code = {
    testthat::expect_no_error(
      object = elastic_demands(
        eic = "10YCZ-CEPS-----N",
        process_type = "A47",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-12-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ),
      message = "uses offsetting"
    ) |>
      testthat::expect_warning()
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
    ) |>
      testthat::expect_warning()
    testthat::expect_contains(
      object = elastic_demands(
        eic = "10YCZ-CEPS-----N",
        process_type = "A47",
        period_start = lubridate::ymd(
          x = "2024-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-12-01",
          tz = "CET"
        ),
        tidy_output = TRUE
      ) |>
        names(),
      expected = c(
        "bid_ts_mrid",
        "bid_ts_auction_mrid",
        "bid_ts_flow_direction"
      )
    ) |>
      testthat::expect_warning()
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
    ) |>
      testthat::expect_warning()
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
    ) |>
      testthat::expect_warning()
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
    ) |>
      testthat::expect_warning() |>
      testthat::expect_warning()
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
  }
)



# testthat::test_that(
#   desc = "balancing_accepted_aggr_offers() works",
#   code = {
#     testthat::expect_no_error(
#       object = balancing_accepted_aggr_offers(
#         eic = "10YHU-MAVIR----U",
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2020-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE
#       )
#     )
#     testthat::expect_error(
#       object = balancing_accepted_aggr_offers(
#         eic = NULL,
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2020-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE
#       ),
#       info = "One control area EIC should be provided!"
#     )
#     testthat::expect_error(
#       object = balancing_accepted_aggr_offers(
#         eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2020-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE
#       ),
#       info = "This wrapper only supports one control area EIC per request!"
#     )
#     testthat::expect_error(
#       object = balancing_accepted_aggr_offers(
#         eic = "10YHU-MAVIR----U",
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2021-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE
#       ),
#       info = "One year range limit should be applied!"
#     )
#     testthat::expect_error(
#       object = balancing_accepted_aggr_offers(
#         eic = "10YHU-MAVIR----U",
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2020-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE,
#         security_token = ""
#       ),
#       info = "Valid security token should be provided!"
#     )
#   }
# )



# testthat::test_that(
#   desc = "balancing_activated_reserves() works",
#   code = {
#     testthat::expect_no_error(
#       object = balancing_activated_reserves(
#         eic = "10YHU-MAVIR----U",
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2020-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE
#       )
#     )
#     testthat::expect_error(
#       object = balancing_activated_reserves(
#         eic = NULL,
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2020-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE
#       ),
#       info = "One control area EIC should be provided!"
#     )
#     testthat::expect_error(
#       object = balancing_activated_reserves(
#         eic = c("10YHU-MAVIR----U", "10Y1001A1001A83F"),
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2020-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE
#       ),
#       info = "This wrapper only supports one control area EIC per request!"
#     )
#     testthat::expect_error(
#       object = balancing_activated_reserves(
#         eic = "10YHU-MAVIR----U",
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2021-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE
#       ),
#       info = "One year range limit should be applied!"
#     )
#     testthat::expect_error(
#       object = balancing_activated_reserves(
#         eic = "10YHU-MAVIR----U",
#         period_start = lubridate::ymd(
#           x = "2020-02-01",
#           tz = "CET"
#         ),
#         period_end = lubridate::ymd(
#           x = "2020-03-01",
#           tz = "CET"
#         ),
#         reserve_type = "A96",
#         tidy_output = TRUE,
#         security_token = ""
#       ),
#       info = "Valid security token should be provided!"
#     )
#   }
# )
