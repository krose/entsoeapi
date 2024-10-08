testthat::test_that(
  desc = "outages_both() works",
  code = {
    testthat::expect_no_error(
      object = outages_both(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      )
    )
    testthat::expect_vector(
      object = outages_both(
        eic = "ABC",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      ),
      ptype = NULL,
      size = 0
    )
  }
)



testthat::test_that(
  desc = "outages_gen_units() works",
  code = {
    testthat::expect_no_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        doc_status = "A05",
        event_nature = "A54",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        doc_status = "A05",
        event_nature = "A54",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        doc_status = "A05",
        event_nature = "A54",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 400L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = c("10YFR-RTE------C", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = outages_gen_units(
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One control area EIC should be provided!"
    )
  }
)



testthat::test_that(
  desc = "outages_prod_units() works",
  code = {
    testthat::expect_no_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 400L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = c("10YFR-RTE------C", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = outages_prod_units(
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One control area EIC should be provided!"
    )
  }
)



testthat::test_that(
  desc = "outages_offshore_grid() works",
  code = {
    testthat::expect_no_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        doc_status = "A09",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        doc_status = "A09",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        doc_status = "A09",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 400L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = c("10Y1001A1001A82H", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One control area EIC should be provided!"
    )
  }
)



testthat::test_that(
  desc = "outages_cons_units() works",
  code = {
    testthat::expect_no_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 400L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = c("10YFI-1--------U", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = outages_cons_units(
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One control area EIC should be provided!"
    )
  }
)



testthat::test_that(
  desc = "outages_transmission_grid() works",
  code = {
    testthat::expect_no_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = NULL,
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One OUT control area EIC should be provided!"
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = NULL,
        eic_out = "10Y1001A1001A82H",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One IN control area EIC should be provided!"
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 400L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = c("10YFR-RTE------C", "45Y000000000001C"),
        eic_out = c("10Y1001A1001A82H"),
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one in control area EIC per request!"
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = c("10Y1001A1001A82H"),
        eic_out = c("10YFR-RTE------C", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one out control area EIC per request!"
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        period_start_update = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 7L),
          tz = "CET"),
        period_end_update = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One control area EIC should be provided!"
    )
  }
)



testthat::test_that(
  desc = "outages_fallbacks() works",
  code = {
    testthat::expect_no_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        process_type = "A63",
        event_nature = "C47",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        tidy_output = FALSE
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        process_type = "A63",
        event_nature = "C47",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = ""
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        process_type = "A01",
        event_nature = "C47",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = paste(
        "The process_type value should be chosen among",
        "'A47', 'A51' or 'A63'!"
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        process_type = "A63",
        event_nature = "C01",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 8),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = paste(
        "The event_nature value should be chosen among",
        "'C47', 'A53', 'A54' or '83'!"
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 400L),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One year range limit should be applied!"
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        tidy_output = FALSE,
        security_token = "ABC"
      ),
      info = "Valid security token should be provided!"
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = c("10YBE----------2", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "This wrapper only supports one control area EIC per request!"
    )
    testthat::expect_error(
      object = outages_fallbacks(
        period_start = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 1L),
          tz = "CET"),
        period_end = lubridate::ymd(
          x = Sys.Date() + lubridate::days(x = 2L),
          tz = "CET"),
        tidy_output = FALSE
      ),
      info = "One control area EIC should be provided!"
    )
  }
)
