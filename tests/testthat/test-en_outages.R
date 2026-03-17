testthat::test_that(
  desc = "outages_both() validates inputs",
  code = {
    testthat::expect_error(
      object = outages_both(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-11-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    ) |>
      testthat::expect_error()
  }
)


testthat::test_that(
  desc = "outages_both() works",
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
      object = outages_both(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
    testthat::expect_no_error(
      object = outages_both(
        eic = "ABCDEFGHIJKLMNOP",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = "outages_gen_units() validates inputs",
  code = {
    testthat::expect_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        doc_status = "ABC",
        event_nature = "A54",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        doc_status = "A05",
        event_nature = "A33",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        doc_status = "A05",
        event_nature = "A54",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-11-26",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = outages_gen_units(
        eic = c("10YFR-RTE------C", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_gen_units(
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "outages_gen_units() works",
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
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        doc_status = "A05",
        event_nature = "A54",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        doc_status = "A05",
        event_nature = "A54",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "outages_prod_units() validates inputs",
  code = {
    testthat::expect_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        doc_status = "ABC",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        doc_status = "A09",
        event_nature = "ABC",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-11-26",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      )
    )
    testthat::expect_error(
      object = outages_prod_units(
        eic = c("10YFR-RTE------C", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_prod_units(
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "outages_prod_units() works",
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
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "outages_offshore_grid() validates inputs",
  code = {
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        doc_status = "ABC",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        doc_status = "A05",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-11-26",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      )
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        eic = c("10Y1001A1001A82H", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_offshore_grid(
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "outages_offshore_grid() works",
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
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        doc_status = "A05",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        doc_status = "A05",
        period_start = lubridate::ymd(
          x = "2025-01-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-01-30",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "outages_cons_units() validates inputs",
  code = {
    testthat::expect_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        doc_status = "ABC",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        doc_status = "A09",
        event_nature = "ABC",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-11-26",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      )
    )
    testthat::expect_error(
      object = outages_cons_units(
        eic = c("10YFI-1--------U", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_cons_units(
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "outages_cons_units() works",
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
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "outages_transmission_grid() validates inputs",
  code = {
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        doc_status = "ABC",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        doc_status = "A09",
        event_nature = "ABC",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = NULL,
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = NULL,
        eic_out = "10Y1001A1001A82H",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-11-26",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = c("10YFR-RTE------C", "45Y000000000001C"),
        eic_out = c("10Y1001A1001A82H"),
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        eic_in = c("10Y1001A1001A82H"),
        eic_out = c("10YFR-RTE------C", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_transmission_grid(
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "outages_transmission_grid() works",
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
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        doc_status = "A09",
        event_nature = "A53",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        period_start_update = lubridate::ymd(
          x = "2024-10-15",
          tz = "CET"
        ),
        period_end_update = lubridate::ymd(
          x = "2024-10-22",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "outages_fallbacks() validates inputs",
  code = {
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        process_type = "A63",
        event_nature = "C47",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        process_type = "A01",
        event_nature = "C47",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        process_type = "A63",
        event_nature = "C01",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2025-11-26",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = "10YBE----------2",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "ABC"
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        eic = c("10YBE----------2", "45Y000000000001C"),
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = outages_fallbacks(
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-24",
          tz = "CET"
        ),
        tidy_output = FALSE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "outages_fallbacks() works",
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
      object = outages_fallbacks(
        eic = "10YBE----------2",
        process_type = "A63",
        event_nature = "C47",
        period_start = lubridate::ymd(
          x = "2024-10-23",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-10-30",
          tz = "CET"
        ),
        tidy_output = FALSE
      )
    )
  }
)


testthat::test_that(
  desc = "outages_gen_units() covers happy path with mock",
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
      object = outages_gen_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "outages_prod_units() covers happy path with mock",
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
      object = outages_prod_units(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "outages_both() covers happy path with mock",
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
    testthat::expect_no_error(
      object = outages_both(
        eic = "10YFR-RTE------C",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "outages_offshore_grid() covers happy path with mock",
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
      object = outages_offshore_grid(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "outages_cons_units() covers happy path with mock",
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
      object = outages_cons_units(
        eic = "10YFI-1--------U",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "outages_transmission_grid() covers happy path with mock",
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
      object = outages_transmission_grid(
        eic_in = "10YFR-RTE------C",
        eic_out = "10Y1001A1001A82H",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "outages_fallbacks() covers happy path with mock",
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
      object = outages_fallbacks(
        eic = "10YBE----------2",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)
