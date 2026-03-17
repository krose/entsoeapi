testthat::test_that(
  desc = "load_actual_total() validates inputs",
  code = {
    testthat::expect_error(
      object = load_actual_total(
        eic = NULL,
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_actual_total(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_actual_total(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = load_actual_total(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 390),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "load_actual_total() works and returns valid output structure",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    result <- testthat::expect_no_error(
      object = load_actual_total(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_s3_class(object = result, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(result), expected = 0L)
    testthat::expect_gt(object = ncol(result), expected = 0L)
  }
)


testthat::test_that(
  desc = "load_day_ahead_total_forecast() validates inputs",
  code = {
    testthat::expect_error(
      object = load_day_ahead_total_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_day_ahead_total_forecast(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_day_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = load_day_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 390),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "load_day_ahead_total_forecast() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    result <- testthat::expect_no_error(
      object = load_day_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = Sys.Date() - lubridate::days(x = 30),
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = Sys.Date(),
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_s3_class(object = result, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(result), expected = 0L)
    testthat::expect_gt(object = ncol(result), expected = 0L)
  }
)


testthat::test_that(
  desc = "load_week_ahead_total_forecast() validates inputs",
  code = {
    testthat::expect_error(
      object = load_week_ahead_total_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_week_ahead_total_forecast(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_week_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = load_week_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2018-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "load_week_ahead_total_forecast() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    result <- testthat::expect_no_error(
      object = load_week_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_s3_class(object = result, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(result), expected = 0L)
    testthat::expect_gt(object = ncol(result), expected = 0L)
  }
)


testthat::test_that(
  desc = "load_month_ahead_total_forecast() validates inputs",
  code = {
    testthat::expect_error(
      object = load_month_ahead_total_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_month_ahead_total_forecast(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_month_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = load_month_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "load_month_ahead_total_forecast() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    result <- testthat::expect_no_error(
      object = load_month_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_s3_class(object = result, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(result), expected = 0L)
    testthat::expect_gt(object = ncol(result), expected = 0L)
  }
)


testthat::test_that(
  desc = "load_year_ahead_total_forecast() validates inputs",
  code = {
    testthat::expect_error(
      object = load_year_ahead_total_forecast(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_year_ahead_total_forecast(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_year_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = load_year_ahead_total_forecast(
        eic = "10Y1001A1001A82H",
        period_start = lubridate::ymd(
          x = "2019-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "load_year_ahead_total_forecast() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    result <- testthat::expect_no_error(
      object = load_year_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2024-11-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2024-11-30",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_s3_class(object = result, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(result), expected = 0L)
    testthat::expect_gt(object = ncol(result), expected = 0L)
  }
)


testthat::test_that(
  desc = "load_year_ahead_forecast_margin() validates inputs",
  code = {
    testthat::expect_error(
      object = load_year_ahead_forecast_margin(
        eic = NULL,
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-31",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_year_ahead_forecast_margin(
        eic = c("10Y1001A1001A83F", "10YCZ-CEPS-----N"),
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-31",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
    testthat::expect_error(
      object = load_year_ahead_forecast_margin(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-31",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = ""
      )
    )
    testthat::expect_error(
      object = load_year_ahead_forecast_margin(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2020-12-31",
          tz = "CET"
        ),
        tidy_output = TRUE,
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "load_year_ahead_forecast_margin() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    result <- testthat::expect_no_error(
      object = load_year_ahead_forecast_margin(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(
          x = "2019-01-01",
          tz = "CET"
        ),
        period_end = lubridate::ymd(
          x = "2019-12-31",
          tz = "CET"
        ),
        tidy_output = TRUE
      )
    )
    testthat::expect_s3_class(object = result, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(result), expected = 0L)
    testthat::expect_gt(object = ncol(result), expected = 0L)
  }
)


testthat::test_that(
  desc = "load_actual_total() covers happy path with mock",
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
      object = load_actual_total(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "load_day_ahead_total_forecast() covers happy path with mock",
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
      object = load_day_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "load_week_ahead_total_forecast() covers happy path with mock",
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
      object = load_week_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "load_month_ahead_total_forecast() covers happy path with mock",
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
      object = load_month_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "load_year_ahead_total_forecast() covers happy path with mock",
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
      object = load_year_ahead_total_forecast(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "load_year_ahead_forecast_margin() covers happy path with mock",
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
      object = load_year_ahead_forecast_margin(
        eic = "10Y1001A1001A83F",
        period_start = lubridate::ymd(x = "2024-01-01", tz = "CET"),
        period_end = lubridate::ymd(x = "2024-01-02", tz = "CET"),
        tidy_output = TRUE,
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)
