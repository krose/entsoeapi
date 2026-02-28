testthat::test_that(
  desc = "all_approved_eic() works",
  code = {
    testthat::expect_no_error(
      object = tbl <- all_approved_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_equal(
      object = anyNA(tbl$type),
      expected = FALSE
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName")
    )
    testthat::expect_error(
      object = all_approved_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "party_eic() works",
  code = {
    testthat::expect_no_error(
      object = tbl <- party_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(
      object = all(tbl$type == "X")
    )
    testthat::expect_error(
      object = party_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "area_eic() works",
  code = {
    testthat::expect_no_error(
      object = tbl <- area_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(
      object = all(tbl$type == "Y")
    )
    testthat::expect_error(
      object = area_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "accounting_point_eic() works",
  code = {
    testthat::expect_no_error(
      object = tbl <- accounting_point_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(
      object = all(tbl$type == "Z")
    )
    testthat::expect_error(
      object = accounting_point_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "tie_line_eic() works",
  code = {
    testthat::expect_no_error(
      object = tbl <- tie_line_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(
      object = all(tbl$type == "T")
    )
    testthat::expect_error(
      object = tie_line_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "location_eic() works",
  code = {
    testthat::expect_no_error(
      object = tbl <- location_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(
      object = all(tbl$type == "V")
    )
    testthat::expect_error(
      object = location_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "resource_object_eic() works",
  code = {
    testthat::expect_no_error(
      object = tbl <- resource_object_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(
      object = all(tbl$type == "W")
    )
    testthat::expect_error(
      object = resource_object_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "substation_eic() works",
  code = {
    testthat::expect_no_error(
      object = tbl <- substation_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(
      object = all(tbl$type == "A")
    )
    testthat::expect_error(
      object = substation_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() responses got and appended into a tibble",
  code = {
    mh$reset()
    testthat::expect_no_error(
      object = tbl <- all_allocated_eic()
    )
    testthat::expect_s3_class(
      object = tbl,
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_equal(
      object = anyNA(tbl$created_date_time),
      expected = FALSE
    )
    testthat::expect_contains(
      object = names(tbl),
      expected = c(
        "eic_code",
        "long_name",
        "display_name"
      )
    )
    testthat::expect_error(
      object = all_allocated_eic("foo"),
      regexp = 'unused argument \\("foo"\\)'
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() errors with empty response body",
  code = {
    req_url <- paste0(
      "https://eepublicdownloads.blob.core.windows.net/",
      "cio-lio/xml/allocated-eic-codes.xml"
    )
    mock_list <- list()
    mock_list[[req_url]] <- httr2::response(
      status_code = 200L,
      url = req_url,
      headers = list("content-type" = "application/xml"),
      body = raw(0L)
    )
    httr2::with_mocked_responses(
      mock = mock_list,
      code = {
        mh$reset()
        testthat::expect_error(
          object = all_allocated_eic(),
          regexp = "Can't retrieve empty body"
        )
      }
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() errors on HTTP error response",
  code = {
    mh$reset()
    xml_error_body <- paste0(
      '<?xml version="1.0" encoding="utf-8"?>',
      "<root><Reason>Service Unavailable</Reason></root>"
    ) |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = xml_error_body
        )
      }
    )
    testthat::expect_error(
      object = all_allocated_eic(),
      regexp = "HTTP 503 Service Unavailable"
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() parses XML content-type response",
  code = {
    mh$reset()
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "allocated_eic_min.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 200L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = xml_fixture
        )
      }
    )
    testthat::expect_s3_class(
      object = tbl <- all_allocated_eic(),
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_contains(
      object   = names(tbl),
      expected = c(
        "eic_code",
        "doc_status",
        "last_request_date",
        "instance_component_attribute"
      )
    )
    testthat::expect_contains(
      object   = names(tbl),
      expected = "function_names"
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() parses ZIP/octet-stream content-type response",
  code = {
    mh$reset()
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "allocated_eic_min.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 200L,
          url = req$url,
          headers = list("content-type" = "application/zip"),
          body = xml_fixture
        )
      }
    )
    testthat::expect_s3_class(
      object = tbl <- all_allocated_eic(),
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() stops on XML with unexpected structure",
  code = {
    req_url <- paste0(
      "https://eepublicdownloads.blob.core.windows.net/",
      "cio-lio/xml/allocated-eic-codes.xml"
    )
    mock_list <- list()
    mock_list[[req_url]] <- httr2::response(
      status_code = 200L,
      url = req_url,
      headers = list("content-type" = "application/xml"),
      # Minimal XML with no EICCode_MarketDocument second-level nodes; the
      # setnames() call then fails because the expected EIC column names are
      # absent, exercising the error path after XML parsing.
      body = paste0(
        '<?xml version="1.0" encoding="UTF-8"?>',
        "<root>",
        "  <docStatusValue>A05</docStatusValue>",
        "  <simpleLeaf>value</simpleLeaf>",
        "</root>"
      ) |>
        charToRaw()
    )
    httr2::with_mocked_responses(
      mock = mock_list,
      code = {
        mh$reset()
        testthat::expect_error(
          object = all_allocated_eic(),
          regexp = "The XML document has an unexpected tree structure"
        )
      }
    )
  }
)
