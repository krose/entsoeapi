testthat::test_that(
  desc = "all_approved_eic() works",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = all_approved_eic())
    tbl <- all_approved_eic()
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
      info = "Error in all_approved_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "party_eic() works",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = party_eic())
    tbl <- party_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "X"))
    testthat::expect_error(
      object = party_eic("foo"),
      info = "Error in party_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "area_eic() works",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = area_eic())
    tbl <- area_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "Y"))
    testthat::expect_error(
      object = area_eic("foo"),
      info = "Error in area_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "accounting_point_eic() works",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = accounting_point_eic())
    tbl <- accounting_point_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "Z"))
    testthat::expect_error(
      object = accounting_point_eic("foo"),
      info = "Error in accounting_point_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "tie_line_eic() works",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = tie_line_eic())
    tbl <- tie_line_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "T"))
    testthat::expect_error(
      object = tie_line_eic("foo"),
      info = "Error in tie_line_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "location_eic() works",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = location_eic())
    tbl <- location_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "V"))
    testthat::expect_error(
      object = location_eic("foo"),
      info = "Error in location_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "resource_object_eic() works",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = resource_object_eic())
    tbl <- resource_object_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "W"))
    testthat::expect_error(
      object = resource_object_eic("foo"),
      info = "Error in resource_object_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "substation_eic() works",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = substation_eic())
    tbl <- substation_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "A"))
    testthat::expect_error(
      object = substation_eic("foo"),
      info = "Error in substation_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() responses got and appended into a tibble",
  code = {
    entsoeapi:::mh$reset()
    testthat::expect_no_error(object = all_allocated_eic())
    tbl <- all_allocated_eic()
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
      info = "Error in all_allocated_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() errors with unknown content-type",
  code = {
    entsoeapi:::mh$reset()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "text/csv"),
          body = raw(0L)
        )
      }
    )
    testthat::expect_error(
      object = all_allocated_eic(),
      regexp = "Can't retrieve empty body"
    )
  }
)



testthat::test_that(
  desc = "all_allocated_eic() errors on HTTP error response",
  code = {
    entsoeapi:::mh$reset()
    xml_error_body <- charToRaw(paste0(
      '<?xml version="1.0" encoding="utf-8"?>',
      "<root><Reason>Service Unavailable</Reason></root>"
    ))
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
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
    entsoeapi:::mh$reset()
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
          headers = list("content-type" = "application/xml"),
          body = xml_fixture
        )
      }
    )
    tbl <- all_allocated_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
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
    entsoeapi:::mh$reset()
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
          headers = list("content-type" = "application/zip"),
          body = xml_fixture
        )
      }
    )
    tbl <- all_allocated_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
  }
)



testthat::test_that(
  desc = "all_allocated_eic() stops on XML with unexpected structure",
  code = {
    entsoeapi:::mh$reset()
    # Minimal XML with no EICCode_MarketDocument second-level nodes; the
    # setnames() call then fails because the expected EIC column names are
    # absent, exercising the error path after XML parsing.
    minimal_xml <- charToRaw(x = paste0(
      '<?xml version="1.0" encoding="UTF-8"?>',
      "<root>",
      "  <docStatusValue>A05</docStatusValue>",
      "  <simpleLeaf>value</simpleLeaf>",
      "</root>"
    ))
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = minimal_xml
        )
      }
    )
    testthat::expect_error(
      object = all_allocated_eic(),
      regexp = "The XML document has an unexpected tree structure"
    )
  }
)
