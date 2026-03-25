testthat::test_that(
  desc = "all_approved_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = all_approved_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "all_approved_eic() works",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(object = tbl <- all_approved_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_false(object = anyNA(tbl$type))
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "eic_code",
        "eic_display_name",
        "eic_long_name",
        "eic_parent",
        "eic_responsible_party",
        "eic_status",
        "market_participant_iso_country_code",
        "market_participant_postal_code",
        "market_participant_vat_code",
        "eic_type_function_list",
        "type"
      )
    )
  }
)


testthat::test_that(
  desc = "party_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = party_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "party_eic() works",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(object = tbl <- party_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "eic_code",
        "eic_display_name",
        "eic_long_name",
        "eic_parent",
        "eic_responsible_party",
        "eic_status",
        "market_participant_iso_country_code",
        "market_participant_postal_code",
        "market_participant_vat_code",
        "eic_type_function_list",
        "type"
      )
    )
    testthat::expect_true(object = all(tbl$type == "X"))
  }
)


testthat::test_that(
  desc = "area_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = area_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "area_eic() works",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(object = tbl <- area_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "eic_code",
        "eic_display_name",
        "eic_long_name",
        "eic_parent",
        "eic_responsible_party",
        "eic_status",
        "market_participant_iso_country_code",
        "market_participant_postal_code",
        "market_participant_vat_code",
        "eic_type_function_list",
        "type"
      )
    )
    testthat::expect_true(object = all(tbl$type == "Y"))
  }
)


testthat::test_that(
  desc = "accounting_point_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = accounting_point_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "accounting_point_eic() works",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(object = tbl <- accounting_point_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "eic_code",
        "eic_display_name",
        "eic_long_name",
        "eic_parent",
        "eic_responsible_party",
        "eic_status",
        "market_participant_iso_country_code",
        "market_participant_postal_code",
        "market_participant_vat_code",
        "eic_type_function_list",
        "type"
      )
    )
    testthat::expect_true(object = all(tbl$type == "Z"))
  }
)


testthat::test_that(
  desc = "tie_line_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = tie_line_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "tie_line_eic() works",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(object = tbl <- tie_line_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "eic_code",
        "eic_display_name",
        "eic_long_name",
        "eic_parent",
        "eic_responsible_party",
        "eic_status",
        "market_participant_iso_country_code",
        "market_participant_postal_code",
        "market_participant_vat_code",
        "eic_type_function_list",
        "type"
      )
    )
    testthat::expect_true(object = all(tbl$type == "T"))
  }
)


testthat::test_that(
  desc = "location_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = location_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "location_eic() works",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(object = tbl <- location_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "eic_code",
        "eic_display_name",
        "eic_long_name",
        "eic_parent",
        "eic_responsible_party",
        "eic_status",
        "market_participant_iso_country_code",
        "market_participant_postal_code",
        "market_participant_vat_code",
        "eic_type_function_list",
        "type"
      )
    )
    testthat::expect_true(object = all(tbl$type == "V"))
  }
)


testthat::test_that(
  desc = "resource_object_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = resource_object_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "resource_object_eic() works",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(object = tbl <- resource_object_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "eic_code",
        "eic_display_name",
        "eic_long_name",
        "eic_parent",
        "eic_responsible_party",
        "eic_status",
        "market_participant_iso_country_code",
        "market_participant_postal_code",
        "market_participant_vat_code",
        "eic_type_function_list",
        "type"
      )
    )
    testthat::expect_true(object = all(tbl$type == "W"))
  }
)


testthat::test_that(
  desc = "substation_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = substation_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "substation_eic() works",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    testthat::expect_no_error(object = tbl <- substation_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("eic_code", "eic_long_name", "type")
    )
    testthat::expect_true(object = all(tbl$type == "A"))
  }
)


testthat::test_that(
  desc = "all_allocated_eic() validates inputs",
  code = {
    testthat::expect_error(
      object = all_allocated_eic(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "all_allocated_eic() responses got and appended into a tibble",
  code = {
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    m$reset()
    testthat::expect_no_error(object = tbl <- all_allocated_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_false(object = anyNA(tbl$created_date_time))
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "revision_number",
        "created_date_time",
        "eic_code",
        "doc_status_value",
        "doc_status",
        "instance_component_attribute",
        "long_name",
        "display_name",
        "last_request_date",
        "deactivation_requested_date_and_or_time_date",
        "eic_code_market_participant_street_address",
        "market_participant_vat_code_name",
        "market_participant_acer_code_name",
        "description",
        "responsible_market_participant_mrid",
        "function_names",
        "parent_market_document_mrid"
      )
    )
  }
)


testthat::test_that(
  desc = "all_allocated_eic() errors with empty response body",
  code = {
    mock_list <- list()
    mock_list[[1L]] <- httr2::response(
      status_code = 200L,
      url = paste0(
        "https://eepublicdownloads.blob.core.windows.net/",
        "cio-lio/xml/allocated-eic-codes.xml"
      ),
      headers = list("content-type" = "application/xml"),
      body = raw(0L)
    )
    httr2::with_mocked_responses(
      mock = mock_list,
      code = {
        m$reset()
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
    m$reset()
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "error_body.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = xml_fixture
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
    m$reset()
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
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c(
        "eic_code",
        "doc_status",
        "last_request_date",
        "instance_component_attribute"
      )
    )
    testthat::expect_contains(object = names(tbl), expected = "function_names")
  }
)


testthat::test_that(
  desc = "all_allocated_eic() parses ZIP/octet-stream content-type response",
  code = {
    m$reset()
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
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
  }
)


testthat::test_that(
  desc = "all_allocated_eic() stops on XML with unexpected structure",
  code = {
    # Minimal XML with no EICCode_MarketDocument second-level nodes; the
    # setnames() call then fails because the expected EIC column names are
    # absent, exercising the error path after XML parsing.
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "unexpected_structure.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    mock_list <- list()
    mock_list[[1L]] <- httr2::response(
      status_code = 200L,
      url = paste0(
        "https://eepublicdownloads.blob.core.windows.net/",
        "cio-lio/xml/allocated-eic-codes.xml"
      ),
      headers = list("content-type" = "application/xml"),
      body = xml_fixture
    )
    httr2::with_mocked_responses(
      mock = mock_list,
      code = {
        m$reset()
        testthat::expect_error(
          object = all_allocated_eic(),
          regexp = "The XML document has an unexpected tree structure"
        )
      }
    )
  }
)


testthat::test_that(
  desc = "all_allocated_eic() uses cache on second call",
  code = {
    m$reset()
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "get_allocated_eic_min.xml"),
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
    # First call populates the cache
    tbl1 <- all_allocated_eic()
    # Second call hits the cache
    tbl2 <- all_allocated_eic()
    testthat::expect_identical(object = tbl1, expected = tbl2)
  }
)
