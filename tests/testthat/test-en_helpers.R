testthat::test_that(
  desc = "fetch_eic_csv() works",
  code = {
    m$reset()
    fake_eic <- data.frame(
      eic_code = "10YDE-VE-------2",
      eic_display_name = "DE(50HzT)",
      eic_long_name = "50Hertz Transmission",
      eic_parent = NA_character_,
      eic_responsible_party = NA_character_,
      eic_status = "Active",
      market_participant_postal_code = NA_character_,
      market_participant_iso_country_code = "DE",
      market_participant_vat_code = NA_character_,
      eic_type_function_list = NA_character_,
      type = "T",
      stringsAsFactors = FALSE
    ) |> as_tbl()
    testthat::local_mocked_bindings(
      get_eiccodes = function(...) fake_eic,
      .package = "entsoeapi"
    )
    testthat::expect_no_error(
      object = result <- fetch_eic_csv(csv_file = "T_eicCodes.csv")
    )
    testthat::expect_s3_class(
      object = result,
      class = "tbl_df"
    )
    testthat::expect_contains(
      object = names(result),
      expected = c("eic_code", "eic_display_name", "eic_long_name")
    )
  }
)


testthat::test_that(
  desc = "fetch_eic_csv() validates inputs",
  code = {
    testthat::expect_error(
      object = fetch_eic_csv(csv_file = "foo.csv"),
      regexp = "Assertion on 'csv_file' failed\\: Must be element of set"
    )
  }
)


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
    m$reset()
    make_fake_eic <- function(type_val) {
      data.frame(
        eic_code = paste0("10Y-FAKE-", type_val),
        eic_display_name = paste("Fake", type_val),
        eic_long_name = paste("Fake Long", type_val),
        eic_parent = NA_character_,
        eic_responsible_party = NA_character_,
        eic_status = "Active",
        market_participant_iso_country_code = "DE",
        market_participant_postal_code = NA_character_,
        market_participant_vat_code = NA_character_,
        eic_type_function_list = NA_character_,
        type = type_val,
        stringsAsFactors = FALSE
      ) |> as_tbl()
    }
    testthat::local_mocked_bindings(
      get_eiccodes = function(f, ...) {
        type_val <- switch(
          f,
          "X_eicCodes.csv" = "X", "Y_eicCodes.csv" = "Y",
          "Z_eicCodes.csv" = "Z", "T_eicCodes.csv" = "T",
          "V_eicCodes.csv" = "V", "W_eicCodes.csv" = "W",
          "A_eicCodes.csv" = "A"
        )
        make_fake_eic(type_val)
      },
      .package = "entsoeapi"
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
    m$reset()
    fake_eic <- data.frame(
      eic_code = "10X-FAKE-PARTY-1",
      eic_display_name = "Fake Party",
      eic_long_name = "Fake Party Long",
      eic_parent = NA_character_,
      eic_responsible_party = NA_character_,
      eic_status = "Active",
      market_participant_iso_country_code = "DE",
      market_participant_postal_code = NA_character_,
      market_participant_vat_code = NA_character_,
      eic_type_function_list = NA_character_,
      type = "X",
      stringsAsFactors = FALSE
    ) |> as_tbl()
    testthat::local_mocked_bindings(
      get_eiccodes = function(...) fake_eic,
      .package = "entsoeapi"
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
    m$reset()
    fake_eic <- data.frame(
      eic_code = "10Y-FAKE-AREA--1",
      eic_display_name = "Fake Area",
      eic_long_name = "Fake Area Long",
      eic_parent = NA_character_,
      eic_responsible_party = NA_character_,
      eic_status = "Active",
      market_participant_iso_country_code = "DE",
      market_participant_postal_code = NA_character_,
      market_participant_vat_code = NA_character_,
      eic_type_function_list = NA_character_,
      type = "Y",
      stringsAsFactors = FALSE
    ) |> as_tbl()
    testthat::local_mocked_bindings(
      get_eiccodes = function(...) fake_eic,
      .package = "entsoeapi"
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
    m$reset()
    fake_eic <- data.frame(
      eic_code = "10Z-FAKE-ACCP-01",
      eic_display_name = "Fake AccPt",
      eic_long_name = "Fake AccPt Long",
      eic_parent = NA_character_,
      eic_responsible_party = NA_character_,
      eic_status = "Active",
      market_participant_iso_country_code = "DE",
      market_participant_postal_code = NA_character_,
      market_participant_vat_code = NA_character_,
      eic_type_function_list = NA_character_,
      type = "Z",
      stringsAsFactors = FALSE
    ) |> as_tbl()
    testthat::local_mocked_bindings(
      get_eiccodes = function(...) fake_eic,
      .package = "entsoeapi"
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
    m$reset()
    fake_eic <- data.frame(
      eic_code = "10T-FAKE-TIE---1",
      eic_display_name = "Fake Tie",
      eic_long_name = "Fake Tie Long",
      eic_parent = NA_character_,
      eic_responsible_party = NA_character_,
      eic_status = "Active",
      market_participant_iso_country_code = "DE",
      market_participant_postal_code = NA_character_,
      market_participant_vat_code = NA_character_,
      eic_type_function_list = NA_character_,
      type = "T",
      stringsAsFactors = FALSE
    ) |> as_tbl()
    testthat::local_mocked_bindings(
      get_eiccodes = function(...) fake_eic,
      .package = "entsoeapi"
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
    m$reset()
    fake_eic <- data.frame(
      eic_code = "10V-FAKE-LOC---1",
      eic_display_name = "Fake Loc",
      eic_long_name = "Fake Loc Long",
      eic_parent = NA_character_,
      eic_responsible_party = NA_character_,
      eic_status = "Active",
      market_participant_iso_country_code = "DE",
      market_participant_postal_code = NA_character_,
      market_participant_vat_code = NA_character_,
      eic_type_function_list = NA_character_,
      type = "V",
      stringsAsFactors = FALSE
    ) |> as_tbl()
    testthat::local_mocked_bindings(
      get_eiccodes = function(...) fake_eic,
      .package = "entsoeapi"
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
    m$reset()
    fake_eic <- data.frame(
      eic_code = "10W-FAKE-RESRC-1",
      eic_display_name = "Fake Resource",
      eic_long_name = "Fake Resource Long",
      eic_parent = NA_character_,
      eic_responsible_party = NA_character_,
      eic_status = "Active",
      market_participant_iso_country_code = "DE",
      market_participant_postal_code = NA_character_,
      market_participant_vat_code = NA_character_,
      eic_type_function_list = NA_character_,
      type = "W",
      stringsAsFactors = FALSE
    ) |> as_tbl()
    testthat::local_mocked_bindings(
      get_eiccodes = function(...) fake_eic,
      .package = "entsoeapi"
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
    m$reset()
    fake_eic <- data.frame(
      eic_code = "10A-FAKE-SUBST-1",
      eic_display_name = "Fake Sub",
      eic_long_name = "Fake Sub Long",
      eic_parent = NA_character_,
      eic_responsible_party = NA_character_,
      eic_status = "Active",
      market_participant_iso_country_code = "DE",
      market_participant_postal_code = NA_character_,
      market_participant_vat_code = NA_character_,
      eic_type_function_list = NA_character_,
      type = "A",
      stringsAsFactors = FALSE
    ) |> as_tbl()
    testthat::local_mocked_bindings(
      get_eiccodes = function(...) fake_eic,
      .package = "entsoeapi"
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
  desc = "get_news() validates inputs",
  code = {
    testthat::expect_error(
      object = get_news(url = "foo"),
      regexp = 'unused argument \\(url = "foo"\\)'
    )
  }
)


testthat::test_that(
  desc = "get_news() returns a tibble with expected columns",
  code = {
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "news_feed.xml"),
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
    tbl <- get_news()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_setequal(
      object = names(tbl),
      expected = c("title", "pub_date", "description")
    )
    testthat::expect_equal(object = nrow(tbl), expected = 3L)
    testthat::expect_equal(
      object = tbl$title[[1L]],
      expected = "TP PROD data publication delays"
    )
    testthat::expect_equal(
      object = tbl$description[[1L]],
      expected = "Some data publications are delayed."
    )
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
    testthat::expect_no_error(object = tbl <- all_allocated_eic())
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_false(object = anyNA(tbl$created_date_time))
    testthat::expect_contains(
      object = names(tbl),
      expected = c(
        "eic_code",
        "doc_status",
        "created_date_time",
        "last_request_date",
        "instance_component_attribute",
        "function_names"
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


testthat::test_that(
  desc = "all_allocated_eic() stops on curl/network error",
  code = {
    m$reset()
    curl_err <- structure(
      class = c("curl_error", "error", "condition"),
      list(
        message = paste(
          "Could not resolve host:",
          "eepublicdownloads.blob.core.windows.net"
        )
      )
    )
    httr2_err <- structure(
      class = c("httr2_failure", "httr2_error", "error", "condition"),
      list(
        message = "Failed to perform HTTP request.",
        resp = NULL,
        parent = curl_err
      )
    )
    httr2::local_mocked_responses(
      mock = function(req) stop(httr2_err)
    )
    testthat::expect_error(
      object = all_allocated_eic(),
      regexp = "Failed to perform HTTP request"
    )
  }
)


testthat::test_that(
  desc = "all_allocated_eic() returns correct number of rows",
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
    tbl <- all_allocated_eic()
    testthat::expect_equal(object = nrow(tbl), expected = 2L)
  }
)


testthat::test_that(
  desc = "all_allocated_eic() handles duplicate Function_Names with separator",
  code = {
    m$reset()
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "get_allocated_eic_dupl.xml"),
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
    tbl <- all_allocated_eic()
    testthat::expect_true(
      object = tbl$function_names[[1L]] |>
        str_detect(pattern = " - ")
    )
  }
)


testthat::test_that(
  desc = "all_allocated_eic() stops when bind_cols fails",
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
    testthat::local_mocked_bindings(
      bind_cols = function(...) stop("mocked bind_cols error"),
      .package = "entsoeapi"
    )
    testthat::expect_error(
      object = all_allocated_eic(),
      regexp = "unexpected tree structure"
    )
  }
)


testthat::test_that(
  desc = "get_news() falls back to raw text when read_html() fails",
  code = {
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "news_feed.xml"),
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
    testthat::local_mocked_bindings(
      read_html = function(...) stop("mocked read_html failure"),
      .package = "entsoeapi"
    )
    tbl <- testthat::expect_no_error(
      object = suppressMessages(get_news(n = 1L))
    )
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_equal(object = nrow(tbl), expected = 1L)
    testthat::expect_equal(
      object = tbl$description[[1L]],
      expected = "<p>Some data publications are <strong>delayed</strong>.</p>"
    )
  }
)
