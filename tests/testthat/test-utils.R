test_df_list <- testthat::test_path("fixtures", "test_df_list.rds") |>
  readRDS()
entsoe_gen_min_raw <- testthat::test_path("fixtures", "entsoe_gen_min.xml") |>
  readLines(encoding = "UTF-8") |>
  paste(collapse = "\n") |>
  charToRaw()
entsoe_gen_min_xml <- testthat::test_path("fixtures", "entsoe_gen_min.xml") |>
  xml2::read_xml()
order_schema_xml <- testthat::test_path("fixtures", "order-schema.xml") |>
  xml2::read_xml()
get_allocated_eic_min_raw <- testthat::test_path(
  "fixtures", "get_allocated_eic_min.xml"
) |>
  readLines(encoding = "UTF-8") |>
  paste(collapse = "\n") |>
  charToRaw()


testthat::test_that(
  desc = "extract_leaf_twig_branch() works",
  code = {
    result <- xml2::xml_contents(entsoe_gen_min_xml) |>
      extract_leaf_twig_branch()
    testthat::expect_s3_class(object = result, class = "tbl")
    testthat::expect_equal(
      object = dim(result),
      expected = c(2L, 22L)
    )
    testthat::expect_setequal(
      object = names(result),
      expected = c(
        "createdDateTime", "mRID", "process.processType",
        "receiver_MarketParticipant.marketRole.type",
        "receiver_MarketParticipant.mRID", "revisionNumber",
        "sender_MarketParticipant.marketRole.type",
        "sender_MarketParticipant.mRID", "time_Period.timeInterval.end",
        "time_Period.timeInterval.start", "TimeSeries.businessType",
        "TimeSeries.curveType", "TimeSeries.inBiddingZone_Domain.mRID",
        "TimeSeries.MktPSRType.psrType", "TimeSeries.mRID",
        "TimeSeries.Period.Point.position", "TimeSeries.Period.Point.quantity",
        "TimeSeries.Period.resolution", "TimeSeries.Period.timeInterval.end",
        "TimeSeries.Period.timeInterval.start",
        "TimeSeries.quantity_Measure_Unit.name", "type"
      )
    )
  }
)


testthat::test_that(
  desc = "extract_leaf_twig_branch() cross-joins unequal-group XML",
  code = {
    testthat::expect_equal(
      object = testthat::test_path("fixtures", "unequal_group.xml") |>
        readLines(encoding = "UTF-8") |>
        paste0(collapse = "") |>
        xml2::read_xml() |>
        xml2::xml_contents() |>
        extract_leaf_twig_branch() |>
        dim(),
      expected = c(6L, 2L)
    )
  }
)


testthat::test_that(
  desc = paste(
    "extract_leaf_twig_branch() returns empty tibble",
    "when both leaf_rows and nested_all_rows are empty"
  ),
  code = {
    result <- entsoeapi:::extract_leaf_twig_branch(nodesets = list())
    testthat::expect_s3_class(object = result, class = "tbl_df")
    testthat::expect_equal(object = dim(result), expected = c(0L, 0L))
  }
)


testthat::test_that(
  desc = "extract_leaf_twig_branch() not works for data.frame",
  code = {
    testthat::expect_error(
      object = test_df_list[[6L]] |>
        extract_leaf_twig_branch(),
      regexp = paste(
        "no applicable method for 'nodeset_apply'",
        "applied to an object of class \\\"c\\('double', 'numeric'\\)\\\""
      )
    )
  }
)


testthat::test_that(
  desc = "read_zipped_xml() works as expected",
  code = {
    gzipped_sample_fixture <- testthat::test_path("fixtures", "mtcars.gzip")
    zipped_sample_fixture <- testthat::test_path("fixtures", "mtcars.zip")
    zipped_xml_sample_fixture <- testthat::test_path(
      "fixtures", "cd_catalog_xml.zip"
    )
    testthat::expect_no_error(
      object = read_zipped_xml(temp_file_path = zipped_xml_sample_fixture)
    )
    testthat::expect_warning(
      object = read_zipped_xml(temp_file_path = tempfile()),
      regexp = "error 1 in extracting from zip file"
    )
    testthat::expect_warning(
      object = read_zipped_xml(temp_file_path = gzipped_sample_fixture),
      regexp = "error 1 in extracting from zip file"
    )
    testthat::expect_error(
      object = read_zipped_xml(temp_file_path = zipped_sample_fixture),
      regexp = "Start tag expected, '<' not found \\[4\\]"
    )
  }
)


testthat::test_that(
  desc = "read_zipped_xml() aborts when unzip throws an error",
  code = {
    testthat::local_mocked_bindings(
      unzip = \(...) stop("cannot open the connection"),
      .package = "entsoeapi"
    )
    testthat::expect_error(
      object = read_zipped_xml(temp_file_path = tempfile()),
      regexp = "cannot open the connection"
    )
  }
)


testthat::test_that(
  desc = "calc_offset_urls() works as expected",
  code = {
    url_sample <- paste(
      "documentType=A65",
      "processType=A16",
      "outBiddingZone_Domain=10YCZ-CEPS-----N",
      "periodStart=202412312300",
      "periodEnd=202501312300",
      sep = "&"
    )
    reason_1 <- "allowed: 200; requested: 1111"
    reason_2 <- "allowed: -200; requested: -1111"
    reason_3 <- "foo; bar"
    testthat::expect_length(
      object = calc_offset_urls(reason = reason_1, query_string = url_sample),
      n = 6L
    )
    testthat::expect_error(
      object = calc_offset_urls(reason = reason_2, query_string = url_sample),
      regexp = "'from' must be a finite number"
    )
    testthat::expect_error(
      object = calc_offset_urls(reason = reason_3, query_string = url_sample),
      regexp = "'from' must be a finite number"
    )
  }
)


testthat::test_that(
  desc = "api_req() works",
  code = {
    m$reset()
    url_sample <- paste(
      "documentType=A73",
      "processType=A16",
      "periodStart=202001302300",
      "periodEnd=202001312300",
      "in_Domain=10YDE-VE-------2",
      sep = "&"
    )
    # validation errors (offline)
    testthat::expect_error(
      object = api_req(query_string = NULL, security_token = .test_token),
      regexp = paste(
        "Assertion on 'query_string' failed:",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = api_req(),
      regexp = paste(
        "Assertion on 'query_string' failed:",
        "Must be of type 'string', not 'NULL'"
      )
    )
    testthat::expect_error(
      object = api_req(query_string = NA, security_token = .test_token),
      regexp = "Assertion on 'query_string' failed: May not be NA."
    )
    testthat::expect_error(
      object = api_req(query_string = url_sample),
      regexp = paste(
        "Assertion on 'security_token' failed:",
        "Must be of type 'string', not 'NULL'."
      )
    )
    # mocked XML 200 response
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = entsoe_gen_min_raw
        )
      }
    )
    testthat::expect_s3_class(
      object = api_req(query_string = url_sample, security_token = .test_token),
      class = "xml_document"
    )
  }
)


testthat::test_that(
  desc = "api_req_safe() works",
  code = {
    m$reset()
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = entsoe_gen_min_raw
        )
      }
    )
    testthat::expect_no_error(
      object = content_1 <- api_req_safe(
        query_string = paste(
          "documentType=A73",
          "processType=A16",
          "periodStart=202001302300",
          "periodEnd=202001312300",
          "in_Domain=10YDE-VE-------2",
          sep = "&"
        ),
        security_token = .test_token
      )
    )
    testthat::expect_s3_class(object = content_1$result, class = "xml_document")
    testthat::expect_null(object = content_1$error)
    testthat::expect_true(
      object = is.list(content_1) &&
        length(content_1) == 2L &&
        all(names(content_1) == c("result", "error"))
    )
    # error path: NULL query_string triggers
    # assertion error captured by safely()
    testthat::expect_no_error(
      object = content_2 <- api_req_safe(
        query_string = NULL,
        security_token = .test_token
      )
    )
    testthat::expect_s3_class(object = content_2$error, class = "error")
    testthat::expect_null(object = content_2$result)
  }
)


testthat::test_that(
  desc = "url_posixct_format() works as expected",
  code = {
    testthat::expect_null(
      object = url_posixct_format(x = NULL),
      info = "The result of this functions should be NULL!"
    )
    testthat::expect_error(
      object = url_posixct_format(),
      regexp = 'argument "x" is missing'
    )
    testthat::expect_error(
      object = url_posixct_format(x = NA),
      regexp = "The argument is not in an acceptable timestamp format"
    )
    testthat::expect_error(
      object = url_posixct_format(x = 101L:110L),
      regexp = "The argument is not in an acceptable timestamp format"
    )
    testthat::expect_error(
      object = url_posixct_format(x = 101L),
      regexp = "The argument is not in an acceptable timestamp format"
    )
    testthat::expect_error(
      object = url_posixct_format(x = 101.5),
      regexp = "The argument is not in an acceptable timestamp format"
    )
    testthat::expect_error(
      object = url_posixct_format(x = "ABC"),
      regexp = paste(
        "Only the class POSIXct or '%Y-%m-%d %H:%M:%S'",
        "formatted text are supported by the converter"
      )
    )
    testthat::expect_match(
      object = as.POSIXct("2026-03-25 12:00:00", tz = "CET") |>
        url_posixct_format(),
      regexp = "[0-9]{12}",
      info = "The result of this functions should be 12 digit length string!"
    )
    testthat::expect_message(
      object = url_posixct_format(x = "20240722210000"),
      regexp = "The .+ value has been interpreted as UTC!"
    )
  }
)


testthat::test_that(
  desc = "get_eiccodes() works",
  code = {
    result <- get_eiccodes(
      pd_scheme = "",
      pd_domain = "",
      pd_csv_eic = "",
      f = testthat::test_path("fixtures", "eiccodes.csv")
    )
    testthat::expect_setequal(
      object = names(result),
      expected = c(
        "eic_code", "eic_display_name", "eic_long_name", "eic_parent",
        "eic_responsible_party", "eic_status", "market_participant_postal_code",
        "market_participant_iso_country_code", "market_participant_vat_code",
        "eic_type_function_list", "type"
      )
    )
    testthat::expect_equal(object = dim(result), expected = c(2L, 11L))
    # error path: non-existent file returns NULL
    testthat::expect_null(
      object = get_eiccodes(
        pd_scheme = "",
        pd_domain = "",
        pd_csv_eic = "",
        f = "/nonexistent/path.csv"
      )
    ) |>
      testthat::expect_warning(regexp = "No such file or directory")
  }
)


testthat::test_that(
  desc = "get_eiccodes() suppresses incomplete final line warning",
  code = {
    # Write a valid CSV file that intentionally lacks a trailing newline so
    # that readLines() would normally issue an "incomplete final line" warning.
    # The withCallingHandlers() block in get_eiccodes() must muffle it.
    csv_no_newline <- paste0(
      '"EicCode";"EicDisplayName";"EicLongName";"EicParent";',
      '"EicResponsibleParty";"EicStatus";"MarketParticipantPostalCode";',
      '"MarketParticipantIsoCountryCode";"MarketParticipantVatCode";',
      '"EicTypeFunctionList"\n',
      '"10Y000-0000-0001";"AreaX";"Area X Long";"";',
      '"";"";"";"";"";""'  # <- no \n here, so last line is incomplete
    )
    tmp <- tempfile(fileext = ".csv")
    con <- file(tmp, open = "wb")
    writeBin(charToRaw(csv_no_newline), con)
    close(con)
    on.exit(unlink(tmp), add = TRUE)
    testthat::expect_no_warning(
      object = get_eiccodes(
        pd_scheme = "",
        pd_domain = "",
        pd_csv_eic = "",
        f = tmp
      )
    )
  }
)


testthat::test_that(
  desc = "tidy_or_not() works as expected",
  code = {
    testthat::expect_equal(
      object = tidy_or_not(tbl = test_df_list[[1L]], tidy_output = TRUE) |>
        dim(),
      expected = c(1L, 4L)
    )
    testthat::expect_equal(
      object = tidy_or_not(tbl = test_df_list[[1L]], tidy_output = FALSE) |>
        dim(),
      expected = c(1L, 4L)
    )
    testthat::expect_setequal(
      object = tidy_or_not(tbl = test_df_list[[2L]], tidy_output = TRUE) |>
        names(),
      expected = c(
        "ts_resolution", "ts_reason_code", "ts_time_interval_start",
        "ts_point_price", "ts_point_dt_start"
      )
    )
    testthat::expect_setequal(
      object = tidy_or_not(tbl = test_df_list[[2L]], tidy_output = FALSE) |>
        names(),
      expected = c(
        "ts_resolution", "ts_reason_code", "ts_time_interval_start",
        "ts_point"
      )
    )
    testthat::expect_setequal(
      object = tidy_or_not(tbl = test_df_list[[3L]], tidy_output = TRUE) |>
        names(),
      expected = c(
        "bid_ts_mrid", "bid_ts_resolution", "bid_ts_time_interval_start",
        "bid_ts_point_price", "bid_ts_point_dt_start"
      )
    )
    testthat::expect_setequal(
      object = tidy_or_not(tbl = test_df_list[[4L]], tidy_output = FALSE) |>
        names(),
      expected = c("bid_ts_resolution", "bid_ts_mrid")
    )
    testthat::expect_setequal(
      object = tidy_or_not(tbl = test_df_list[[5L]], tidy_output = TRUE) |>
        names(),
      expected = c(
        "ts_mrid", "ts_curve_type", "ts_resolution", "ts_time_interval_start",
        "ts_time_interval_end", "ts_point_quantity", "ts_point_dt_start"
      )
    )
    testthat::expect_equal(
      object = tidy_or_not(tbl = test_df_list[[5L]], tidy_output = TRUE) |>
        dim(),
      expected = c(6L, 7L)
    )
  }
)


testthat::test_that(
  desc = "tidy_or_not() stops on unknown curve_type",
  code = {
    testthat::expect_error(
      object = tidy_or_not(tbl = test_df_list[[13L]], tidy_output = TRUE),
      regexp = "The curve type is not defined, but A99!"
    )
  }
)


testthat::test_that(
  desc = "my_snakecase() works as expected",
  code = {
    testthat::expect_setequal(
      object = my_snakecase(tbl = test_df_list[[6L]]),
      expected = c(
        "sepal_length", "sepal_width", "petal_length", "petal_width",
        "species"
      )
    )
    testthat::expect_setequal(
      object = my_snakecase(tbl = transmission_pair_eic_dict),
      expected = c(
        "out_area_code", "out_area_type_code", "out_area_name", "out_map_code",
        "in_area_code", "in_area_type_code", "in_area_name", "in_map_code"
      )
    )
    testthat::expect_setequal(
      object = my_snakecase(tbl = test_df_list[[7L]]),
      expected = c(
        "mrid", "ts_type", "unavailability", "a_ts_production",
        "b_ts_production", "c_ts_asset", "d_ts_asset", "e_psr_type",
        "f_psr_type", "g_psr_type", "receiver_market_participant_market_role"
      )
    )
    testthat::expect_error(
      object = my_snakecase(tbl = NULL),
      regexp = paste(
        "Assertion on 'tbl' failed:",
        "Must be of type 'data.frame', not 'NULL'."
      )
    )
  }
)


testthat::test_that(
  desc = "add_type_names() works as expected",
  code = {
    testthat::expect_setequal(
      object = add_type_names(tbl = test_df_list[[8L]]) |>
        names(),
      expected = c(
        "process_type", "process_type_def", "ts_auction_type",
        "ts_auction_type_def", "ts_business_type", "ts_business_type_def",
        "type", "type_def", "market_agreement_type",
        "market_agreement_type_def", "ts_asset_psr_type",
        "ts_asset_psr_type_def", "ts_production_psr_type",
        "ts_production_psr_type_def", "ts_mkt_psr_type", "ts_mkt_psr_type_def"
      )
    )
    testthat::expect_no_warning(
      object = add_type_names(tbl = test_df_list[[6L]]),
      message = "No additional definitions added!"
    )
    testthat::expect_no_warning(
      object = add_type_names(tbl = NULL),
      message = "No additional definitions added!"
    )
    # ts_product branch
    testthat::expect_setequal(
      object = add_type_names(tbl = test_df_list[[12L]]) |> names(),
      expected = c("ts_product", "ts_product_def")
    )
    testthat::expect_setequal(
      object = add_type_names(tbl = test_df_list[[9L]]) |>
        names(),
      expected = c(
        "subject_market_participant_market_role_type",
        "bid_ts_flow_direction",
        "subject_market_participant_market_role_type_def",
        "bid_ts_flow_direction_def"
      )
    )
  }
)


testthat::test_that(
  desc = "add_eic_names() works as expected",
  code = {
    m$reset()
    testthat::local_mocked_bindings(
      get_resource_object_eic = \(...) test_df_list[[16L]],
      fetch_eic_csv = \(...) test_df_list[[17L]],
      .package = "entsoeapi"
    )
    testthat::expect_setequal(
      object = add_eic_names(tbl = test_df_list[[10L]]) |>
        names(),
      expected = c(
        "control_area_domain_mrid", "ts_out_domain_mrid", "ts_in_domain_mrid",
        "ts_out_bidding_zone_domain_mrid", "ts_in_bidding_zone_domain_mrid",
        "ts_bidding_zone_domain_mrid", "ts_registered_resource_mrid",
        "ts_registered_resource_name", "ts_bidding_zone_domain_name",
        "ts_in_bidding_zone_domain_name", "ts_out_bidding_zone_domain_name",
        "ts_in_domain_name", "ts_out_domain_name", "control_area_domain_name"
      )
    )
    testthat::expect_s3_class(
      object = add_eic_names(tbl = test_df_list[[6L]]),
      class = "data.frame"
    )
    testthat::expect_s3_class(
      object = add_eic_names(tbl = NULL),
      class = "data.frame"
    )
  }
)


testthat::test_that(
  desc = "add_eic_names() adds names for additional domain mrid columns",
  code = {
    m$reset()
    testthat::local_mocked_bindings(
      fetch_eic_csv = \(...) test_df_list[[18L]],
      .package = "entsoeapi"
    )
    testthat::expect_setequal(
      object = add_eic_names(tbl = test_df_list[[14L]]) |>
        names(),
      expected = c(
        "area_domain_mrid", "area_domain_name", "ts_acquiring_domain_mrid",
        "ts_acquiring_domain_name", "ts_connecting_domain_mrid",
        "ts_connecting_domain_name", "bid_ts_acquiring_domain_mrid",
        "bid_ts_acquiring_domain_name", "bid_ts_connecting_domain_mrid",
        "bid_ts_connecting_domain_name", "domain_mrid", "domain_name",
        "constraint_ts_monitored_ptdf_domain_mrid",
        "constraint_ts_monitored_ptdf_domain_name"
      )
    )
  }
)


testthat::test_that(
  desc = "add_definitions() worksas expected",
  code = {
    testthat::expect_setequal(
      object = add_definitions(tbl = test_df_list[[15L]]) |>
        names(),
      expected = c("ts_reason_code", "ts_reason_text")
    )
    testthat::expect_true(
      object = add_definitions(tbl = test_df_list[[15L]]) |>
        pluck("ts_reason_text") |>
        str_detect(pattern = " - ") |>
        all()
    )
    testthat::expect_setequal(
      object = add_definitions(tbl = test_df_list[[11L]]) |>
        names(),
      expected = c(
        "doc_status_value", "ts_auction_category", "ts_flow_direction",
        "reason_code", "ts_reason_code", "ts_object_aggregation",
        "doc_status", "ts_auction_category_def", "ts_flow_direction_def",
        "reason_text", "ts_reason_text", "ts_object_aggregation_def"
      )
    )
    testthat::expect_s3_class(
      object = add_definitions(tbl = test_df_list[[6L]]),
      class = "data.frame"
    )
    testthat::expect_equal(
      object = add_definitions(tbl = NULL),
      expected = data.frame()
    )
  }
)


testthat::test_that(
  desc = "xml_to_table() works as expected 1",
  code = {
    testthat::expect_error(
      object = xml_to_table(
        xml_content = order_schema_xml,
        tidy_output = FALSE
      ),
      regexp = "There is no interesting column in the result table!"
    )
    testthat::expect_error(
      object = xml_to_table(xml_content = test_df_list[[6L]]),
      regexp = "The `xml_content` should be an xml document"
    )
    testthat::expect_error(
      object = xml_to_table(xml_content = NULL),
      regexp = "The `xml_content` should be an xml document"
    )
    testthat::expect_error(
      object = xml_to_table(xml_content = NA),
      regexp = "The `xml_content` should be an xml document"
    )
    testthat::expect_error(
      object = xml_to_table(xml_content = "not an xml"),
      regexp = "The `xml_content` should be an xml document"
    )
  }
)


testthat::test_that(
  desc = "xml_to_table() works as expected 2",
  code = {
    m$reset()
    testthat::local_mocked_bindings(
      fetch_eic_csv = \(...) test_df_list[[17L]],
      .package = "entsoeapi"
    )
    testthat::expect_equal(
      object = xml_to_table(
        xml_content = entsoe_gen_min_xml,
        tidy_output = TRUE
      ) |>
        dim(),
      expected = c(2L, 21L)
    )
    testthat::expect_equal(
      object = xml_to_table(
        xml_content = entsoe_gen_min_xml,
        tidy_output = FALSE
      ) |>
        dim(),
      expected = c(1L, 20L)
    )
  }
)


testthat::test_that(
  desc = "xml_to_table() aborts on unexpected XML tree structure",
  code = {
    m$reset()
    # Mock extract_leaf_twig_branch to throw an error
    testthat::local_mocked_bindings(
      extract_leaf_twig_branch = \(...) stop("simulated extraction failure"),
      .package = "entsoeapi"
    )
    testthat::expect_error(
      object = testthat::test_path("fixtures", "unexpected_structure.xml") |>
        xml2::read_xml() |>
        entsoeapi:::xml_to_table(),
      regexp = "unexpected tree structure.+simulated extraction failure"
    )
  }
)


testthat::test_that(
  desc = "extract_response() works as expected 1",
  code = {
    testthat::expect_error(
      object = extract_response(
        content = list(
          result = testthat::test_path("fixtures", "cd_catalog.xml") |>
            xml2::read_xml(),
          error = NULL
        )
      ),
      regexp = "There is no interesting column in the result table"
    )
    testthat::expect_error(
      object = extract_response(content = list(result = "A", error = "B")),
      regexp = "\\$ operator is invalid for atomic vectors"
    )
    testthat::expect_error(
      object = extract_response(content = test_df_list[[6L]]),
      regexp = "The content is not in the required list format"
    )
    testthat::expect_error(
      object = extract_response(content = NULL),
      regexp = "The content is not in the required list format"
    )
  }
)


testthat::test_that(
  desc = "extract_response() works as expected 2",
  code = {
    m$reset()
    testthat::local_mocked_bindings(
      fetch_eic_csv = \(...) test_df_list[[17L]],
      .package = "entsoeapi"
    )
    content_ok <- list(
      result = entsoe_gen_min_xml,
      error = NULL
    )
    testthat::expect_equal(
      object = extract_response(content = content_ok, tidy_output = TRUE) |>
        dim(),
      expected = c(2L, 21L)
    )
    testthat::expect_equal(
      object = extract_response(content = content_ok, tidy_output = FALSE) |>
        dim(),
      expected = c(1L, 20L)
    )
  }
)


testthat::test_that(
  desc = "extract_response() returns empty tibble for NULL result element",
  code = {
    content_null <- list(result = list(NULL), error = NULL)
    result <- extract_response(content = content_null)
    testthat::expect_s3_class(object = result, class = "tbl_df")
    testthat::expect_equal(object = dim(result), expected = c(0L, 0L))
  }
)


testthat::test_that(
  desc = "extract_response() processes a list-of-xml_documents element",
  code = {
    content_alldoc <- list(result = list(list(order_schema_xml)), error = NULL)
    # xml_to_table on order-schema.xml fails because it has no ENTSO-E columns;
    # the important thing is the all_doc branch is entered
    testthat::expect_error(
      object = extract_response(content = content_alldoc),
      regexp = "There is no interesting column in the result table"
    )
  }
)


testthat::test_that(
  desc = "extract_response() returns empty tibble for list of non-documents",
  code = {
    content_nondoc <- list(result = list(list(1L, 2L, 3L)), error = NULL)
    result <- extract_response(content = content_nondoc)
    testthat::expect_s3_class(object = result, class = "tbl_df")
    testthat::expect_equal(object = dim(result), expected = c(0L, 0L))
  }
)


testthat::test_that(
  desc = paste(
    "get_all_allocated_eic() returns a tibble",
    "with expected columns on valid XML"
  ),
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = get_allocated_eic_min_raw
        )
      }
    )
    testthat::expect_s3_class(
      object = tbl <- get_all_allocated_eic(),
      class = "tbl_df"
    )
    testthat::expect_equal(
      object = dim(tbl),
      expected = c(2L, 12L)
    )
    testthat::expect_setequal(
      object = names(tbl),
      expected = c(
        "revision_number", "created_date_time", "eic_code", "doc_status_value",
        "doc_status", "instance_component_attribute", "long_name",
        "display_name", "last_request_date",
        "responsible_market_participant_mrid", "function_names",
        "parent_market_document_mrid"
      )
    )
    testthat::expect_false(object = anyNA(tbl$created_date_time))
  }
)


testthat::test_that(
  desc = paste(
    "get_all_allocated_eic() stops with HTTP error",
    "message and request URL"
  ),
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 503L,
          headers = list("content-type" = "application/xml"),
          body = charToRaw(
            paste0(
              '<?xml version="1.0" encoding="utf-8"?>',
              "<root><Reason>Unavailable</Reason></root>"
            )
          )
        )
      }
    )
    testthat::expect_error(
      object = get_all_allocated_eic(),
      regexp = "HTTP 503"
    )
    testthat::expect_error(
      object = get_all_allocated_eic(),
      regexp = "eepublicdownloads\\.blob\\.core\\.windows\\.net"
    )
  }
)


testthat::test_that(
  desc = "get_all_allocated_eic() stops on empty response body",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = raw(0L)
        )
      }
    )
    testthat::expect_error(
      object = get_all_allocated_eic(),
      regexp = "Can't retrieve empty body"
    )
  }
)


testthat::test_that(
  desc = "get_all_allocated_eic() stops on XML with unexpected tree structure",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = testthat::test_path("fixtures", "minimal.xml") |>
            readLines(encoding = "UTF-8") |>
            paste(collapse = "\n") |>
            charToRaw()
        )
      }
    )
    testthat::expect_error(
      object = get_all_allocated_eic(),
      regexp = "unexpected tree structure"
    )
  }
)


testthat::test_that(
  desc =
    "get_all_allocated_eic() collapses duplicate Function_Names with ' - '",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = get_allocated_eic_min_raw
        )
      }
    )
    tbl <- get_all_allocated_eic()
    testthat::expect_equal(object = dim(tbl), expected = c(2L, 12L))
    testthat::expect_true(
      object = tbl |>
        pluck("function_names") |>
        str_detect(pattern = " - ") |>
        all()
    )
  }
)


testthat::test_that(
  desc = paste(
    "get_all_allocated_eic() collapses actual duplicate",
    "Function_Names elements with ' - '"
  ),
  code = {
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "get_allocated_eic_dupl.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = xml_fixture
        )
      }
    )
    tbl <- get_all_allocated_eic()
    testthat::expect_equal(object = nrow(tbl), expected = 1L)
    testthat::expect_true(
      object = tbl$function_names[[1L]] |>
        str_detect(pattern = "GENERATION - LOAD")
    )
  }
)


testthat::test_that(
  desc = paste(
    "get_all_allocated_eic() stops with 'unexpected tree structure'",
    "when bind_cols raises an error"
  ),
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = get_allocated_eic_min_raw
        )
      }
    )
    testthat::local_mocked_bindings(
      bind_cols = \(...) stop("mocked bind_cols error"),
      .package = "entsoeapi"
    )
    testthat::expect_error(
      object = get_all_allocated_eic(),
      regexp = "unexpected tree structure"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops on unknown 200 response content-type",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          url = req$url,
          headers = list("content-type" = "text/plain"),
          body = charToRaw("some plain text")
        )
      }
    )
    testthat::expect_error(
      object = api_req(
        query_string = "documentType=A73",
        security_token = .test_token
      ),
      regexp = "Not known response content-type: text/plain"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops on HTML error response",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 403L,
          url = req$url,
          headers = list("content-type" = "text/html"),
          body = charToRaw(
            "<!DOCTYPE html><html><body>Access Denied</body></html>"
          )
        )
      }
    )
    # The stop() message may be empty when xmlconvert::xml_to_list() |> pluck()
    # returns NULL for the HTML body; what matters is that the HTML error branch
    # is reached and an error is thrown.
    testthat::expect_error(
      object = api_req(
        query_string = "documentType=A73",
        security_token = .test_token
      ),
      regexp = "subscript out of bounds"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops on XML error with unexpected Reason structure",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 500L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = readLines(
            con = testthat::test_path("fixtures", "something_went_wrong.xml"),
            encoding = "UTF-8"
          ) |>
            paste(collapse = "\n") |>
            charToRaw()
        )
      }
    )
    testthat::expect_error(
      object = api_req(
        query_string = "documentType=A73",
        security_token = .test_token
      ),
      regexp = "^500"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops with code:text message on non-999 XML error code",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 400L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = readLines(
            con = testthat::test_path("fixtures", "not_available.xml"),
            encoding = "UTF-8"
          ) |>
            paste(collapse = "\n") |>
            charToRaw()
        )
      }
    )
    testthat::expect_error(
      object = api_req(
        query_string = "documentType=A73",
        security_token = .test_token
      ),
      regexp = "B11"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops with curl error message on no internet connection",
  code = {
    httr2_err <- structure(
      class = c("httr2_failure", "httr2_error", "error", "condition"),
      list(
        message = "Failed to perform HTTP request.",
        resp = NULL,
        parent = structure(
          class = c("curl_error", "error", "condition"),
          list(message = "Could not resolve host: web-api.tp.entsoe.eu")
        )
      )
    )
    httr2::local_mocked_responses(
      mock = \(req) stop(httr2_err)
    )
    testthat::expect_error(
      object = api_req(
        query_string = paste0(
          "documentType=A73",
          "&processType=A16",
          "&periodStart=202001302300",
          "&periodEnd=202001312300",
          "&in_Domain=10YDE-VE-------2"
        ),
        security_token = .test_token
      ),
      regexp = "Could not resolve host: web-api\\.tp\\.entsoe\\.eu"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops with HTTP 503 message on service unavailable",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 503L,
          url = req$url,
          headers = list("content-type" = "text/html"),
          body = charToRaw("Service Unavailable")
        )
      }
    )
    testthat::expect_error(
      object = api_req(
        query_string = "documentType=A73",
        security_token = .test_token
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "api_req() extracts body text from HTML error response",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 400L,
          url = req$url,
          headers = list("content-type" = "text/html"),
          body = readLines(
            con = testthat::test_path("fixtures", "bad_request.xml"),
            encoding = "UTF-8"
          ) |>
            paste(collapse = "\n") |>
            charToRaw()
        )
      }
    )
    testthat::expect_error(
      object = entsoeapi:::api_req(
        query_string = "documentType=A73",
        security_token = .test_token
      ),
      regexp = "400.*Bad Request"
    )
  }
)


testthat::test_that(
  desc = "api_req() aborts on unexpected non-200 HTTP success status",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 201L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = charToRaw("")
        )
      }
    )
    testthat::expect_error(
      object = entsoeapi:::api_req(
        query_string = "documentType=A73",
        security_token = .test_token
      ),
      regexp = "Unexpected HTTP 201"
    )
  }
)


testthat::test_that(
  desc = "api_req() aborts and extracts error from JSON response",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 400L,
          url = req$url,
          headers = list("content-type" = "application/json"),
          body = charToRaw(
            '{"uuAppErrorMap": {"URI_FORMAT_ERROR": {"message": "foo"}}}'
          )
        )
      }
    )
    testthat::expect_error(
      object = entsoeapi:::api_req(
        query_string = "documentType=A73",
        security_token = .test_token
      ),
      regexp = "foo"
    )
  }
)


testthat::test_that(
  desc = "there_is_provider() returns TRUE on 401 Unauthorized",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 401L,
          headers = list("content-type" = "text/html"),
          body = charToRaw("Unauthorized 401")
        )
      }
    )
    testthat::expect_true(
      object = there_is_provider()
    )
  }
)


testthat::test_that(
  desc = "there_is_provider() returns FALSE on 503 Service Unavailable",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 503L,
          headers = list("content-type" = "text/html"),
          body = charToRaw("Service Unavailable")
        )
      }
    )
    testthat::expect_false(
      object = there_is_provider()
    )
  }
)


testthat::test_that(
  desc = "there_is_provider() returns FALSE on network/curl error",
  code = {
    curl_err <- structure(
      class = c("curl_error", "error", "condition"),
      list(message = "Could not resolve host: web-api.tp.entsoe.eu")
    )
    httr2_err <- structure(
      class = c("httr2_failure", "httr2_error", "error", "condition"),
      list(
        message = "Failed to perform HTTP request.",
        resp = NULL,
        parent = curl_err
      )
    )
    httr2::local_mocked_responses(mock = \(req) stop(httr2_err))
    testthat::expect_false(object = there_is_provider())
  }
)


testthat::test_that(
  desc = "there_is_provider() uses custom api_scheme, api_domain, and api_name",
  code = {
    captured_url <- NULL
    httr2::local_mocked_responses(
      mock = \(req) {
        captured_url <<- req$url
        httr2::response(
          status_code = 401L,
          headers = list("content-type" = "text/html"),
          body = charToRaw(x = "Unauthorized")
        )
      }
    )
    testthat::expect_true(
      object = there_is_provider(
        api_scheme = .api_scheme,
        api_domain = "test.example.com/",
        api_name = .api_name
      )
    )
    testthat::expect_match(
      object = captured_url,
      regexp = "test\\.example\\.com"
    )
  }
)


testthat::test_that(
  desc = paste(
    "there_is_provider() returns FALSE when",
    "there is no internet connection"
  ),
  code = {
    connection_error <- structure(
      class = c("httr2_failure", "httr2_error", "error", "condition"),
      list(
        message = "Failed to perform HTTP request.",
        resp = NULL,
        parent = structure(
          class = c("curl_error", "error", "condition"),
          list(message = "Could not connect to web-api.tp.entsoe.eu")
        )
      )
    )
    httr2::local_mocked_responses(mock = \(req) stop(connection_error))
    testthat::expect_false(object = there_is_provider())
  }
)


testthat::test_that(
  desc = "assert_eic() returns invisibly for valid EIC codes",
  code = {
    testthat::expect_invisible(
      call = result <- assert_eic(eic = "10YCZ-CEPS-----N")
    )
    testthat::expect_identical(object = result, expected = "10YCZ-CEPS-----N")
    testthat::expect_invisible(
      call = result <- assert_eic(eic = "10YHU-MAVIR----U")
    )
    testthat::expect_identical(object = result, expected = "10YHU-MAVIR----U")
    testthat::expect_invisible(
      call = result <- assert_eic(eic = "10YDE-VE-------2")
    )
    testthat::expect_identical(object = result, expected = "10YDE-VE-------2")
    testthat::expect_invisible(
      call = result <- assert_eic(eic = "10YFI-1--------U")
    )
    testthat::expect_identical(object = result, expected = "10YFI-1--------U")
    testthat::expect_invisible(
      call = result <- assert_eic(eic = "10YNO-0--------C")
    )
    testthat::expect_identical(object = result, expected = "10YNO-0--------C")
  }
)


testthat::test_that(
  desc = "assert_eic() throws for wrong check character",
  code = {
    # "10YCZ-CEPS-----N" is valid; replacing N with X gives wrong check char
    testthat::expect_error(
      object = assert_eic(eic = "10YCZ-CEPS-----X"),
      regexp = "Expected.*N.*got.*X"
    )
    # "10YHU-MAVIR----U" is valid; replacing U with Z gives wrong check char
    testthat::expect_error(
      object = assert_eic(eic = "10YHU-MAVIR----Z"),
      regexp = "Expected.*U.*got.*Z"
    )
    # "10YDE-VE-------2" is valid; replacing 2 with 3 gives wrong check char
    testthat::expect_error(
      object = assert_eic(eic = "10YDE-VE-------3"),
      regexp = "Expected.*2.*got.*3"
    )
  }
)


testthat::test_that(
  desc = "assert_eic() respects null_ok argument",
  code = {
    testthat::expect_invisible(
      call = result <- assert_eic(eic = NULL, null_ok = TRUE)
    )
    testthat::expect_null(object = result)
    testthat::expect_error(
      object = assert_eic(eic = NULL, null_ok = FALSE),
      regexp = "Must be of type 'string', not 'NULL'"
    )
  }
)


testthat::test_that(
  desc = "check_sec_token() returns invisibly for a valid token",
  code = {
    testthat::expect_invisible(
      call = result <- entsoeapi:::check_sec_token(
        security_token = .test_token
      )
    )
    testthat::expect_equal(object = result,  expected = .test_token)
  }
)


testthat::test_that(
  desc = "check_sec_token() rejects tokens containing not hexadecimals",
  code = {
    testthat::expect_error(
      object = entsoeapi:::check_sec_token(
        security_token = "token?value12345678901234567890qwert"
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = entsoeapi:::check_sec_token(
        security_token = ""
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = entsoeapi:::check_sec_token(
        security_token = NA_character_
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = entsoeapi:::check_sec_token(
        security_token = 12345
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = entsoeapi:::check_sec_token(
        security_token = NULL
      ),
      regexp = "should comply with the UUID v4 format"
    )
    testthat::expect_error(
      object = entsoeapi:::check_sec_token(
        security_token = FALSE
      ),
      regexp = "should comply with the UUID v4 format"
    )
  }
)


testthat::test_that(
  desc = "pluck() extracts a nested element by name",
  code = {
    testthat::expect_equal(
      object = list(a = list(b = list(c = 42L))) |> pluck("a", "b", "c"),
      expected = 42L
    )
  }
)


testthat::test_that(
  desc = "pluck() extracts by integer index",
  code = {
    x <- testthat::expect_equal(
      object = list(list(10L, 20L), list(30L, 40L)) |> pluck(2L, 1L),
      expected = 30L
    )
  }
)


testthat::test_that(
  desc = "pluck() returns NULL when path traversal hits NULL",
  code = {
    testthat::expect_null(object = list(a = NULL) |> pluck("a", "b"))
  }
)


testthat::test_that(
  desc = "extract_nodesets() converts nodeset with no text content to NA",
  code = {
    nodesets <- xml2::read_xml(x = "<root><empty/></root>") |>
      xml2::xml_find_first(xpath = "//empty") |>
      list()
    result <- entsoeapi:::extract_nodesets(nodesets = nodesets)
    testthat::expect_length(object = result, n = 1L)
    tbl <- result[[1L]]
    testthat::expect_s3_class(object = tbl, class = "tbl_df")
    testthat::expect_true(object = is.na(tbl[[1L]]) |> all())
  }
)


testthat::test_that(
  desc = "extract_nodesets() warns on non-conformable column lengths",
  code = {
    # Build XML where unlist() produces columns of length 3 and length 2,
    # which are non-conformable (3 %% 2 != 0).
    nodesets <- readLines(
      con = testthat::test_path("fixtures", "semi_structured.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw() |>
      xml2::read_xml() |>
      xml2::xml_find_first(xpath = "//wrapper") |>
      list()
    testthat::expect_warning(
      object = entsoeapi:::extract_nodesets(nodesets = nodesets),
      regexp = "recycling with truncation"
    )
  }
)


testthat::test_that(
  desc = "node_to_rows() handles terminal node with prefix = NULL",
  code = {
    result <- xml2::read_xml(x = "<root><leaf>hello</leaf></root>") |>
      xml2::xml_find_first(xpath = "//leaf") |>
      entsoeapi:::node_to_rows(prefix = NULL)
    testthat::expect_length(object = result, n = 1L)
    testthat::expect_equal(object = result[[1L]], expected = c(leaf = "hello"))
  }
)


testthat::test_that(
  desc = "node_to_rows() returns empty string for empty terminal node",
  code = {
    result <- xml2::read_xml(x = "<root><leaf/></root>") |>
      xml2::xml_find_first(xpath = "//leaf") |>
      entsoeapi:::node_to_rows(prefix = NULL)
    testthat::expect_length(object = result, n = 1L)
    testthat::expect_equal(object = result[[1L]], expected = c(leaf = ""))
    testthat::expect_equal(object = names(result[[1L]]), expected = "leaf")
  }
)


testthat::test_that(
  desc = "node_to_rows() converts NA text to NA_character_ on terminal node",
  code = {
    # Mock xml_text in the entsoeapi namespace to return NA,
    # triggering the L354 defensive branch.
    testthat::local_mocked_bindings(
      xml_text = \(x, ...) NA,
      .package = "entsoeapi"
    )
    result <- xml2::read_xml(x = "<root><leaf>value</leaf></root>") |>
      xml2::xml_find_first(xpath = "//leaf") |>
      entsoeapi:::node_to_rows(prefix = NULL)
    testthat::expect_length(object = result, n = 1L)
    testthat::expect_true(object = is.na(result[[1L]]))
    testthat::expect_equal(object = names(result[[1L]]), expected = "leaf")
  }
)


testthat::test_that(
  desc = "node_to_rows() handles leaf with sub-children text nodes",
  code = {
    # The L380-383 / L396-400 branches fire when a child is classified as a
    # leaf (xml_length == 0) yet xml_children() on that child returns
    # sub-elements. In standard XML this cannot happen, so we mock
    # xml_children in the entsoeapi namespace to inject sub-elements for
    # the leaf child only.
    # Use a single sub-child to stay within vapply's FUN.VALUE=character(1L).
    compound_doc <- xml2::read_xml(x = "<compound><a>X</a></compound>")
    compound_children <- xml2::xml_children(x = compound_doc)
    real_xml_children <- xml2::xml_children
    testthat::local_mocked_bindings(
      xml_children = \(x, ...) {
        ch <- real_xml_children(x, ...)
        # When the real result is empty (leaf node named "compound"),
        # inject a fake sub-child to exercise the branch.
        if (length(ch) == 0L && xml2::xml_name(x = x) == "compound") {
          return(compound_children)
        }
        ch
      },
      .package = "entsoeapi"
    )
    result <- xml2::read_xml(
      x = "<parent><compound>text</compound></parent>"
    ) |>
      xml2::xml_find_first(xpath = "//parent") |>
      entsoeapi:::node_to_rows(prefix = NULL)
    testthat::expect_length(object = result, n = 1L)
    row <- result[[1L]]
    # The name should be "compound.a" and the value "X".
    testthat::expect_equal(object = names(row), expected = "compound.a")
    testthat::expect_equal(object = unname(row), expected = "X")
  }
)


testthat::test_that(
  desc = paste(
    "node_to_rows() uses stacked_rows[[1L]] as leaf_row",
    "when duplicate leaves and nested children exist"
  ),
  code = {
    # Duplicate leaf names + nested children
    result <- testthat::test_path("fixtures", "duplicate_leaf_names.xml") |>
      readLines(encoding = "UTF-8") |>
      paste(collapse = "\n") |>
      charToRaw() |>
      xml2::read_xml() |>
      xml2::xml_find_first(xpath = "//parent") |>
      entsoeapi:::node_to_rows(prefix = "p")
    testthat::expect_true(object = length(result) == 1L)
    # Each row should have both val and nested.inner columns
    row <- result[[1L]]
    testthat::expect_true(object = "p.val" %in% names(row))
    testthat::expect_true(object = "p.nested.inner" %in% names(row))
  }
)


testthat::test_that(
  desc = paste(
    "node_to_rows() returns list() when",
    "leaf_row and nested_rows are empty"
  ),
  code = {
    # It is a defensive branch: both leaf_row and nested_rows are empty.
    # This cannot happen with well-formed XML because any child is either a
    # leaf or nested. We trigger it by mocking the recursive call to return
    # list().
    real_node_to_rows <- entsoeapi:::node_to_rows
    first_call <- TRUE
    testthat::local_mocked_bindings(
      node_to_rows = \(node, prefix = NULL) {
        if (first_call) {
          first_call <<- FALSE
          real_node_to_rows(node = node, prefix = prefix)
        } else {
          # recursive calls return empty
          list()
        }
      },
      .package = "entsoeapi"
    )
    result <- testthat::test_path("fixtures", "empty_leaf_and_nested.xml") |>
      xml2::read_xml() |>
      xml2::xml_find_first(xpath = "//parent") |>
      entsoeapi:::node_to_rows(prefix = "p")
    testthat::expect_equal(object = result, expected = list())
  }
)

testthat::test_that(
  desc = paste(
    "node_to_rows() returns stacked_rows when duplicate leaf names exist",
    "but no nested children are present"
  ),
  code = {
    result <- testthat::test_path("fixtures", "dupl_leaf_names.xml") |>
      xml2::read_xml() |>
      xml2::xml_find_first(xpath = "//parent") |>
      entsoeapi:::node_to_rows(prefix = "p")
    testthat::expect_length(object = result, n = 3L)
    testthat::expect_equal(object = result[[1L]], expected = c(p.val = "1"))
    testthat::expect_equal(object = result[[2L]], expected = c(p.val = "2"))
    testthat::expect_equal(object = result[[3L]], expected = c(p.val = "3"))
  }
)


testthat::test_that(
  desc = "rows_to_tbl() returns empty tibble on empty input",
  code = {
    result <- entsoeapi:::rows_to_tbl(list())
    testthat::expect_s3_class(object = result, class = "tbl_df")
    testthat::expect_equal(object = dim(result), expected = c(0L, 0L))
  }
)


testthat::test_that(
  desc = "check_period() returns formatted start/end for a valid period",
  code = {
    result <- entsoeapi:::check_period(
      period_start = lubridate::ymd("2023-01-01", tz = "CET"),
      period_end = lubridate::ymd("2023-12-31", tz = "CET"),
      period_length = "1 year"
    )
    testthat::expect_type(object = result, type = "list")
    testthat::expect_named(object = result, expected = c("start", "end"))
    testthat::expect_type(object = result$start, type = "character")
    testthat::expect_type(object = result$end, type = "character")
  }
)


testthat::test_that(
  desc = "check_period() aborts when period exceeds one year",
  code = {
    testthat::expect_error(
      object = entsoeapi:::check_period(
        period_start = lubridate::ymd("2023-01-01", tz = "CET"),
        period_end = lubridate::ymd("2024-01-02", tz = "CET"),
        period_length = "1 year"
      ),
      regexp = "1 year range limit should be applied"
    )
  }
)


testthat::test_that(
  desc = "check_period() skips validation when period_length is NULL",
  code = {
    result <- entsoeapi:::check_period(
      period_start = lubridate::ymd("2020-01-01", tz = "CET"),
      period_end = lubridate::ymd("2025-01-01", tz = "CET"),
      period_length = NULL
    )
    testthat::expect_type(object = result, type = "list")
    testthat::expect_named(object = result, expected = c("start", "end"))
  }
)


testthat::test_that(
  desc = "check_period() passes when period is within day limit",
  code = {
    result <- entsoeapi:::check_period(
      period_start = lubridate::ymd("2023-06-01", tz = "CET"),
      period_end = lubridate::ymd("2023-06-02", tz = "CET"),
      period_length = "1 day"
    )
    testthat::expect_type(object = result, type = "list")
    testthat::expect_named(object = result, expected = c("start", "end"))
  }
)


testthat::test_that(
  desc = "check_period() aborts when period exceeds day limit",
  code = {
    testthat::expect_error(
      object = entsoeapi:::check_period(
        period_start = lubridate::ymd("2023-06-01", tz = "CET"),
        period_end = lubridate::ymd("2023-06-03", tz = "CET"),
        period_length = "1 day"
      ),
      regexp = "1 day range limit should be applied"
    )
  }
)


testthat::test_that(
  desc = "check_period() aborts on invalid period_length pattern",
  code = {
    testthat::expect_error(
      object = entsoeapi:::check_period(
        period_start = lubridate::ymd("2023-01-01", tz = "CET"),
        period_end = lubridate::ymd("2023-06-01", tz = "CET"),
        period_length = "foo"
      ),
      regexp = "Assertion on 'period_length' failed: Must comply to pattern"
    )
  }
)


testthat::test_that(
  desc = "check_period() aborts on missing period borders",
  code = {
    testthat::expect_error(
      object = entsoeapi:::check_period(
        period_start = NULL,
        period_end = lubridate::ymd("2023-06-01", tz = "CET")
      ),
      regexp = "the period should have both start and end"
    )
    testthat::expect_error(
      object = entsoeapi:::check_period(
        period_start = lubridate::ymd("2023-01-01", tz = "CET"),
        period_end = NULL
      ),
      regexp = "the period should have both start and end"
    )
    testthat::expect_error(
      object = entsoeapi:::check_period(
        period_start = NULL,
        period_end = NULL
      ),
      regexp = "the period should have both start and end"
    )
  }
)


testthat::test_that(
  desc = "run_api_query() returns a tibble from a mocked XML response",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = testthat::test_path("fixtures", "allocated_eic_min.xml") |>
            readLines(encoding = "UTF-8") |>
            paste(collapse = "\n") |>
            charToRaw()
        )
      }
    )
    testthat::expect_s3_class(
      object = result <- entsoeapi:::run_api_query(
        query_string   = "documentType=A73",
        security_token = .test_token,
        tidy_output    = FALSE
      ),
      class = "tbl_df"
    )
    testthat::expect_equal(object = dim(result), expected = c(1L, 4L))
  }
)


testthat::test_that(
  desc = "api_req() handles zip response content-type",
  code = {
    file_size <- testthat::test_path("fixtures", "cd_catalog_xml.zip") |>
      file.size()
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 200L,
          url = req$url,
          headers = list("content-type" = "application/zip"),
          body = testthat::test_path("fixtures", "cd_catalog_xml.zip") |>
            readBin(what = "raw", n = file_size)
        )
      }
    )
    result <- entsoeapi:::api_req(
      query_string = "documentType=A73",
      security_token = .test_token
    )
    testthat::expect_type(object = result, type = "list")
  }
)


testthat::test_that(
  desc = "api_req() aborts on code-999 XML error when offset is not needed",
  code = {
    httr2::local_mocked_responses(
      mock = \(req) {
        httr2::response(
          status_code = 400L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = testthat::test_path("fixtures", "no_data.xml") |>
            readLines(encoding = "UTF-8") |>
            paste(collapse = "\n") |>
            charToRaw()
        )
      }
    )
    testthat::expect_error(
      object = entsoeapi:::api_req(
        query_string = "documentType=A73&processType=A16",
        security_token = .test_token
      ),
      regexp = "Internal error"
    )
  }
)


testthat::test_that(
  desc = "api_req() handles code-999 offset pagination",
  code = {
    call_count <- 0L
    httr2::local_mocked_responses(
      mock = \(req) {
        call_count <<- call_count + 1L
        if (call_count == 1L) {
          httr2::response(
            status_code = 200L,
            url = req$url,
            headers = list("content-type" = "application/xml"),
            body = testthat::test_path("fixtures", "offset_response.xml") |>
              readLines(encoding = "UTF-8") |>
              paste(collapse = "\n") |>
              charToRaw()
          )
        } else {
          httr2::response(
            status_code = 200L,
            url = req$url,
            headers = list("content-type" = "application/xml"),
            body = entsoe_gen_min_raw
          )
        }
      }
    )
    result <- entsoeapi:::api_req(
      query_string = paste0(
        "documentType=A73&processType=A16",
        "&periodStart=202001302300&periodEnd=202001312300"
      ),
      security_token = .test_token
    )
    testthat::expect_type(object = result, type = "list")
    testthat::expect_length(object = result, n = 2L)
  }
)


testthat::test_that(
  desc = "xml_to_table() merges start_DateAndOrTime date and time columns",
  code = {
    result <- testthat::test_path("fixtures", "merge_date_and_time.xml") |>
      xml2::read_xml() |>
      entsoeapi:::xml_to_table()
    testthat::expect_s3_class(object = result, class = "tbl_df")
    # date and time sub-columns should have been merged and removed
    testthat::expect_false(
      object = str_detect(
        string = names(result),
        pattern = "date_and_or_time_date|date_and_or_time_time"
      ) |>
        any()
    )
  }
)


testthat::test_that(
  desc = "get_resource_object_eic() executes compute_fn on cold cache",
  code = {
    m$reset()
    mock_resource_tbl <- tibble::tibble(
      eic_code      = c("10YDE-VE-------2"),
      eic_long_name = c("DE TSO")
    )
    testthat::local_mocked_bindings(
      resource_object_eic = \(...) mock_resource_tbl,
      .package = "entsoeapi"
    )
    result <- entsoeapi:::get_resource_object_eic()
    testthat::expect_setequal(
      object = names(result),
      expected = c("ts_registered_resource_mrid", "ts_registered_resource_name")
    )
    testthat::expect_s3_class(object = result, class = "tbl_df")
  }
)


testthat::test_that(
  desc = paste(
    "extract_response() triggers progress bar and",
    "processes xml_document list elements"
  ),
  code = {
    m$reset()
    testthat::local_mocked_bindings(
      fetch_eic_csv = \(...) test_df_list[[17L]],
      .package = "entsoeapi"
    )
    content <- list(result = list(entsoe_gen_min_xml), error = NULL)
    result <- entsoeapi:::extract_response(
      content = content,
      progress_bar_limit = 0L
    )
    testthat::expect_s3_class(object = result, class = "tbl_df")
  }
)
