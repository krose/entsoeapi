testthat::test_that(
  desc = "extract_leaf_twig_branch() works 1",
  code = {
    sample_path_1 <- testthat::test_path("fixtures", "cd_catalog.xml")
    content_1 <- list(
      result = xml2::read_xml(x = sample_path_1),
      error = NULL
    )
    testthat::expect_s3_class(
      object = xml2::xml_contents(x = content_1$result) |>
        extract_leaf_twig_branch(),
      class = "tbl"
    )
    testthat::expect_equal(
      object = xml2::xml_contents(x = content_1$result) |>
        extract_leaf_twig_branch() |>
        dim(),
      expected = c(1, 6)
    )
  }
)


testthat::test_that(
  desc = "extract_leaf_twig_branch() works 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    url_sample_2 <- paste(
      "documentType=A73",
      "processType=A16",
      "periodStart=202001302300",
      "periodEnd=202001312300",
      "in_Domain=10YDE-VE-------2",
      sep = "&"
    )
    url_sample_3 <- paste(
      "documentType=A80",
      "biddingZone_Domain=10YFR-RTE------C",
      "periodStart=202407192200",
      "periodEnd=202407232200",
      sep = "&"
    )
    url_sample_4 <- paste(
      "documentType=A73",
      "processType=A16",
      "periodStart=202001302300",
      "periodEnd=202001312300",
      "in_Domain=10YDE-VE-------2",
      sep = "&"
    )
    content_2 <- api_req_safe(
      query_string = url_sample_2,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    content_3 <- api_req_safe(
      query_string = url_sample_3,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    content_4 <- api_req_safe(
      query_string = url_sample_4,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    testthat::expect_setequal(
      object = xml2::xml_contents(x = content_2$result) |>
        extract_leaf_twig_branch() |>
        names() |>
        sort(),
      expected = c(
        "createdDateTime",
        "mRID",
        "process.processType",
        "receiver_MarketParticipant.marketRole.type",
        "receiver_MarketParticipant.mRID",
        "revisionNumber",
        "sender_MarketParticipant.marketRole.type",
        "sender_MarketParticipant.mRID",
        "time_Period.timeInterval.end",
        "time_Period.timeInterval.start",
        "TimeSeries.businessType",
        "TimeSeries.curveType",
        "TimeSeries.inBiddingZone_Domain.mRID",
        "TimeSeries.MktPSRType.PowerSystemResources.mRID",
        "TimeSeries.MktPSRType.PowerSystemResources.name",
        "TimeSeries.MktPSRType.psrType",
        "TimeSeries.mRID",
        "TimeSeries.objectAggregation",
        "TimeSeries.Period.Point.position",
        "TimeSeries.Period.Point.quantity",
        "TimeSeries.Period.resolution",
        "TimeSeries.Period.timeInterval.end",
        "TimeSeries.Period.timeInterval.start",
        "TimeSeries.quantity_Measure_Unit.name",
        "TimeSeries.registeredResource.mRID",
        "type"
      )
    )
    testthat::expect_contains(
      object = xml2::xml_contents(x = content_3$result[[1L]]) |>
        extract_leaf_twig_branch() |>
        names() |>
        sort(),
      expected = c(
        "createdDateTime",
        "mRID",
        "process.processType",
        "Reason.code",
        "receiver_MarketParticipant.marketRole.type",
        "receiver_MarketParticipant.mRID",
        "revisionNumber",
        "sender_MarketParticipant.marketRole.type",
        "sender_MarketParticipant.mRID",
        "TimeSeries.Available_Period.Point.position",
        "TimeSeries.Available_Period.Point.quantity",
        "TimeSeries.Available_Period.resolution",
        "TimeSeries.Available_Period.timeInterval.end",
        "TimeSeries.Available_Period.timeInterval.start",
        "TimeSeries.biddingZone_Domain.mRID",
        "TimeSeries.businessType",
        "TimeSeries.curveType",
        "TimeSeries.end_DateAndOrTime.date",
        "TimeSeries.end_DateAndOrTime.time",
        "TimeSeries.mRID",
        "TimeSeries.production_RegisteredResource.location.name",
        "TimeSeries.production_RegisteredResource.mRID",
        "TimeSeries.production_RegisteredResource.name",
        paste0(
          "TimeSeries.production_RegisteredResource.",
          "pSRType.powerSystemResources.mRID"
        ),
        paste0(
          "TimeSeries.production_RegisteredResource.",
          "pSRType.powerSystemResources.name"
        ),
        paste0(
          "TimeSeries.production_RegisteredResource.",
          "pSRType.powerSystemResources.nominalP"
        ),
        "TimeSeries.production_RegisteredResource.pSRType.psrType",
        "TimeSeries.quantity_Measure_Unit.name",
        "TimeSeries.start_DateAndOrTime.date",
        "TimeSeries.start_DateAndOrTime.time",
        "type",
        "unavailability_Time_Period.timeInterval.end",
        "unavailability_Time_Period.timeInterval.start"
      )
    )
    testthat::expect_equal(
      object = xml2::xml_contents(x = content_4$result) |>
        extract_leaf_twig_branch() |>
        dim(),
      expected = c(546, 26)
    )
    testthat::expect_error(
      object = extract_leaf_twig_branch(nodesets = test_df_6),
      info = paste(
        "no applicable method for 'nodeset_apply'",
        "applied to an object of class 'c('double', 'numeric')'"
      )
    )
  }
)


testthat::test_that(
  desc = "read_zipped_xml() works",
  code = {
    gzipped_sample_fixture <- testthat::test_path("fixtures", "mtcars.gzip")
    zipped_sample_fixture <- testthat::test_path("fixtures", "mtcars.zip")
    zipped_xml_sample_fixture <- testthat::test_path(
      "fixtures", "cd_catalog_xml.zip"
    )
    testthat::expect_warning(
      object = read_zipped_xml(temp_file_path = tempfile()),
      info = "In .f(...) : error 1 in extracting from zip file"
    )
    testthat::expect_warning(
      object = read_zipped_xml(temp_file_path = gzipped_sample_fixture),
      info = "In .f(...) : error 1 in extracting from zip file"
    )
    testthat::expect_error(
      object = read_zipped_xml(temp_file_path = zipped_sample_fixture),
      regexp = "Start tag expected, '<' not found \\[4\\]"
    )
    testthat::expect_no_error(
      object = read_zipped_xml(temp_file_path = zipped_xml_sample_fixture)
    )
  }
)


testthat::test_that(
  desc = "read_zipped_xml() aborts when unzip throws an error",
  code = {
    testthat::local_mocked_bindings(
      unzip = function(...) stop("cannot open the connection"),
      .package = "entsoeapi"
    )
    testthat::expect_error(
      object = read_zipped_xml(temp_file_path = tempfile()),
      regexp = "cannot open the connection"
    )
  }
)


testthat::test_that(
  desc = "calc_offset_urls() works",
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
    testthat::expect_equal(
      object = calc_offset_urls(
        reason = reason_1,
        query_string = url_sample
      ) |>
        length(),
      expected = 6L
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
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    url_sample_1 <- paste(
      "documentType=A73",
      "processType=A16",
      "periodStart=202001302300",
      "periodEnd=202001312300",
      "in_Domain=10YDE-VE-------2",
      sep = "&"
    )
    url_sample_2 <- paste(
      "documentType=A80",
      "biddingZone_Domain=10YFR-RTE------C",
      "periodStart=202407222200",
      "periodEnd=202407232200",
      sep = "&"
    )
    url_sample_3 <- paste(
      "documentType=A65",
      "processType=A16",
      "outBiddingZone_Domain=10YCZ-CEPS-----N",
      "periodStart=202501242300",
      "periodEnd=202501312300",
      sep = "&"
    )
    url_sample_4 <- paste(
      "documentType=A80",
      "biddingZone_Domain=10YFR-RTE------C",
      "periodStart=202407132200",
      "periodEnd=202407232200",
      sep = "&"
    )
    url_sample_5 <- paste(
      "documentType=A75",
      "processType=A16",
      "in_Domain=10YFR-RTE------C",
      "periodStart=202202292300",
      "periodEnd=202403312200",
      sep = "&"
    )
    testthat::expect_no_error(
      object = api_req(
        query_string = url_sample_4,
        security_token = Sys.getenv("ENTSOE_PAT")
      )
    )
    testthat::expect_error(
      object = api_req(
        query_string = url_sample_5,
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      regexp = paste0(
        "(is larger than maximum allowed period 'P1Y'|",
        "Operation timed out)"
      )
    )
    testthat::expect_error(
      object = api_req(
        query_string = paste0(.api_scheme, .api_domain, .api_name),
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      regexp = "Unable to parse URI. Its format is not valid"
    )
    testthat::expect_error(
      object = api_req(
        query_string = NULL,
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
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
      object = api_req(
        query_string = NA,
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      regexp = "Assertion on 'query_string' failed: May not be NA."
    )
    testthat::expect_error(
      object = api_req(
        query_string = paste0(.api_scheme, "google.com/"),
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      regexp = "Unable to parse URI."
    )
    testthat::expect_error(
      object = api_req(
        query_string = "",
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      regexp = paste(
        "The combination of \\[\\] is not valid, or the requested data is not",
        "allowed to be fetched via this service"
      )
    )
    testthat::expect_error(
      object = api_req(
        query_string = url_sample_1
      ),
      regexp = paste(
        "Assertion on 'security_token' failed:",
        "Must be of type 'string', not 'NULL'."
      )
    )
    testthat::expect_s3_class(
      object = api_req(
        query_string = url_sample_1,
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      class = "xml_document"
    )
    testthat::expect_true(
      object = api_req(
        query_string = url_sample_2,
        security_token = Sys.getenv("ENTSOE_PAT")
      ) |>
        lapply(\(x) inherits(x = x, what = "xml_document")) |>
        unlist() |>
        all(),
      info = "The url value should be printed in console!"
    )
  }
)


testthat::test_that(
  desc = "api_req_safe() works",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    url_sample_1 <- paste(
      "documentType=A73",
      "processType=A16",
      "periodStart=202001302300",
      "periodEnd=202001312300",
      "in_Domain=10YDE-VE-------2",
      sep = "&"
    )
    url_sample_2 <- NULL
    testthat::expect_no_error(
      object = content_1 <- api_req_safe(
        query_string = url_sample_1,
        security_token = Sys.getenv("ENTSOE_PAT")
      )
    )
    testthat::expect_true(
      object = "xml_document" %in% class(content_1$result)
    )
    testthat::expect_true(
      object = is.null(content_1$error)
    )
    testthat::expect_true(
      object = is.list(content_1) &&
        length(content_1) == 2L &&
        all(names(content_1) == c("result", "error"))
    )
    testthat::expect_no_error(
      object = content_2 <- api_req_safe(
        query_string = url_sample_2,
        security_token = Sys.getenv("ENTSOE_PAT")
      )
    )
    testthat::expect_true(
      object = "error" %in% class(content_2$error)
    )
    testthat::expect_true(
      object = is.null(content_2$result)
    )
  }
)


testthat::test_that(
  desc = "url_posixct_format() works",
  code = {
    sample_hour <- as.POSIXct("2026-03-25 12:00:00", tz = "CET")
    testthat::expect_null(
      object = url_posixct_format(x = NULL),
      info = "The result of this functions should be NULL!"
    )
    testthat::expect_error(
      object = url_posixct_format(),
      info = "The argument 'x' is missing!"
    )
    testthat::expect_error(
      object = url_posixct_format(x = NA),
      info = "The argument 'x' not in an acceptable timestamp format!"
    )
    testthat::expect_error(
      object = url_posixct_format(x = 101L:110L),
      info = "The argument 'x' not in an acceptable timestamp format!"
    )
    testthat::expect_error(
      object = url_posixct_format(x = 101L),
      info = "The argument 'x' not in an acceptable timestamp format!"
    )
    testthat::expect_error(
      object = url_posixct_format(x = 101.5),
      info = "The argument 'x' not in an acceptable timestamp format!"
    )
    testthat::expect_error(
      object = url_posixct_format(x = "ABC"),
      info = "The argument 'x' not in an acceptable timestamp format!"
    )
    testthat::expect_true(
      object = url_posixct_format(x = sample_hour) |>
        stringr::str_like(pattern = "[0-9]{12}"),
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
    testthat::skip_if_not(
      condition = curl::nslookup(host = .pd_domain, error = FALSE) |>
        is.null() |>
        isFALSE(),
      message = "The Entso-e download site cannot be reached"
    )
    testthat::expect_setequal(
      object = get_eiccodes(
        f = "Y_eicCodes.csv"
      ) |> names(),
      expected = c(
        "eic_code",
        "eic_display_name",
        "eic_long_name",
        "eic_parent",
        "eic_responsible_party",
        "eic_status",
        "market_participant_postal_code",
        "market_participant_iso_country_code",
        "market_participant_vat_code",
        "eic_type_function_list",
        "type"
      )
    )
    testthat::expect_gt(
      object = get_eiccodes(f = "Y_eicCodes.csv") |> nrow(),
      expected = 300
    )
    testthat::expect_null(
      object = get_eiccodes(f = NULL),
      info = "Cannot open the connection!"
    )
    testthat::expect_null(
      object = get_eiccodes(f = "ABC"),
      info = "Cannot open the connection!"
    )
    testthat::expect_no_error(object = get_eiccodes(f = "X_eicCodes.csv"))
    testthat::expect_no_error(object = get_eiccodes(f = "Z_eicCodes.csv"))
    testthat::expect_no_error(object = get_eiccodes(f = "T_eicCodes.csv"))
    testthat::expect_no_error(object = get_eiccodes(f = "V_eicCodes.csv"))
    testthat::expect_no_error(object = get_eiccodes(f = "W_eicCodes.csv"))
    testthat::expect_no_error(object = get_eiccodes(f = "A_eicCodes.csv"))
  }
)


testthat::test_that(
  desc = "tidy_or_not() works",
  code = {
    test_df_list <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    )
    for (i in 1L:5L) {
      assign(x = paste0("test_df_", i), value = test_df_list[[i]])
    }
    testthat::expect_equal(
      object = tidy_or_not(tbl = test_df_1, tidy_output = TRUE) |> dim(),
      expected = c(1L, 4L)
    )
    testthat::expect_equal(
      object = tidy_or_not(tbl = test_df_1, tidy_output = FALSE) |> dim(),
      expected = c(1L, 4L)
    )
    testthat::expect_contains(
      object = tidy_or_not(tbl = test_df_2, tidy_output = TRUE) |>
        names(),
      expected = c(
        "ts_resolution", "ts_reason_code", "ts_time_interval_start",
        "ts_point_price", "ts_point_dt_start"
      )
    )
    testthat::expect_contains(
      object = tidy_or_not(tbl = test_df_2, tidy_output = FALSE) |>
        names(),
      expected = c(
        "ts_resolution", "ts_reason_code", "ts_time_interval_start",
        "ts_point"
      )
    )
    testthat::expect_no_error(
      object = result_3_tidy <- tidy_or_not(tbl = test_df_3, tidy_output = TRUE)
    )
    testthat::expect_contains(
      object = names(result_3_tidy),
      expected = c("bid_ts_point_price", "bid_ts_point_dt_start")
    )
    testthat::expect_false(object = "ts_point_price" %in% names(result_3_tidy))
    testthat::expect_no_error(
      object = result_4 <- tidy_or_not(tbl = test_df_4, tidy_output = FALSE)
    )
    testthat::expect_contains(
      object = names(result_4),
      expected = c("bid_ts_resolution", "bid_ts_mrid")
    )
    testthat::expect_no_error(
      object = result_5 <- tidy_or_not(tbl = test_df_5, tidy_output = TRUE)
    )
    testthat::expect_contains(
      object = names(result_5),
      expected = c("ts_point_dt_start", "ts_point_quantity")
    )
    testthat::expect_equal(object = nrow(result_5), expected = 6L)
  }
)


testthat::test_that(
  desc = "my_snakecase() works",
  code = {
    test_df_6 <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    ) |>
      pluck(6)
    test_df_7 <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    ) |>
      pluck(7)
    testthat::expect_contains(
      object = my_snakecase(tbl = test_df_6),
      expected = c(
        "sepal_length", "sepal_width", "petal_length", "petal_width",
        "species"
      )
    )
    testthat::expect_contains(
      object = my_snakecase(tbl = transmission_pair_eic_dict),
      expected = c(
        "out_area_code", "out_area_type_code", "out_area_name",
        "out_map_code", "in_area_code", "in_area_type_code",
        "in_area_name", "in_map_code"
      )
    )
    testthat::expect_contains(
      object = my_snakecase(tbl = test_df_7),
      expected = c(
        "mrid", "ts_type",
        "unavailability", "a_ts_production",
        "b_ts_production", "c_ts_asset", "d_ts_asset",
        "e_psr_type", "f_psr_type", "g_psr_type",
        "receiver_market_participant_market_role"
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
  desc = "add_type_names() works",
  code = {
    test_df_list <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    )
    for (i in c(6L, 8L, 9L, 12L)) {
      assign(x = paste0("test_df_", i), value = test_df_list[[i]])
    }
    testthat::expect_setequal(
      object = add_type_names(tbl = test_df_8) |>
        names(),
      expected = c(
        "process_type",
        "process_type_def",
        "ts_auction_type",
        "ts_auction_type_def",
        "ts_business_type",
        "ts_business_type_def",
        "type",
        "type_def",
        "market_agreement_type",
        "market_agreement_type_def",
        "ts_asset_psr_type",
        "ts_asset_psr_type_def",
        "ts_production_psr_type",
        "ts_production_psr_type_def",
        "ts_mkt_psr_type",
        "ts_mkt_psr_type_def"
      )
    )
    testthat::expect_no_warning(
      object = add_type_names(tbl = test_df_6),
      message = "No additional definitions added!"
    )
    testthat::expect_no_warning(
      object = add_type_names(tbl = NULL),
      message = "No additional definitions added!"
    )
    # ts_product branch
    testthat::expect_contains(
      object = add_type_names(tbl = test_df_12) |> names(),
      expected = "ts_product_def"
    )
    testthat::expect_no_error(
      object = result_role_dir <- add_type_names(tbl = test_df_9)
    )
    testthat::expect_contains(
      object = names(result_role_dir),
      expected = c(
        "subject_market_participant_market_role_type_def",
        "bid_ts_flow_direction_def"
      )
    )
  }
)


testthat::test_that(
  desc = "add_eic_names() works",
  code = {
    testthat::skip_if_not(
      condition = curl::nslookup(host = .pd_domain, error = FALSE) |>
        is.null() |>
        isFALSE(),
      message = "The Entso-e download site cannot be reached"
    )
    test_df_list <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    )
    for (i in c(6L, 10L)) {
      assign(x = paste0("test_df_", i), value = test_df_list[[i]])
    }
    testthat::expect_contains(
      object = add_eic_names(tbl = test_df_10) |>
        names(),
      expected = c(
        "control_area_domain_mrid",
        "ts_out_domain_mrid",
        "ts_in_domain_mrid",
        "ts_out_bidding_zone_domain_mrid",
        "ts_in_bidding_zone_domain_mrid",
        "ts_bidding_zone_domain_mrid",
        "ts_registered_resource_mrid",
        "ts_registered_resource_name",
        "ts_bidding_zone_domain_name",
        "ts_in_bidding_zone_domain_name",
        "ts_out_bidding_zone_domain_name",
        "ts_in_domain_name",
        "ts_out_domain_name",
        "control_area_domain_name"
      )
    )
    testthat::expect_s3_class(
      object = add_eic_names(tbl = test_df_6),
      class = "data.frame"
    )
    testthat::expect_equal(
      object = add_eic_names(tbl = NULL),
      expected = data.frame()
    )
  }
)


testthat::test_that(
  desc = "add_definitions() works",
  code = {
    test_df_list <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    )
    for (i in c(6L, 11L)) {
      assign(x = paste0("test_df_", i), value = test_df_list[[i]])
    }
    testthat::expect_setequal(
      object = add_definitions(tbl = test_df_11) |>
        names(),
      expected = c(
        "doc_status_value",
        "ts_auction_category",
        "ts_flow_direction",
        "reason_code",
        "ts_reason_code",
        "ts_object_aggregation",
        "doc_status",
        "ts_auction_category_def",
        "ts_flow_direction_def",
        "reason_text",
        "ts_reason_text",
        "ts_object_aggregation_def"
      )
    )
    testthat::expect_s3_class(
      object = add_definitions(tbl = test_df_6),
      class = "data.frame"
    )
    testthat::expect_equal(
      object = add_definitions(tbl = NULL),
      expected = data.frame()
    )
  }
)


testthat::test_that(
  desc = "xml_to_table() works 1",
  code = {
    test_df_6 <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    ) |>
      pluck(6)
    xml_fixture <- testthat::test_path("fixtures", "order-schema.xml") |>
      xml2::read_xml()
    testthat::expect_error(
      object = xml_to_table(xml_content = xml_fixture, tidy_output = FALSE),
      info = "There is no interesting columns in the result table!"
    )
    testthat::expect_error(
      object = xml_to_table(xml_content = test_df_6),
      info = "The 'xml_content' should be an xml document!"
    )
    testthat::expect_error(
      object = xml_to_table(xml_content = NULL),
      info = "The 'xml_content' should be an xml document!"
    )
    testthat::expect_error(
      object = xml_to_table(xml_content = NA),
      info = "The 'xml_content' should be an xml document!"
    )
  }
)


testthat::test_that(
  desc = "xml_to_table() works 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    url_sample_1 <- paste(
      "documentType=A73",
      "processType=A16",
      "periodStart=202001302300",
      "periodEnd=202001312300",
      "in_Domain=10YDE-VE-------2",
      sep = "&"
    )
    url_sample_2 <- paste(
      "documentType=A44",
      "in_Domain=10YDK-1--------W",
      "out_Domain=10YDK-1--------W",
      "periodStart=201910312300",
      "periodEnd=201911302300",
      sep = "&"
    )
    url_sample_3 <- paste(
      "documentType=A63",
      "businessType=A85",
      "in_Domain=10YNL----------L",
      "out_Domain=10YNL----------L",
      "periodStart=202402292300",
      "periodEnd=202403312300",
      sep = "&"
    )
    url_sample_4 <- paste(
      "documentType=A63",
      "businessType=A85",
      "in_Domain=10YNL----------L",
      "out_Domain=10YNL----------L",
      "periodStart=202310302300",
      "periodEnd=202311302300",
      sep = "&"
    )
    url_sample_5 <- paste(
      "documentType=A65",
      "businessType=A85",
      "in_Domain=10YNO-0--------C",
      "out_Domain=10YNO-0--------C",
      "periodStart=202402292300",
      "periodEnd=202403102300",
      sep = "&"
    )
    content_1 <- api_req_safe(
      query_string = url_sample_1,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    content_2 <- api_req_safe(
      query_string = url_sample_2,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    content_3 <- api_req_safe(
      query_string = url_sample_3,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    content_4 <- api_req_safe(
      query_string = url_sample_4,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    content_5 <- api_req_safe(
      query_string = url_sample_5,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    testthat::expect_no_error(
      object = xml_to_table(
        xml_content = content_1$result,
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = xml_to_table(
        xml_content = content_2$result,
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = xml_to_table(
        xml_content = content_3$result,
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = xml_to_table(
        xml_content = content_4$result,
        tidy_output = TRUE
      )
    )
    testthat::expect_error(
      object = xml_to_table(
        xml_content = content_5$result,
        tidy_output = TRUE
      ),
      regexp = "The 'xml_content' should be an xml document"
    )
    testthat::expect_s3_class(
      object = xml_to_table(xml_content = content_1$result),
      class = "data.frame"
    )
    testthat::expect_gt(
      object = xml_to_table(xml_content = content_1$result) |>
        nrow(),
      expected = 0L
    )
    testthat::expect_gt(
      object = xml_to_table(xml_content = content_1$result) |>
        ncol(),
      expected = 0L
    )
  }
)


testthat::test_that(
  desc = "extract_response() works 1",
  code = {
    content_1 <- list(
      result = testthat::test_path("fixtures", "cd_catalog.xml") |>
        xml2::read_xml(),
      error = NULL
    )
    test_df_6 <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    ) |>
      pluck(6)
    testthat::expect_error(
      object = extract_response(content = content_1),
      info = "There is no interesting columns in the result table!"
    )
    testthat::expect_error(
      object = extract_response(content = test_df_6),
      info = "The content is not in the required list format!"
    )
    testthat::expect_error(
      object = extract_response(content = NULL),
      info = "The argument 'tbl' is missing!"
    )
    testthat::expect_error(
      object = extract_response(content = list(result = "A", error = "B")),
      info = paste(
        "Error in extract_response(content",
        "= list(result = 'A', error = 'B'))"
      )
    )
  }
)


testthat::test_that(
  desc = "extract_response() works 2",
  code = {
    testthat::skip_if_not(
      condition = nchar(Sys.getenv("ENTSOE_PAT")) > 0L,
      message = "No ENTSOE_PAT environment variable set"
    )
    testthat::skip_if_not(
      condition = there_is_provider(),
      message = "The Entso-e API cannot be reached"
    )
    url_sample_2 <- paste(
      "documentType=A73",
      "processType=A16",
      "periodStart=202001302300",
      "periodEnd=202001312300",
      "in_Domain=10YDE-VE-------2",
      sep = "&"
    )
    url_sample_3 <- paste(
      "documentType=A80",
      "biddingZone_Domain=10YFR-RTE------C",
      "periodStart=202407192200",
      "periodEnd=202407232200",
      sep = "&"
    )
    url_sample_4 <- paste(
      "documentType=A73",
      "processType=A16",
      "periodStart=202001302300",
      "periodEnd=202001312300",
      "in_Domain=10YDE-VE-------2",
      sep = "&"
    )
    content_2 <- api_req_safe(
      query_string = url_sample_2,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    content_3 <- api_req_safe(
      query_string = url_sample_3,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    content_4 <- api_req_safe(
      query_string = url_sample_4,
      security_token = Sys.getenv("ENTSOE_PAT")
    )
    testthat::expect_no_error(
      object = extract_response(
        content = content_2,
        tidy_output = TRUE
      )
    )
    testthat::expect_no_error(
      object = extract_response(
        content = content_3,
        tidy_output = FALSE
      )
    )
    testthat::expect_true(
      object = extract_response(
        content = content_4,
        tidy_output = TRUE
      ) |>
        inherits(what = "data.frame")
    )
  }
)


testthat::test_that(
  desc = paste(
    "get_all_allocated_eic() returns a tibble",
    "with expected columns on valid XML"
  ),
  code = {
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
          headers = list("content-type" = "application/xml"),
          body = xml_fixture
        )
      }
    )
    testthat::expect_s3_class(
      object = tbl <- get_all_allocated_eic(),
      class = "tbl_df",
      exact = FALSE
    )
    testthat::expect_gt(
      object = nrow(tbl),
      expected = 0L
    )
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
        "responsible_market_participant_mrid",
        "function_names",
        "parent_market_document_mrid"
      )
    )
    testthat::expect_false(object = anyNA(tbl$created_date_time))
  }
)


testthat::test_that(
  desc =
    "get_all_allocated_eic() joins doc_status from message_types correctly",
  code = {
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
          headers = list("content-type" = "application/xml"),
          body = xml_fixture
        )
      }
    )
    tbl <- get_all_allocated_eic()
    # A05 is in the fixture; its title in message_types
    # is "Control block area schedule"
    testthat::expect_equal(
      object = tbl$doc_status[[1L]],
      expected = "Control block area schedule"
    )
  }
)


testthat::test_that(
  desc =
    "get_all_allocated_eic() stops with HTTP error message and request URL",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
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
      mock = function(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = raw(0L)
        )
      }
    )
    testthat::expect_error(object = get_all_allocated_eic())
  }
)


testthat::test_that(
  desc = "get_all_allocated_eic() stops on XML with unexpected tree structure",
  code = {
    minimal_xml <- readLines(
      con = testthat::test_path("fixtures", "minimal.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
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
      object = get_all_allocated_eic(),
      regexp = "unexpected tree structure"
    )
  }
)


testthat::test_that(
  desc =
    "get_all_allocated_eic() returns one row per EICCode_MarketDocument node",
  code = {
    multi_eic_xml <- readLines(
      con = testthat::test_path("fixtures", "get_allocated_eic_min.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = multi_eic_xml
        )
      }
    )
    tbl <- get_all_allocated_eic()
    testthat::expect_equal(object = nrow(tbl), expected = 2L)
    testthat::expect_setequal(
      object = tbl$eic_code,
      expected = c("10X-TEST--EIC--1", "10X-TEST--EIC--2")
    )
  }
)


testthat::test_that(
  desc =
    "get_all_allocated_eic() collapses duplicate Function_Names with ' - '",
  code = {
    dupl_fn_xml <- readLines(
      con = testthat::test_path("fixtures", "get_allocated_eic_min.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/xml"),
          body = dupl_fn_xml
        )
      }
    )
    tbl <- get_all_allocated_eic()
    testthat::expect_equal(object = nrow(tbl), expected = 2L)
    testthat::expect_true(
      object = grepl(
        pattern = " - ",
        x = tbl$function_names[[1L]],
        fixed = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = paste(
    "get_all_allocated_eic() stops with error message",
    "and URL on no internet connection"
  ),
  code = {
    curl_err <- structure(
      class = c("curl_error", "error", "condition"),
      list(message = paste(
        "Could not resolve host:",
        "eepublicdownloads.blob.core.windows.net"
      ))
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
      object = get_all_allocated_eic(),
      regexp = "Failed to perform HTTP request\\."
    )
    testthat::expect_error(
      object = get_all_allocated_eic(),
      regexp = "eepublicdownloads\\.blob\\.core\\.windows\\.net"
    )
  }
)


testthat::test_that(
  desc = "tidy_or_not() stops on unknown curve_type",
  code = {
    test_df_13 <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    ) |>
      pluck(13)
    testthat::expect_error(
      object = tidy_or_not(tbl = test_df_13, tidy_output = TRUE),
      regexp = "The curve type is not defined, but A99!"
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
      mock = function(req) {
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
      object = grepl(
        pattern = "GENERATION - LOAD",
        x = tbl$function_names[[1L]],
        fixed = TRUE
      )
    )
  }
)


testthat::test_that(
  desc = paste(
    "get_all_allocated_eic() stops with 'unexpected tree structure'",
    "when bind_cols raises an error"
  ),
  code = {
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
      object = get_all_allocated_eic(),
      regexp = "unexpected tree structure"
    )
  }
)


testthat::test_that(
  desc = "add_eic_names() adds names for additional domain mrid columns",
  code = {
    testthat::skip_if_not(
      condition = curl::nslookup(host = .pd_domain, error = FALSE) |>
        is.null() |>
        isFALSE(),
      message = "The Entso-e download site cannot be reached"
    )
    test_df_14 <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    ) |>
      pluck(14)
    testthat::expect_no_error(
      object = result <- add_eic_names(tbl = test_df_14)
    )
    testthat::expect_setequal(
      object = names(result),
      expected = c(
        "area_domain_mrid",
        "area_domain_name",
        "ts_acquiring_domain_mrid",
        "ts_acquiring_domain_name",
        "ts_connecting_domain_mrid",
        "ts_connecting_domain_name",
        "bid_ts_acquiring_domain_mrid",
        "bid_ts_acquiring_domain_name",
        "bid_ts_connecting_domain_mrid",
        "bid_ts_connecting_domain_name",
        "domain_mrid",
        "domain_name",
        "constraint_ts_monitored_ptdf_domain_mrid",
        "constraint_ts_monitored_ptdf_domain_name"
      )
    )
  }
)


testthat::test_that(
  desc = "add_definitions() unites multiple ts_reason_code columns",
  code = {
    test_df_15 <- readRDS(
      file = testthat::test_path("fixtures", "test_df_list.rds")
    ) |>
      pluck(15)
    testthat::expect_no_error(
      object = result <- add_definitions(tbl = test_df_15)
    )
    testthat::expect_contains(
      object = names(result),
      expected = c("ts_reason_code", "ts_reason_text")
    )
    testthat::expect_true(
      object = all(grepl(" - ", result$ts_reason_text, fixed = TRUE))
    )
  }
)


testthat::test_that(
  desc = "extract_leaf_twig_branch() cross-joins unequal-group XML",
  code = {
    result <- readLines(
      con = testthat::test_path("fixtures", "unequal_group.xml"),
      encoding = "UTF-8"
    ) |>
      paste0(collapse = "") |>
      xml2::read_xml() |>
      xml2::xml_contents() |>
      extract_leaf_twig_branch()
    testthat::expect_equal(object = nrow(result), expected = 6L)
    testthat::expect_equal(object = ncol(result), expected = 2L)
  }
)


testthat::test_that(
  desc = "extract_response() returns empty tibble for NULL result element",
  code = {
    content_null <- list(result = list(NULL), error = NULL)
    result <- extract_response(content = content_null)
    testthat::expect_s3_class(object = result, class = "tbl_df", exact = FALSE)
    testthat::expect_equal(object = nrow(result), expected = 0L)
  }
)


testthat::test_that(
  desc = "extract_response() processes a list-of-xml_documents element",
  code = {
    xml_doc <- xml2::xml2_example(path = "order-schema.xml") |>
      xml2::read_xml()
    content_alldoc <- list(result = list(list(xml_doc)), error = NULL)
    # xml_to_table on order-schema.xml fails because it has no ENTSO-E columns;
    # the important thing is the all_doc branch is entered (lines 2037-2041)
    testthat::expect_error(
      object = extract_response(content = content_alldoc)
    )
  }
)


testthat::test_that(
  desc = "extract_response() returns empty tibble for list of non-documents",
  code = {
    content_nondoc <- list(result = list(list(1L, 2L, 3L)), error = NULL)
    result <- extract_response(content = content_nondoc)
    testthat::expect_s3_class(object = result, class = "tbl_df", exact = FALSE)
    testthat::expect_equal(object = nrow(result), expected = 0L)
  }
)


testthat::test_that(
  desc = "api_req() stops on unknown 200 response content-type",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
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
        security_token = "dummy_token"
      ),
      regexp = "Not known response content-type: text/plain"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops on HTML error response",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
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
    # The stop() message may be empty when xmlconvert::xml_to_list()|>pluck()
    # returns NULL for the HTML body; what matters is that the HTML error branch
    # (lines 721-730) is reached and an error is thrown.
    testthat::expect_error(
      object = api_req(
        query_string = "documentType=A73",
        security_token = "dummy_token"
      )
    )
  }
)


testthat::test_that(
  desc = "api_req() stops on XML error with unexpected Reason structure",
  code = {
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "something_went_wrong.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 500L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = xml_fixture
        )
      }
    )
    testthat::expect_error(
      object = api_req(
        query_string = "documentType=A73",
        security_token = "dummy_token"
      ),
      regexp = "^500"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops with code:text message on non-999 XML error code",
  code = {
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "not_available.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 400L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = xml_fixture
        )
      }
    )
    testthat::expect_error(
      object = api_req(
        query_string = "documentType=A73",
        security_token = "dummy_token"
      ),
      regexp = "B11"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops with curl error message on no internet connection",
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
    httr2::local_mocked_responses(
      mock = function(req) stop(httr2_err)
    )
    testthat::expect_error(
      object = api_req(
        query_string = paste(
          "documentType=A73",
          "processType=A16",
          "periodStart=202001302300",
          "periodEnd=202001312300",
          "in_Domain=10YDE-VE-------2",
          sep = "&"
        ),
        security_token = "dummy_token"
      ),
      regexp = "Could not resolve host: web-api\\.tp\\.entsoe\\.eu"
    )
  }
)


testthat::test_that(
  desc = "api_req() stops with HTTP 503 message on service unavailable",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
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
        security_token = "dummy_token"
      ),
      regexp = "HTTP 503"
    )
  }
)


testthat::test_that(
  desc = "there_is_provider() returns TRUE on 401 Unauthorized",
  code = {
    httr2::local_mocked_responses(
      mock = function(req) {
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
      mock = function(req) {
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
    httr2::local_mocked_responses(
      mock = function(req) stop(httr2_err)
    )
    testthat::expect_false(
      object = there_is_provider()
    )
  }
)


testthat::test_that(
  desc = "there_is_provider() uses custom api_scheme, api_domain, and api_name",
  code = {
    captured_url <- NULL
    httr2::local_mocked_responses(
      mock = function(req) {
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
    httr2::local_mocked_responses(
      mock = function(req) {
        stop(structure(
          class = c("httr2_failure", "httr2_error", "error", "condition"),
          list(
            message = "Failed to perform HTTP request.",
            resp = NULL,
            parent = structure(
              class = c("curl_error", "error", "condition"),
              list(message = "Could not connect to web-api.tp.entsoe.eu")
            )
          )
        ))
      }
    )
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
  desc = "assert_eic() throws for strings with wrong length",
  code = {
    testthat::expect_error(object = assert_eic(eic = ""))
    testthat::expect_error(object = assert_eic(eic = "10YCZ-CEPS----N"))
    testthat::expect_error(object = assert_eic(eic = "10YCZ-CEPS------N"))
  }
)


testthat::test_that(
  desc = "assert_eic() throws for strings with disallowed characters",
  code = {
    testthat::expect_error(object = assert_eic(eic = "10YCZ-CEPS----!N"))
    testthat::expect_error(object = assert_eic(eic = "10ycz-ceps-----n"))
  }
)


testthat::test_that(
  desc = "assert_eic() respects null_ok argument",
  code = {
    testthat::expect_invisible(
      call = result <- assert_eic(eic = NULL, null_ok = TRUE)
    )
    testthat::expect_null(object = result)
    testthat::expect_error(object = assert_eic(eic = NULL, null_ok = FALSE))
  }
)


testthat::test_that(
  desc = "assert_eic() uses var_name in error messages",
  code = {
    testthat::expect_error(
      assert_eic(eic = "BAD-EIC-CODE----", var_name = "my_eic"),
      regexp = "Invalid EIC checksum character"
    )
    testthat::expect_error(
      object = assert_eic(eic = "SHORT", var_name = "my_eic"),
      regexp = "All elements must have exactly 16 characters"
    )
  }
)


# ── pluck() ───────────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "pluck() extracts a nested element by name",
  code = {
    x <- list(a = list(b = list(c = 42L)))
    testthat::expect_equal(object = pluck(x, "a", "b", "c"), expected = 42L)
  }
)


testthat::test_that(
  desc = "pluck() extracts by integer index",
  code = {
    x <- list(list(10L, 20L), list(30L, 40L))
    testthat::expect_equal(object = pluck(x, 2L, 1L), expected = 30L)
  }
)


testthat::test_that(
  desc = "pluck() returns NULL when path traversal hits NULL",
  code = {
    x <- list(a = NULL)
    testthat::expect_null(object = pluck(x, "a", "b"))
  }
)


testthat::test_that(
  desc = "pluck() with no keys returns the input unchanged",
  code = {
    x <- list(a = 1L)
    testthat::expect_equal(object = pluck(x), expected = x)
  }
)


# ── safely() ──────────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "safely() returns result and NULL error on success",
  code = {
    safe_sqrt <- safely(sqrt)
    out <- safe_sqrt(4)
    testthat::expect_equal(object = out$result, expected = 2)
    testthat::expect_null(object = out$error)
  }
)


testthat::test_that(
  desc = "safely() returns NULL result and error object on failure",
  code = {
    safe_log <- safely(log)
    out <- safe_log("not a number")
    testthat::expect_null(object = out$result)
    testthat::expect_s3_class(object = out$error, class = "error")
  }
)


# ── extract_nodesets: NULL named_vect (L289) ─────────────────────────────────

testthat::test_that(
  desc = "extract_nodesets() converts nodeset with no text content to NA",
  code = {
    doc <- xml2::read_xml(x = "<root><empty/></root>")
    nodesets <- list(xml2::xml_find_first(x = doc, xpath = "//empty"))
    result <- entsoeapi:::extract_nodesets(nodesets = nodesets)
    testthat::expect_length(object = result, n = 1L)
    tbl <- result[[1L]]
    testthat::expect_true(object = tibble::is_tibble(x = tbl))
    testthat::expect_true(object = all(is.na(tbl[[1L]])))
  }
)


# ── extract_nodesets: non-conformable column lengths ──────────────

testthat::test_that(
  desc = "extract_nodesets() warns on non-conformable column lengths",
  code = {
    # Build XML where unlist() produces columns of length 3 and length 2,
    # which are non-conformable (3 %% 2 != 0).
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "semi_structured.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw() |>
      xml2::read_xml()
    nodesets <- list(xml2::xml_find_first(x = xml_fixture, xpath = "//wrapper"))
    testthat::expect_warning(
      object = entsoeapi:::extract_nodesets(nodesets = nodesets),
      regexp = "recycling with truncation"
    )
  }
)


# ── node_to_rows: terminal node with prefix = NULL ────────────────

testthat::test_that(
  desc = "node_to_rows() handles terminal node with prefix = NULL",
  code = {
    doc <- xml2::read_xml(x = "<root><leaf>hello</leaf></root>")
    leaf_node <- xml2::xml_find_first(x = doc, xpath = "//leaf")
    result <- entsoeapi:::node_to_rows(node = leaf_node, prefix = NULL)
    testthat::expect_length(object = result, n = 1L)
    testthat::expect_equal(object = result[[1L]], expected = c(leaf = "hello"))
  }
)


testthat::test_that(
  desc = "node_to_rows() returns empty string for empty terminal node",
  code = {
    doc <- xml2::read_xml(x = "<root><leaf/></root>")
    leaf_node <- xml2::xml_find_first(x = doc, xpath = "//leaf")
    result <- entsoeapi:::node_to_rows(node = leaf_node, prefix = NULL)
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
      xml_text = function(x, ...) NA,
      .package = "entsoeapi"
    )
    doc <- xml2::read_xml(x = "<root><leaf>value</leaf></root>")
    leaf_node <- xml2::xml_find_first(x = doc, xpath = "//leaf")
    result <- entsoeapi:::node_to_rows(node = leaf_node, prefix = NULL)
    testthat::expect_length(object = result, n = 1L)
    testthat::expect_true(object = is.na(result[[1L]]))
    testthat::expect_equal(object = names(result[[1L]]), expected = "leaf")
  }
)


# ── node_to_rows: leaf with internal structure ──────────

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
      xml_children = function(x, ...) {
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

    doc <- xml2::read_xml(x = paste0(
      "<parent>",
      "<compound>text</compound>",
      "</parent>"
    ))
    parent_node <- xml2::xml_find_first(x = doc, xpath = "//parent")
    result <- entsoeapi:::node_to_rows(node = parent_node, prefix = NULL)
    testthat::expect_length(object = result, n = 1L)
    row <- result[[1L]]
    # The name should be "compound.a" and the value "X".
    testthat::expect_equal(object = names(row), expected = "compound.a")
    testthat::expect_equal(object = unname(row), expected = "X")
  }
)


# ── node_to_rows: stacked_rows with nested children ──────────────────

testthat::test_that(
  desc = paste(
    "node_to_rows() uses stacked_rows[[1L]] as leaf_row",
    "when duplicate leaves and nested children exist"
  ),
  code = {
    # Duplicate leaf names + nested children
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "duplicate_leaf_names.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw() |>
      xml2::read_xml()
    parent_node <- xml2::xml_find_first(x = xml_fixture, xpath = "//parent")
    result <- entsoeapi:::node_to_rows(node = parent_node, prefix = "p")
    testthat::expect_true(object = length(result) >= 1L)
    # Each row should have both val and nested.inner columns
    row <- result[[1L]]
    testthat::expect_true(object = "p.val" %in% names(row))
    testthat::expect_true(object = "p.nested.inner" %in% names(row))
  }
)


# ── node_to_rows: both leaf_row and nested_rows empty ────────────────

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
      node_to_rows = function(node, prefix = NULL) {
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
    xml_fixture <- xml2::read_xml(
      x = testthat::test_path("fixtures", "empty_leaf_and_nested.xml")
    )
    parent_node <- xml2::xml_find_first(x = xml_fixture, xpath = "//parent")
    result <- entsoeapi:::node_to_rows(node = parent_node, prefix = "p")
    testthat::expect_equal(object = result, expected = list())
  }
)


# ── node_to_rows: stacked_rows exists but nested_rows empty ──────

testthat::test_that(
  desc = paste(
    "node_to_rows() returns stacked_rows when duplicate leaf names exist",
    "but no nested children are present"
  ),
  code = {
    xml_fixture <- testthat::test_path("fixtures", "dupl_leaf_names.xml") |>
      xml2::read_xml()
    parent_node <- xml2::xml_find_first(x = xml_fixture, xpath = "//parent")
    result <- entsoeapi:::node_to_rows(node = parent_node, prefix = "p")
    testthat::expect_length(object = result, n = 3L)
    testthat::expect_equal(object = result[[1L]], expected = c(p.val = "1"))
    testthat::expect_equal(object = result[[2L]], expected = c(p.val = "2"))
    testthat::expect_equal(object = result[[3L]], expected = c(p.val = "3"))
  }
)


# ── rows_to_tbl: empty input ─────────────────────────────────────────

testthat::test_that(
  desc = "rows_to_tbl() returns empty tibble on empty input",
  code = {
    result <- entsoeapi:::rows_to_tbl(list())
    testthat::expect_true(object = tibble::is_tibble(result))
    testthat::expect_equal(object = nrow(result), expected = 0L)
    testthat::expect_equal(object = ncol(result), expected = 0L)
  }
)


# ── extract_leaf_twig_branch: both leaf and nested empty ──────────────

testthat::test_that(
  desc = paste(
    "extract_leaf_twig_branch() returns empty tibble",
    "when both leaf_rows and nested_all_rows are empty"
  ),
  code = {
    result <- entsoeapi:::extract_leaf_twig_branch(nodesets = list())
    testthat::expect_true(object = tibble::is_tibble(result))
    testthat::expect_equal(object = nrow(result), expected = 0L)
    testthat::expect_equal(object = ncol(result), expected = 0L)
  }
)


# ── api_req: HTML error response path ──────────────────────────

testthat::test_that(
  desc = "api_req() extracts body text from HTML error response",
  code = {
    xml_fixture <- readLines(
      con = testthat::test_path("fixtures", "bad_request.xml"),
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n") |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 400L,
          url = req$url,
          headers = list("content-type" = "text/html"),
          body = xml_fixture
        )
      }
    )
    testthat::expect_error(
      object = entsoeapi:::api_req(
        query_string = "documentType=A73",
        security_token = "dummy_token"
      ),
      regexp = "400.*Bad Request"
    )
  }
)


# ── xml_to_table: unexpected XML tree structure ─────────────────

testthat::test_that(
  desc = "xml_to_table() aborts on unexpected XML tree structure",
  code = {
    # Mock extract_leaf_twig_branch to throw an error
    testthat::local_mocked_bindings(
      extract_leaf_twig_branch = function(...) {
        stop("simulated extraction failure")
      },
      .package = "entsoeapi"
    )
    xml_fixture <- xml2::read_xml(
      x = testthat::test_path("fixtures", "unexpected_structure.xml")
    )
    testthat::expect_error(
      object = entsoeapi:::xml_to_table(xml_content = xml_fixture),
      regexp = "unexpected tree structure"
    )
  }
)
