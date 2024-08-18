testthat::test_that(
  desc = "read_zipped_xml() works",
  code = {
    data(mtcars)
    mtcars$make <- row.names(mtcars)
    gzip_sample_file <- tempfile(fileext = ".gzip")
    data.table::fwrite(x = mtcars, file = gzip_sample_file, compress = "gzip")
    csv_sample_file <- tempfile(fileext = ".csv")
    zip_sample_file <- tempfile(fileext = ".zip")
    xml_sample_file <- tempfile(fileext = "xml")
    zip_xml_sample_file <- tempfile(fileext = ".zip")
    data.table::fwrite(x = mtcars, file = csv_sample_file, sep = ";")
    zip(zipfile = zip_sample_file,
        files = c(csv_sample_file))
    cd_cat_xml <- xml2::read_xml(xml2::xml2_example(path = "cd_catalog.xml"))
    xml2::write_xml(x = cd_cat_xml, file = xml_sample_file)
    zip(zipfile = zip_xml_sample_file,
        files = c(xml_sample_file))
    testthat::expect_warning(
      object = read_zipped_xml(temp_file_path = tempfile()),
      info = "In .f(...) : error 1 in extracting from zip file"
    )
    testthat::expect_warning(
      object = read_zipped_xml(temp_file_path = gzip_sample_file),
      info = "In .f(...) : error 1 in extracting from zip file"
    )
    testthat::expect_error(
      object = read_zipped_xml(temp_file_path = zip_sample_file),
      info = "Start tag expected, '<' not found [4]"
    )
    testthat::expect_no_error(
      object = read_zipped_xml(temp_file_path = zip_xml_sample_file)
    )
  }
)



testthat::test_that(
  desc = "calc_offset_urls() works",
  code = {
    url_sample_3 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "&documentType=A65",
      "&processType=A16",
      "&outBiddingZone_Domain=10YCZ-CEPS-----N",
      "&periodStart=202412312300",
      "&periodEnd=202501312300",
      "?securityToken="
    )
    reason_1 <- "allowed: 200; requested: 1111"
    reason_2 <- "allowed: -200; requested: -1111"
    reason_3 <- "foo; bar"
    testthat::expect_equal(
      object = calc_offset_urls(reason = reason_1, url = url_sample_3) |>
        length(),
      expected = 6L
    ) |>
      testthat::expect_message()
    testthat::expect_error(
      object = calc_offset_urls(reason = reason_2, url = url_sample_3),
      info = "The 'from' argument must be a finite number!"
    )
    testthat::expect_error(
      object = calc_offset_urls(reason = reason_3, url = url_sample_3),
      info = "The 'from' argument must be a finite number!"
    )
  }
)



testthat::test_that(
  desc = "api_req() works",
  code = {
    api_url <- "https://web-api.tp.entsoe.eu/api"
    url_sample_1 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A73",
      "&processType=A16",
      "&periodStart=202001302300",
      "&periodEnd=202001312300",
      "&in_Domain=10YDE-VE-------2",
      "&securityToken="
    )
    url_sample_2 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A80",
      "&biddingZone_Domain=10YFR-RTE------C",
      "&periodStart=202407222200",
      "&periodEnd=202407232200",
      "&securityToken="
    )
    url_sample_3 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "&documentType=A65",
      "&processType=A16",
      "&outBiddingZone_Domain=10YCZ-CEPS-----N",
      "&periodStart=202501242300",
      "&periodEnd=202501312300",
      "?securityToken="
    )
    url_sample_4 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A80",
      "&biddingZone_Domain=10YFR-RTE------C",
      "&periodStart=202407132200",
      "&periodEnd=202407232200",
      "&securityToken="
    )
    testthat::expect_no_error(
      object = api_req(url = paste0(url_sample_4, Sys.getenv("ENTSOE_PAT")))
    )
    testthat::expect_error(
      object = api_req(url = "https://web-api.tp.entsoe.eu/api"),
      info = "Unauthorized. Missing or invalid security token"
    )
    testthat::expect_error(
      object = api_req(url = NULL),
      info = "The argument 'url' is missing!"
    )
    testthat::expect_error(
      object = api_req(),
      info = "The argument 'url' is missing!"
    )
    testthat::expect_error(
      object = api_req(url = NA),
      info = "The argument 'url' not in an acceptable timestamp format!"
    )
    testthat::expect_error(
      object = api_req(url = "https://google.com/"),
      info = "The argument 'url' is not valid!"
    )
    testthat::expect_error(
      object = api_req(url = api_url),
      info = "The argument 'url' is not valid!"
    )
    testthat::expect_message(
      object = api_req(url = paste0(url_sample_1, Sys.getenv("ENTSOE_PAT"))),
      info = "The url value should be printed in console!"
    )
    testthat::expect_true(
      object = api_req(url = paste0(url_sample_1, Sys.getenv("ENTSOE_PAT"))) |>
        inherits(what = "xml_document"),
      info = "The url value should be printed in console!"
    )
    testthat::expect_true(
      object = api_req(url = paste0(url_sample_2, Sys.getenv("ENTSOE_PAT"))) |>
        purrr::map(~inherits(x = .x, what = "xml_document")) |>
        unlist() |>
        all(),
      info = "The url value should be printed in console!"
    )
  }
)



testthat::test_that(
  desc = "api_req_safe() works",
  code = {
    url_sample_1 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A73",
      "&processType=A16",
      "&periodStart=202001302300",
      "&periodEnd=202001312300",
      "&in_Domain=10YDE-VE-------2",
      "&securityToken="
    )
    content <- api_req_safe(
      url = paste0(url_sample_1, Sys.getenv("ENTSOE_PAT"))
    )
    testthat::expect_true(
      object = is.list(content) &&
        length(content) == 2L &&
        all(names(content) == c("result", "error"))
    )
    testthat::expect_no_error(
      object = api_req_safe(url = "https://web-api.tp.entsoe.eu/api")
    )
  }
)


testthat::test_that(
  desc = "url_posixct_format() works",
  code = {
    current_hour <- lubridate::floor_date(
      x = Sys.time(),
      unit = "hour"
    )
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
      object = url_posixct_format(x = current_hour) |>
        stringr::str_like(pattern = "[0-9]{12}"),
      info = "The result of this functions should be 12 digit length string!"
    )
    testthat::expect_warning(
      object = url_posixct_format(x = "20240722210000"),
      info = "The 'x' value has interpreted as UTC!"
    )
  }
)



testthat::test_that(  # TODO: 209, 219, 235
  desc = "dt_seq_helper() works",
  code = {
    start_sample_1 <- lubridate::floor_date(
      x = Sys.time() |>
        lubridate::with_tz(tzone = "UTC"),
      unit = "hour"
    )
    end_sample_1 <- start_sample_1 + lubridate::hours(x = 10L)
    testthat::expect_error(
      object = dt_seq_helper(
        from = start_sample_1,
        to = end_sample_1,
        pos = 1L:10L,
        qty = 101L:110L,
        seq_resolution = "PT86M"
      ),
      info = "The provided 'seq_resolution' value is not supported!"
    )
    start_sample_2 <- lubridate::ymd(Sys.Date(), tz = "CET") |>
      lubridate::with_tz(tzone = "UTC")
    end_sample_2 <- start_sample_2 + lubridate::years(x = 2L)
    testthat::expect_no_error(
      object = dt_seq_helper(
        from = start_sample_2,
        to = end_sample_2,
        pos = 1L:2L,
        qty = 101L:102L,
        seq_resolution = "P1Y"
      )
    )
    start_sample_3 <- lubridate::ymd_hms("2024-03-31 01:00:00", tz = "CET") |>
      lubridate::with_tz(tzone = "UTC")
    end_sample_3 <- start_sample_3 + lubridate::hours(x = 2L)
    testthat::expect_no_error(
      object = dt_seq_helper(
        from = start_sample_3,
        to = end_sample_3,
        pos = 1L:2L,
        qty = 101L:102L,
        seq_resolution = "PT60M"
      )
    )
    start_sample_4 <- lubridate::ymd("2024-03-20", tz = "CET") |>
      lubridate::with_tz(tzone = "UTC")
    end_sample_4 <- lubridate::ymd("2024-04-10", tz = "CET") |>
      lubridate::with_tz(tzone = "UTC")
    testthat::expect_no_error(
      object = dt_seq_helper(
        from = start_sample_4,
        to = end_sample_4,
        pos = 1L:3L,
        qty = 101L:103L,
        seq_resolution = "P7D"
      )
    )
    start_sample_5 <- lubridate::ymd("2024-03-27", tz = "CET") |>
      lubridate::with_tz(tzone = "UTC")
    end_sample_5 <- lubridate::ymd("2024-04-10", tz = "CET") |>
      lubridate::with_tz(tzone = "UTC")
    testthat::expect_no_error(
      object = dt_seq_helper(
        from = start_sample_5,
        to = end_sample_5,
        pos = 1L:2L,
        qty = 103L:104L,
        seq_resolution = "P7D"
      )
    )
    start_sample_6 <- lubridate::ymd("2024-03-31", tz = "CET") |>
      lubridate::with_tz(tzone = "UTC")
    end_sample_6 <- lubridate::ymd("2024-04-02", tz = "CET") |>
      lubridate::with_tz(tzone = "UTC")
    testthat::expect_no_error(
      object = dt_seq_helper(
        from = start_sample_6,
        to = end_sample_6,
        pos = 1L:2L,
        qty = 104L:105L,
        seq_resolution = "P1D"
      )
    )
    start_sample_7 <- lubridate::ymd("2024-03-20", tz = "NZ") |>
      lubridate::with_tz(tzone = "UTC")
    end_sample_7 <- lubridate::ymd("2024-04-10", tz = "NZ") |>
      lubridate::with_tz(tzone = "UTC")
    testthat::expect_error(
      object = dt_seq_helper(
        from = start_sample_7,
        to = end_sample_7,
        pos = 1L:3L,
        qty = 101L:103L,
        seq_resolution = "P7D"
      ),
      info = paste(
        "The from date should denote the midnight hour either",
        "in 'UTC', 'WET', 'CET', 'EET' or Europe/Moscow timezone!"
      )
    )
    testthat::expect_error(
      object = dt_seq_helper(),
      info = "The argument 'qty' is missing!"
    )
    testthat::expect_error(
      object = dt_seq_helper(qty = 101L:110L),
      info = "The argument 'from' is missing!"
    )
    testthat::expect_error(
      object = dt_seq_helper(
        from = start_sample_1,
        qty = 101L:110L
      ),
      info = "The argument 'to' is missing!"
    )
    testthat::expect_equal(
      object = dt_seq_helper(
        from = start_sample_1,
        to = end_sample_1,
        pos = 1L:10L,
        qty = 101L:110L
      ) |>
        dim(),
      expected = c(10L, 2L)
    )
    testthat::expect_equal(
      object = dt_seq_helper(
        from = start_sample_1,
        to = end_sample_1,
        pos = 1L:9L,
        qty = 101L:109L
      ) |>
        dim(),
      expected = c(9L, 2L)
    )
    testthat::expect_equal(
      object = dt_seq_helper(
        from = start_sample_1,
        to = end_sample_1,
        pos = 1L:10L,
        qty = 101L:110L
      ) |> names(),
      expected = c("start_dt", "qty")
    )
  }
)



testthat::test_that(
  desc = "get_eiccodes() works",
  code = {
    testthat::expect_equal(
      object = get_eiccodes(
        f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/Y_eiccodes.csv"
      ) |> names(),
      expected = c("EicCode",
                   "EicDisplayName",
                   "EicLongName",
                   "EicParent",
                   "EicResponsibleParty",
                   "EicStatus",
                   "MarketParticipantPostalCode",
                   "MarketParticipantIsoCountryCode",
                   "MarketParticipantVatCode",
                   "EicTypeFunctionList",
                   "type")
    )
    testthat::expect_gt(
      object = get_eiccodes(
        f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/Y_eiccodes.csv"
      ) |> nrow(),
      expected = 300
    )
    testthat::expect_error(
      object = get_eiccodes(f = NULL),
      info = "The argument 'f' is missing!"
    )
    testthat::expect_error(
      object = get_eiccodes(f = "ABC"),
      info = "Cannot open the connection!"
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/X_eiccodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/Z_eiccodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/T_eiccodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/V_eiccodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/W_eiccodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/A_eiccodes.csv"
      )
    )
  }
)



testthat::test_that(
  desc = "unpack_xml() works",
  code = {
    testthat::expect_equal(
      object = xml2::xml2_example(path = "order-schema.xml") |>
        xml2::read_xml() |>
        unpack_xml(parent_name = "foo")
      |> dim(),
      expected = c(1L, 4L)
    )
    testthat::expect_equal(
      object = xml2::xml2_example(path = "order-schema.xml") |>
        xml2::read_xml() |>
        unpack_xml(parent_name = "foo") |>
        names(),
      expected = c("foo.schema.schema.annotation.documentation",
                   "foo.schema.schema.complexType.annotation.documentation",
                   "foo.schema.schema.complexType.annotation.appinfo",
                   "foo.schema.schema")
    )
    testthat::expect_error(
      object = xml2::xml2_example(path = "cd_catalog.xml") |>
        xml2::read_xml() |>
        unpack_xml(parent_name = "foo"),
      info = "Names must be unique!"
    )
  }
)



testthat::test_that(
  desc = "my_snakecase() works",
  code = {
    data("iris")
    testthat::expect_equal(
      object = my_snakecase(tbl = iris),
      expected = c("sepal_length", "sepal_width", "petal_length", "petal_width",
                   "species")
    )
    testthat::expect_equal(
      object = my_snakecase(tbl = transmission_pair_eic_dict),
      expected = c("out_area_code", "out_area_type_code", "out_area_name",
                   "out_map_code", "in_area_code", "in_area_type_code",
                   "in_area_name", "in_map_code")
    )
    df <- data.frame(
      process.mRID = 1L:3L,
      TimeSeriesType = LETTERS[1L:3L],
      unavailability_Time_Period = 1L:3L,
      A.ts.production_RegisteredResource.pSRType = LETTERS[1L:3L],
      B.ts.Production_RegisteredResource = LETTERS[1L:3L],
      C.ts.asset_RegisteredResource.pSRType = LETTERS[1L:3L],
      D.ts.Asset_RegisteredResource = LETTERS[1L:3L],
      E.powerSystemResources_type_psr_type = LETTERS[1L:3L],
      F.PowerSystemResources_type_psr_type = LETTERS[1L:3L],
      G.asset_psr_type = LETTERS[1L:3L],
      receiver_MarketParticipant.marketRole = LETTERS[1L:3L]
    )
    testthat::expect_equal(
      object = my_snakecase(tbl = df),
      expected = c("mrid", "ts_type",
                   "unavailability", "a_ts_production",
                   "b_ts_production", "c_ts_asset", "d_ts_asset",
                   "e_psr_type", "f_psr_type", "g_psr_type",
                   "receiver_market_participant_market_role")
    )
    testthat::expect_error(
      object = my_snakecase(tbl = NULL),
      info = "The argument 'tbl' is missing!"
    )
  }
)



testthat::test_that(
  desc = "xml_grand_children_lengths() works",
  code = {
    testthat::expect_equal(
      object = xml2::xml2_example(path = "order-schema.xml") |>
        xml2::read_xml() |>
        xml_grand_children_lengths(),
      expected = c(1L, 0L, 0L, 2L, 3L, 1L, 1L)
    )
    testthat::expect_error(
      object = xml_grand_children_lengths(xml_content = NULL),
      info = "The argument 'xml_content' is missing!"
    )
  }
)



testthat::test_that(
  desc = "add_definitions() works",
  code = {
    df <- data.frame(
      process_type = c("A12", "A27", "A61"),
      ts_in_domain_mrid = c(
        "16YAOGUADIANA--T",
        "44Y-00000000007S",
        "44Y-00000000012Z"
      ),
      ts_out_domain_mrid = c(
        "16YAOGUADIANA--T",
        "44Y-00000000007S",
        "44Y-00000000012Z"
      ),
      ts_in_bidding_zone_domain_mrid = c(
        "16YAOGUADIANA--T",
        "44Y-00000000007S",
        "44Y-00000000012Z"
      ),
      ts_out_bidding_zone_domain_mrid = c(
        "16YAOGUADIANA--T",
        "44Y-00000000007S",
        "44Y-00000000012Z"
      ),
      control_area_domain_mrid = c(
        "16YAOGUADIANA--T",
        "44Y-00000000007S",
        "44Y-00000000012Z"
      ),
      ts_auction_type = c(
        "A01",
        "A02",
        "A03"
      ),
      ts_auction_category = c(
        "A01",
        "A02",
        "A03"
      ),
      ts_contract_market_agreement_type = c(
        "A01",
        "A02",
        "A03"
      ),
      ts_asset_psr_type = c(
        "B01",
        "B02",
        "B03"
      )
    )
    testthat::expect_equal(
      object = add_definitions(tbl = df) |>
        dim(),
      expected = c(3L, 20L)
    )
    data(iris)
    testthat::expect_warning(
      object = add_definitions(tbl = iris),
      info = "No additional definitions added!"
    )
    testthat::expect_error(
      object = add_definitions(tbl = NULL),
      info = "The argument 'tbl' is missing!"
    )
  }
)



# xml_to_table
testthat::test_that(
  desc = "xml_to_table() works",
  code = {
    url_sample_1 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A73",
      "&processType=A16",
      "&periodStart=202001302300",
      "&periodEnd=202001312300",
      "&in_Domain=10YDE-VE-------2",
      "&securityToken="
    )
    url_sample_2 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A44",
      "&in_Domain=10YDK-1--------W",
      "&out_Domain=10YDK-1--------W",
      "&periodStart=201910312300",
      "&periodEnd=201911302300",
      "&securityToken="
    )
    content_1 <- api_req_safe(paste0(url_sample_1, Sys.getenv("ENTSOE_PAT")))
    content_2 <- api_req_safe(paste0(url_sample_2, Sys.getenv("ENTSOE_PAT")))
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
    testthat::expect_true(
      object = xml_to_table(xml_content = content_1$result) |>
        inherits(what = "data.frame"),
      info = "The result should be a data.frame!"
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
    testthat::expect_error(
      object = xml2::xml2_example(path = "order-schema.xml") |>
        xml2::read_xml() |>
        xml_to_table(),
      info = "There is no interesting columns in the result table!"
    ) |>
      testthat::expect_warning()
    data(iris)
    testthat::expect_error(
      object = xml_to_table(xml_content = iris),
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
  desc = "extract_response() works",
  code = {
    url_sample_2 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A73",
      "&processType=A16",
      "&periodStart=202001302300",
      "&periodEnd=202001312300",
      "&in_Domain=10YDE-VE-------2",
      "&securityToken="
    )
    content_2 <- api_req_safe(url = paste0(url_sample_2,
                                           Sys.getenv("ENTSOE_PAT")))
    testthat::expect_no_error(
      object = extract_response(
        content = content_2,
        tidy_output = TRUE
      )
    )
    url_sample_3 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A80",
      "&biddingZone_Domain=10YFR-RTE------C",
      "&periodStart=202407192200",
      "&periodEnd=202407232200",
      "&securityToken="
    )
    content_3 <- api_req_safe(url = paste0(url_sample_3,
                                           Sys.getenv("ENTSOE_PAT")))
    testthat::expect_no_error(
      object = extract_response(
        content = content_3,
        tidy_output = FALSE
      )
    )
    url_sample_4 <- paste0(
      "https://web-api.tp.entsoe.eu/api",
      "?documentType=A73",
      "&processType=A16",
      "&periodStart=202001302300",
      "&periodEnd=202001312300",
      "&in_Domain=10YDE-VE-------2",
      "&securityToken="
    )
    content_4 <- api_req_safe(url = paste0(url_sample_4,
                                           Sys.getenv("ENTSOE_PAT")))
    testthat::expect_true(
      object = extract_response(
        content = content_4,
        tidy_output = TRUE
      ) |>
        inherits(what = "data.frame")
    )
    content_1 <- setNames(
      object = list(
        xml2::xml2_example(path = "cd_catalog.xml") |>
          xml2::read_xml(),
        NULL
      ),
      c("result", "error")
    )
    testthat::expect_error(
      object = extract_response(content = content_1),
      info = "There is no interesting columns in the result table!"
    ) |>
      testthat::expect_warning()
    data(iris)
    testthat::expect_error(
      object = extract_response(content = iris),
      info = "The content is not in the required list format!"
    )
    testthat::expect_error(
      object = extract_response(content = NULL),
      info = "The argument 'tbl' is missing!"
    )
  }
)
