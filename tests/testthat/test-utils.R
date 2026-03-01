testthat::test_that(
  desc = "grouping_by_common_strings() works",
  code = {
    input1 <- list(
      c("apple", "banana"),
      c("cherry", "date")
    )
    input2 <- list(
      c("apple", "banana"),
      c("banana", "cherry")
    )
    input3 <- list(
      c("apple", "banana"),
      c("cherry", "date"),
      c("banana", "elderberry"),
      c("date", "fig"),
      c("elderberry", "grape")
    )
    input4 <- list(
      c("A", "B"),
      c("B", "C"),
      c("C", "D"),
      c("D", "E")
    )
    input5 <- list(
      c("A", "B"),
      c("B", "C"),
      c("X", "Y"),
      c("C", "D")
    )
    input6 <- list(
      c("apple", "apple", "banana"),
      c("banana", "cherry", "cherry")
    )
    input7 <- list(
      c("apple"),
      character(0),
      c("banana")
    )
    input8 <- list(
      c("A"),
      c("B"),
      c("A"),
      c("C")
    )
    input9 <- list(
      c("A", "B"),
      c("C", "D"),
      c("B", "E"),
      c("F"),
      c("D", "G"),
      c("H", "I"),
      c("E", "F")
    )
    input10 <- lapply(
      1L:100L,
      \(i) paste0("unique_", i)
    )
    input11 <- list(
      c("Apple", "banana"),
      c("apple", "cherry")
    )
    input12 <- list(
      c("1", "2"),
      c("2", "3"),
      c("4", "5")
    )
    input13 <- list(
      c("hello@world.com", "test#123"),
      c("test#123", "foo$bar"),
      c("unique*string")
    )
    input14 <- list(
      c("café", "naïve"),
      c("naïve", "résumé"),
      c("日本語", "中文")
    )
    long_string <- rep("a", 1000L) |>
      paste(collapse = "")
    input15 <- list(
      c(long_string, "b"),
      c(long_string, "c"),
      c("d", "e")
    )
    input16 <- list(
      c("a", "b"),
      c("b", "c")
    )
    input17 <- list(
      c("A", "B"),
      c("C", "D"),
      c("B", "E"),
      c("F")
    )
    result1 <- grouping_by_common_strings(vector_list = input1)
    result2 <- grouping_by_common_strings(vector_list = input2)
    result3 <- grouping_by_common_strings(vector_list = input3)
    result4 <- grouping_by_common_strings(vector_list = input4)
    result5 <- grouping_by_common_strings(vector_list = input5)
    result6 <- grouping_by_common_strings(vector_list = input6)
    result7 <- grouping_by_common_strings(vector_list = input7)
    result8 <- grouping_by_common_strings(vector_list = input8)
    result9 <- grouping_by_common_strings(vector_list = input9)
    result12 <- grouping_by_common_strings(vector_list = input12)
    result13 <- grouping_by_common_strings(vector_list = input13)
    result14 <- grouping_by_common_strings(vector_list = input14)
    result15 <- grouping_by_common_strings(vector_list = input15)
    result16 <- grouping_by_common_strings(vector_list = input16)
    result17 <- grouping_by_common_strings(vector_list = input17) |>
      unlist()
    testthat::expect_equal(
      object = grouping_by_common_strings(vector_list = list()),
      expected = list()
    )
    testthat::expect_equal(
      object = grouping_by_common_strings(vector_list = list(c("a", "b"))),
      expected = list(1L)
    )
    testthat::expect_length(
      object = result1,
      n = 2L
    )
    testthat::expect_true(
      object = 1L %in% result1[[1L]] || 1L %in% result1[[2L]]
    )
    testthat::expect_true(
      object = 2L %in% result1[[1L]] || 2L %in% result1[[2L]]
    )
    testthat::expect_false(
      object = all(c(1L, 2L) %in% result1[[1L]])
    )
    testthat::expect_length(
      object = result2,
      n = 1L
    )
    testthat::expect_setequal(
      object = result2[[1]],
      expected = c(1L, 2L)
    )
    testthat::expect_length(
      object = result3,
      n = 2L
    )
    testthat::expect_setequal(
      object = result3[[which(sapply(result3, \(g) 1L %in% g))]],
      expected = c(1L, 3L, 5L)
    )
    testthat::expect_setequal(
      object = result3[[which(sapply(result3, \(g) 2L %in% g))]],
      expected = c(2L, 4L)
    )
    testthat::expect_length(
      object = result4,
      n = 1L
    )
    testthat::expect_setequal(
      object = result4[[1]],
      expected = 1L:4L
    )
    testthat::expect_length(
      object = result5,
      n = 2L
    )
    testthat::expect_equal(
      object = result5[[which(sapply(result5, \(g) 3L %in% g))]],
      expected = 3L
    )
    testthat::expect_setequal(
      object = result5[[which(sapply(result5, \(g) 1L %in% g))]],
      expected = c(1L, 2L, 4L)
    )
    testthat::expect_length(
      object = result6,
      n = 1L
    )
    testthat::expect_setequal(
      object = result6[[1L]],
      expected = c(1L, 2L)
    )
    testthat::expect_length(
      object = result7,
      n = 3L
    )
    testthat::expect_length(
      object = result8,
      n = 3L
    )
    testthat::expect_setequal(
      object = result8[[which(sapply(result8, \(g) 1L %in% g))]],
      expected = c(1L, 3L)
    )
    testthat::expect_length(
      object = result9,
      n = 3L
    )
    testthat::expect_setequal(
      object = result9[[which(sapply(result9, \(g) 1L %in% g))]],
      expected = c(1L, 3L, 4L, 7L)
    )
    testthat::expect_setequal(
      object = result9[[which(sapply(result9, \(g) 2L %in% g))]],
      expected = c(2L, 5L)
    )
    testthat::expect_equal(
      object = result9[[which(sapply(result9, \(g) 6L %in% g))]],
      expected = 6L
    )
    testthat::expect_length(
      object = grouping_by_common_strings(vector_list = input10),
      n = 100L
    )
    testthat::expect_length(
      object = grouping_by_common_strings(vector_list = input11),
      n = 2L
    )
    testthat::expect_length(
      object = result12,
      n = 2L
    )
    testthat::expect_setequal(
      object = result12[[which(sapply(result12, \(g) 1L %in% g))]],
      expected = c(1L, 2L)
    )
    testthat::expect_length(
      object = result13,
      n = 2L
    )
    testthat::expect_setequal(
      object = result13[[which(sapply(result13, \(g) 1L %in% g))]],
      expected = c(1L, 2L)
    )
    testthat::expect_length(
      object = result14,
      n = 2L
    )
    testthat::expect_setequal(
      object = result14[[which(sapply(result14, \(g) 1L %in% g))]],
      expected = c(1L, 2L)
    )
    testthat::expect_length(
      object = result15,
      n = 2L
    )
    testthat::expect_setequal(
      object = result15[[which(sapply(result15, \(g) 1L %in% g))]],
      expected = c(1L, 2L)
    )
    testthat::expect_true(
      object = is.integer(result16[[1L]])
    )
    testthat::expect_setequal(
      object = result17,
      expected = 1L:4L
    )
    testthat::expect_equal(
      object = length(result17),
      expected = 4L
    )
  }
)



testthat::test_that(
  desc = "extract_leaf_twig_branch() works",
  code = {
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
    data(iris)
    content_1 <- setNames(
      object = list(
        xml2::xml2_example(path = "cd_catalog.xml") |>
          xml2::read_xml(),
        NULL
      ),
      c("result", "error")
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
    testthat::expect_s3_class(
      object = xml2::xml_contents(content_1$result) |>
        extract_leaf_twig_branch(),
      class = "tbl"
    )
    testthat::expect_contains(
      object = xml2::xml_contents(content_2$result) |>
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
      object = xml2::xml_contents(content_3$result[[1]]) |>
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
      object = xml2::xml_contents(content_4$result) |>
        extract_leaf_twig_branch() |>
        dim(),
      expected = c(546, 26)
    )
    testthat::expect_error(
      object = extract_leaf_twig_branch(nodesets = iris),
      info = paste(
        "no applicable method for 'nodeset_apply'",
        " applied to an object of class 'c('double', 'numeric')'"
      )
    )
  }
)



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
    url_sample_3 <- paste(
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
        query_string = url_sample_3
      ) |>
        length(),
      expected = 6L
    ) |>
      testthat::expect_message()
    testthat::expect_error(
      object = calc_offset_urls(reason = reason_2, query_string = url_sample_3),
      info = "The 'from' argument must be a finite number!"
    )
    testthat::expect_error(
      object = calc_offset_urls(reason = reason_3, query_string = url_sample_3),
      info = "The 'from' argument must be a finite number!"
    )
  }
)



testthat::test_that(
  desc = "api_req() works",
  code = {
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
      regexp = "is larger than maximum allowed period"
    )
    testthat::expect_error(
      object = api_req(
        query_string = "https://web-api.tp.entsoe.eu/api",
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      regexp = "Unable to parse URI. Its format is not valid"
    )
    testthat::expect_error(
      object = api_req(
        query_string = NULL,
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      regexp = "The argument 'query_string' is missing!"
    )
    testthat::expect_error(
      object = api_req(),
      regexp = "The argument 'query_string' is missing!"
    )
    testthat::expect_error(
      object = api_req(
        query_string = NA,
        security_token = Sys.getenv("ENTSOE_PAT")
      ),
      regexp = "Input parameter does not exist: NA"
    )
    testthat::expect_error(
      object = api_req(
        query_string = "https://google.com/",
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
      regexp = "The argument 'security_token' is not provided"
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
      info = "The 'x' value has been interpreted as UTC!"
    )
  }
)



testthat::test_that(
  desc = "get_eiccodes() works",
  code = {
    testthat::expect_contains(
      object = get_eiccodes(
        f = "Y_eicCodes.csv"
      ) |> names(),
      expected = c(
        "EicCode",
        "EicDisplayName",
        "EicLongName",
        "EicParent",
        "EicResponsibleParty",
        "EicStatus",
        "MarketParticipantPostalCode",
        "MarketParticipantIsoCountryCode",
        "MarketParticipantVatCode",
        "EicTypeFunctionList",
        "type"
      )
    )
    testthat::expect_gt(
      object = get_eiccodes(
        f = "Y_eicCodes.csv"
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
        f = "X_eicCodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "Z_eicCodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "T_eicCodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "V_eicCodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "W_eicCodes.csv"
      )
    )
    testthat::expect_no_error(
      object = get_eiccodes(
        f = "A_eicCodes.csv"
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
    testthat::expect_contains(
      object = xml2::xml2_example(path = "order-schema.xml") |>
        xml2::read_xml() |>
        unpack_xml(parent_name = "foo") |>
        names(),
      expected = c(
        "foo.schema.schema.annotation.documentation",
        "foo.schema.schema.complexType.annotation.documentation",
        "foo.schema.schema.complexType.annotation.appinfo",
        "foo.schema.schema"
      )
    )
    testthat::expect_error(
      object = xml2::xml2_example(path = "cd_catalog.xml") |>
        xml2::read_xml() |>
        unpack_xml(parent_name = "foo"),
      info = "Names must be unique!"
    )
    # NULL result_vector path (empty XML element)
    testthat::expect_equal(
      object = xml2::read_xml("<root><empty/></root>") |>
        xml2::xml_contents() |>
        (\(ns) ns[[1L]])() |>
        unpack_xml(parent_name = "foo"),
      expected = tibble::tibble()
    )
  }
)



testthat::test_that(
  desc = "tidy_or_not() works",
  code = {
    test_df_1 <- xml2::xml2_example(path = "order-schema.xml") |>
      xml2::read_xml() |>
      unpack_xml(parent_name = "foo")
    test_df_2 <- tibble::tibble(
      ts_resolution = rep(x = "PT15M", 12L),
      ts_reason_code = rep(x = "B01", 12L),
      ts_time_interval_start = as.POSIXct(
        x = "2023-10-01 23:00:00",
        tz = "UTC"
      ),
      ts_point_position = 1:12,
      ts_point_price = rep(c(10, 20, 30), 4L)
    )
    testthat::expect_equal(
      object = tidy_or_not(tbl = test_df_1, tidy_output = TRUE) |>
        dim(),
      expected = c(1L, 4L)
    )
    testthat::expect_equal(
      object = tidy_or_not(tbl = test_df_1, tidy_output = FALSE) |>
        dim(),
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
    # bid_ts_ columns with ts_point_ cols: covers rename-to-ts_ and rename-back
    test_df_3 <- tibble::tibble(
      bid_ts_mrid = rep(x = "TS001", 4L),
      bid_ts_resolution = rep(x = "PT15M", 4L),
      bid_ts_time_interval_start = as.POSIXct(
        x = "2023-10-01 23:00:00",
        tz = "UTC"
      ),
      bid_ts_point_position = 1L:4L,
      bid_ts_point_price = c(10, 20, 30, 40)
    )
    result_3_tidy <- tidy_or_not(tbl = test_df_3, tidy_output = TRUE)
    testthat::expect_contains(
      object = names(result_3_tidy),
      expected = c("bid_ts_point_price", "bid_ts_point_dt_start")
    )
    testthat::expect_false(object = "ts_point_price" %in% names(result_3_tidy))
    # bid_ts_ columns without ts_point_ cols:
    # covers rename-back-early-return path
    test_df_4 <- tibble::tibble(
      bid_ts_resolution = "PT15M",
      bid_ts_mrid = "TS001"
    )
    result_4 <- tidy_or_not(tbl = test_df_4, tidy_output = FALSE)
    testthat::expect_contains(
      object = names(result_4),
      expected = c("bid_ts_resolution", "bid_ts_mrid")
    )
    # A03 curve type with missing positions: covers the fill-missing-values path
    test_df_5 <- tibble::tibble(
      ts_mrid = rep(x = "TS001", 3L),
      ts_curve_type = rep(x = "A03", 3L),
      ts_resolution = rep(x = "PT60M", 3L),
      ts_time_interval_start = rep(
        x = as.POSIXct(x = "2023-10-01 22:00:00", tz = "UTC"), 3L
      ),
      ts_time_interval_end = rep(
        x = as.POSIXct(x = "2023-10-02 04:00:00", tz = "UTC"), 3L
      ),
      ts_point_position = c(1L, 2L, 4L),
      ts_point_quantity = c(100.0, 200.0, 400.0)
    )
    result_5 <- tidy_or_not(tbl = test_df_5, tidy_output = TRUE)
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
    data("iris")
    testthat::expect_contains(
      object = my_snakecase(tbl = iris),
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
    testthat::expect_contains(
      object = my_snakecase(tbl = df),
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
      info = "The argument 'tbl' is missing!"
    )
  }
)



testthat::test_that(
  desc = "add_type_names() works",
  code = {
    df <- data.frame(
      process_type = c(
        "A12",
        "A27",
        "A61"
      ),
      ts_auction_type = c(
        "A01",
        "A02",
        "A03"
      ),
      ts_business_type = c(
        "A01",
        "A02",
        "A03"
      ),
      type = c(
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
      ),
      ts_mkt_psr_type = c(
        "B01",
        "B02",
        "B03"
      ),
      ts_production_psr_type = c(
        "B01",
        "B02",
        "B03"
      )
    )
    data(iris)
    testthat::expect_true(
      object = identical(
        x = add_type_names(tbl = df) |>
          names(),
        y = c(
          "ts_auction_type",
          "ts_contract_market_agreement_type",
          "process_type",
          "ts_production_psr_type",
          "ts_asset_psr_type",
          "ts_mkt_psr_type",
          "ts_business_type",
          "type",
          "type_def",
          "ts_business_type_def",
          "ts_mkt_psr_type_def",
          "ts_asset_psr_type_def",
          "ts_production_psr_type_def",
          "process_type_def",
          "ts_contract_market_agreement_type_def",
          "ts_auction_type_def"
        )
      )
    )
    testthat::expect_no_warning(
      object = add_type_names(tbl = iris),
      message = "No additional definitions added!"
    )
    testthat::expect_no_warning(
      object = add_type_names(tbl = NULL),
      message = "No additional definitions added!"
    )
    # ts_product branch
    df_product <- data.frame(ts_product = c("A03", "A04", "A05"))
    testthat::expect_contains(
      object = add_type_names(tbl = df_product) |> names(),
      expected = "ts_product_def"
    )
    # subject_market_participant_market_role_type
    # and bid_ts_flow_direction branches
    df_role_dir <- data.frame(
      subject_market_participant_market_role_type = c("A01", "A07", "A32"),
      bid_ts_flow_direction = c("A01", "A02", "A01")
    )
    result_role_dir <- add_type_names(tbl = df_role_dir)
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
    df <- data.frame(
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
      ts_bidding_zone_domain_mrid = c(
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
      ts_registered_resource_mrid = c(
        "11WD4GKM-2CM-179",
        "11WD7VIAN2H-1-3K",
        "11WD8HOH22H---DA"
      )
    )
    data(iris)
    testthat::expect_contains(
      object = add_eic_names(tbl = df) |>
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
      object = add_eic_names(tbl = iris),
      class = "data.table"
    )
    testthat::expect_equal(
      object = add_eic_names(tbl = NULL),
      expected = data.table::data.table()
    )
  }
)



testthat::test_that(
  desc = "add_definitions() works",
  code = {
    df <- data.frame(
      doc_status_value = c(
        "A01",
        "A02",
        "A03"
      ),
      ts_auction_category = c(
        "A01",
        "A02",
        "A03"
      ),
      ts_flow_direction = c(
        "A01",
        "A02",
        "A03"
      ),
      reason_code = c(
        "B01",
        "B02",
        "B03"
      ),
      ts_reason_code = c(
        "A01",
        "A02",
        "A03"
      ),
      ts_object_aggregation = c(
        "A01",
        "A02",
        "A03"
      )
    )
    data(iris)
    testthat::expect_true(
      object = identical(
        x = add_definitions(tbl = df) |>
          names(),
        y = c(
          "ts_object_aggregation",
          "ts_reason_code",
          "reason_code",
          "ts_flow_direction",
          "ts_auction_category",
          "doc_status_value",
          "doc_status",
          "ts_auction_category_def",
          "ts_flow_direction_def",
          "reason_text",
          "ts_reason_text",
          "ts_object_aggregation_def"
        )
      )
    )
    testthat::expect_s3_class(
      object = add_definitions(tbl = iris),
      class = "data.table"
    )
    testthat::expect_equal(
      object = add_definitions(tbl = NULL),
      expected = data.table::data.table()
    )
  }
)



testthat::test_that(
  desc = "xml_to_table() works",
  code = {
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
    data(iris)
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
        xml_to_table(tidy_output = FALSE),
      info = "There is no interesting columns in the result table!"
    )
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
    data(iris)
    content_1 <- setNames(
      object = list(
        xml2::xml2_example(path = "cd_catalog.xml") |>
          xml2::read_xml(),
        NULL
      ),
      c("result", "error")
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
    testthat::expect_error(
      object = extract_response(content = content_1),
      info = "There is no interesting columns in the result table!"
    )
    testthat::expect_error(
      object = extract_response(content = iris),
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
    testthat::expect_contains(
      object = names(tbl),
      expected = c(
        "eic_code",
        "doc_status_value",
        "doc_status",
        "last_request_date",
        "instance_component_attribute",
        "long_name",
        "display_name",
        "function_names"
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
    df <- tibble::tibble(
      ts_resolution = "PT60M",
      ts_curve_type = "A99",
      ts_time_interval_start = as.POSIXct("2023-01-01 00:00:00", tz = "UTC"),
      ts_point_position = 1L,
      ts_point_price = 50.0
    )
    testthat::expect_error(
      object = tidy_or_not(tbl = df, tidy_output = TRUE),
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
  desc = "add_eic_names() adds names for additional domain mrid columns",
  code = {
    eic_val <- "16YAOGUADIANA--T"
    df <- data.frame(
      area_domain_mrid              = eic_val,
      ts_acquiring_domain_mrid      = eic_val,
      ts_connecting_domain_mrid     = eic_val,
      bid_ts_acquiring_domain_mrid  = eic_val,
      bid_ts_connecting_domain_mrid = eic_val,
      domain_mrid                   = eic_val,
      constraint_ts_monitored_ptdf_domain_mrid = eic_val,
      stringsAsFactors = FALSE
    )
    result <- add_eic_names(tbl = df)
    testthat::expect_contains(
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
    df <- data.frame(
      ts_reason_code   = c("A01", "A02"),
      ts_reason_code_1 = c("A03", "A04"),
      stringsAsFactors = FALSE
    )
    result <- add_definitions(tbl = df)
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
  desc = "xml_to_table() tryCatch handler fires on unequal-group XML",
  code = {
    conflict_xml <- xml2::read_xml(
      paste0(
        '<?xml version="1.0" encoding="UTF-8"?>',
        "<root><row>",
        "<groupA><subA><item>1</item></subA></groupA>",
        "<groupA><subA><item>2</item></subA></groupA>",
        "<groupA><subA><item>3</item></subA></groupA>",
        "<groupB><subB><item>x</item></subB></groupB>",
        "<groupB><subB><item>y</item></subB></groupB>",
        "</row></root>"
      )
    )
    testthat::expect_error(
      object = xml_to_table(xml_content = conflict_xml),
      regexp = "The XML document has an unexpected tree structure"
    )
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
    xml_body <- paste0(
      '<?xml version="1.0" encoding="UTF-8"?>',
      "<Acknowledgement_MarketDocument>",
      "<mRID>test</mRID>",
      "<Reason><message>Something went wrong</message></Reason>",
      "</Acknowledgement_MarketDocument>"
    ) |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 500L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = xml_body
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
    xml_body <- paste0(
      '<?xml version="1.0" encoding="UTF-8"?>',
      "<Acknowledgement_MarketDocument>",
      "<mRID>test</mRID>",
      "<Reason>",
      "<code>B11</code>",
      "<text>Requested data is not available</text>",
      "</Reason>",
      "</Acknowledgement_MarketDocument>"
    ) |>
      charToRaw()
    httr2::local_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 400L,
          url = req$url,
          headers = list("content-type" = "application/xml"),
          body = xml_body
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
