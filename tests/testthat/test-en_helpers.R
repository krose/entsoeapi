testthat::test_that(
  desc = "all_approved_eic() works",
  code = {
    testthat::expect_no_error(
      object = all_approved_eic()
    )
    testthat::expect_error(
      object = all_approved_eic("foo"),
      info = "Error in all_approved_eic('foo') : unused argument ('foo')"
    )
  }
)



testthat::test_that(
  desc = "responses got and appended into a tibble",
  code = {
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
  }
)



testthat::test_that(
  desc = "responses got and appended into a tibble",
  code = {
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
  }
)



testthat::test_that(
  desc = "party_eic() works",
  code = {
    testthat::expect_no_error(object = party_eic())
    testthat::expect_error(
      object = party_eic("foo"),
      info = "Error in party_eic('foo') : unused argument ('foo')"
    )
    tbl <- party_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "X"))
  }
)



testthat::test_that(
  desc = "area_eic() works",
  code = {
    testthat::expect_no_error(object = area_eic())
    testthat::expect_error(
      object = area_eic("foo"),
      info = "Error in area_eic('foo') : unused argument ('foo')"
    )
    tbl <- area_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "Y"))
  }
)



testthat::test_that(
  desc = "accounting_point_eic() works",
  code = {
    testthat::expect_no_error(object = accounting_point_eic())
    testthat::expect_error(
      object = accounting_point_eic("foo"),
      info = "Error in accounting_point_eic('foo') : unused argument ('foo')"
    )
    tbl <- accounting_point_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "Z"))
  }
)



testthat::test_that(
  desc = "tie_line_eic() works",
  code = {
    testthat::expect_no_error(object = tie_line_eic())
    testthat::expect_error(
      object = tie_line_eic("foo"),
      info = "Error in tie_line_eic('foo') : unused argument ('foo')"
    )
    tbl <- tie_line_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "T"))
  }
)



testthat::test_that(
  desc = "location_eic() works",
  code = {
    testthat::expect_no_error(object = location_eic())
    testthat::expect_error(
      object = location_eic("foo"),
      info = "Error in location_eic('foo') : unused argument ('foo')"
    )
    tbl <- location_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "V"))
  }
)



testthat::test_that(
  desc = "resource_object_eic() works",
  code = {
    testthat::expect_no_error(object = resource_object_eic())
    testthat::expect_error(
      object = resource_object_eic("foo"),
      info = "Error in resource_object_eic('foo') : unused argument ('foo')"
    )
    tbl <- resource_object_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "W"))
  }
)



testthat::test_that(
  desc = "substation_eic() works",
  code = {
    testthat::expect_no_error(object = substation_eic())
    testthat::expect_error(
      object = substation_eic("foo"),
      info = "Error in substation_eic('foo') : unused argument ('foo')"
    )
    tbl <- substation_eic()
    testthat::expect_s3_class(object = tbl, class = "tbl_df", exact = FALSE)
    testthat::expect_gt(object = nrow(tbl), expected = 0L)
    testthat::expect_contains(
      object = names(tbl),
      expected = c("EicCode", "EicLongName", "type")
    )
    testthat::expect_true(object = all(tbl$type == "A"))
  }
)
