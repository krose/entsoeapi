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
