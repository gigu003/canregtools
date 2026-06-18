test_that("create_canreg_report_data builds a report object", {
  report <- create_canreg_report_data(
    canregs,
    year = 2021,
    province = "河南省",
    keep_source = FALSE,
    include_age_registry = FALSE
  )

  expect_s3_class(report, "canreg_report")
  expect_named(report, c("meta", "source", "pop", "quality", "inci", "mort"))
  expect_equal(report$meta$year_data, 2021)
  expect_equal(names(report$meta$summary_codes), c("all", "urban", "rural"))

  expect_true(all(c("summary", "registry", "age_summary", "age_registry") %in% names(report$inci)))
  expect_true(all(c("summary", "registry", "age_summary", "age_registry") %in% names(report$mort)))
  expect_true(all(c("areacode_cn", "areacode", "year", "sex", "cancer") %in% names(report$inci$summary)))
  expect_true(all(c("areacode_cn", "areacode", "year", "sex", "cancer") %in% names(report$inci$registry)))

  expect_true(all(c("河南", "城市", "农村") %in% unique(as.character(report$inci$summary$areacode_cn))))
})

test_that("validate_canreg_report catches invalid objects", {
  expect_error(validate_canreg_report(list()), "canreg_report")
})