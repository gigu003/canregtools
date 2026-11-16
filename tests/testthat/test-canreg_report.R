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

  event_components <- c("summary", "registry", "age_summary", "age_registry")
  expect_setequal(
    intersect(event_components, names(report$inci)),
    event_components
  )
  expect_setequal(
    intersect(event_components, names(report$mort)),
    event_components
  )

  required_cols <- c("regicode_cn", "regicode", "year", "sex", "cancer")
  expect_setequal(
    intersect(required_cols, names(report$inci$summary)),
    required_cols
  )
  expect_setequal(
    intersect(required_cols, names(report$inci$registry)),
    required_cols
  )

  summary_labels <- c("河南", "城市", "农村")
  expect_setequal(
    intersect(
      summary_labels,
      unique(as.character(report$inci$summary$regicode_cn))
    ),
    summary_labels
  )
})

test_that("validate_canreg_report catches invalid objects", {
  expect_snapshot(error = TRUE, {
    validate_canreg_report(list())
  })
})
