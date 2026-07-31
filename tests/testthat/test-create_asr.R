test_that("create_asr works correctly for canreg and canregs", {
  # Test with canreg object
  data <- canregs[[1]]
  expect_s3_class(data, "canreg")
  expect_s3_class(canregs, "canregs")
  # Test with show_pop, show_var, show_ci
  asr1 <- create_asr(fbsws, year, sex, cancer)
  expect_s3_class(asr1, "asr")
  expect_contains(
    names(asr1),
    c(
      "areacode",
      "year",
      "sex",
      "cancer",
      "no_cases",
      "cr",
      "asr_cn2000",
      "asr_wld85",
      "rank"
    )
  )
  expect_equal(grep("pop", names(asr1), value = TRUE), character())
  expect_equal(grep("_var", names(asr1), value = TRUE), character())
  expect_equal(grep("_lower", names(asr1), value = TRUE), character())
  expect_equal(grep("_upper", names(asr1), value = TRUE), character())
  expect_gt(nrow(asr1), 0)
  expect_setequal(asr1$sex, c(0L, 1L, 2L))
})

test_that("create_asr can include population, variance, and confidence intervals", {
  asr <- create_asr(
    fbsws[[1]],
    year,
    sex,
    cancer,
    show_pop = TRUE,
    show_var = TRUE,
    show_ci = TRUE
  )

  expect_s3_class(asr, "asr")
  expect_contains(
    names(asr),
    c(
      "pop",
      "cr_var",
      "cr_lower",
      "cr_upper",
      "asr_var_cn2000",
      "asr_lower_cn2000",
      "asr_upper_cn2000"
    )
  )
  expect_gt(sum(asr$pop), 0)
  expect_equal(all(asr$cr_lower <= asr$cr_upper), TRUE)
})
