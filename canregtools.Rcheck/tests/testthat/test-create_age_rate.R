
test_that("create_age_rate.fbswicds works and collapses when collapse = TRUE", {

  ar <- create_age_rate(fbsws, year, sex, collapse = FALSE)
  expect_s3_class(ar, "age_rates")
  expect_true(inherits(ar, "list"))
  expect_true(all(purrr::map_lgl(ar, ~ inherits(., "age_rate"))))
})

test_that("create_age_rate.canreg dispatches to fbswicd method", {
  ar <- create_age_rate(canregs[[1]], year, sex)
  expect_s3_class(ar, "age_rate")
})

test_that("create_age_rate.canregs dispatches and collapses correctly", {
  ar <- create_age_rate(fbsws, year, sex, collapse = TRUE, format = "wide")
  expect_s3_class(ar, "age_rate")
})
