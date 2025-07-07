test_that("create_age_rate.fbswicd works with default parameters", {
  fbsw <- count_canreg(canregs[[1]])
  ar <- create_age_rate(fbsw, year, sex)
  expect_s3_class(ar, "age_rate")
  expect_true(all(c("year", "agegrp", "cases", "rate") %in% names(ar)))
  expect_false(any(is.na(ar$rate)))
})

test_that("create_age_rate.fbswicd works with format = 'wide'", {
  fbsw <- count_canreg(canregs[[1]])
  ar <- create_age_rate(fbsw, year, sex, format = "wide")
  expect_s3_class(ar, "age_rate")
  expect_true(any(grepl("^f[0-9]+", names(ar))))
  expect_true(any(grepl("^r[0-9]+", names(ar))))
})

test_that("create_age_rate.fbswicd hides population if show_pop = FALSE", {
  fbsw <- count_canreg(canregs[[1]])
  ar <- create_age_rate(fbsw, year, show_pop = FALSE)
  expect_false(any(grepl("^rks", names(ar))))
})

test_that("create_age_rate.fbswicds works and collapses when collapse = TRUE", {
  fbsws <- count_canreg(canregs)
  ar <- create_age_rate(fbsws, year, sex, collapse = TRUE)
  expect_s3_class(ar, "age_rate")
  expect_false(inherits(ar, "list"))
})

test_that("create_age_rate.fbswicds works without collapsing", {
  fbsws <- count_canreg(canregs)
  ar <- create_age_rate(fbsws, year, sex, collapse = FALSE)
  expect_s3_class(ar, "age_rates")
  expect_type(ar, "list")
  expect_true(all(purrr::map_lgl(ar, ~ inherits(., "age_rate"))))
})

test_that("create_age_rate.canreg dispatches to fbswicd method", {
  ar <- create_age_rate(canregs[[1]], year, sex)
  expect_s3_class(ar, "age_rate")
})

test_that("create_age_rate.canregs dispatches and collapses correctly", {
  ar <- create_age_rate(canregs, year, sex, collapse = TRUE)
  expect_s3_class(ar, "age_rate")
})

test_that("create_age_rate.canregs dispatches and keeps list structure if collapse = FALSE", {
  ar <- create_age_rate(canregs, year, sex, collapse = FALSE)
  expect_s3_class(ar, "age_rates")
  expect_type(ar, "list")
})
