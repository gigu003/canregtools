test_that("create_age_rate.fbswicds works and collapses when collapse = TRUE", {
  ar <- create_age_rate(fbsws, year, sex, collapse = FALSE)

  expect_s3_class(ar, "age_rates")
  expect_type(ar, "list")
  expect_length(ar, length(fbsws))
  expect_equal(
    unname(purrr::map_lgl(ar, inherits, "age_rate")),
    rep(TRUE, length(ar))
  )
  expect_equal(
    unname(purrr::map_lgl(
      ar,
      ~ identical(
        names(.x),
        c("year", "sex", "cancer", "agegrp", "cases", "rate")
      )
    )),
    rep(TRUE, length(ar))
  )
})

test_that("create_age_rate.canreg dispatches to fbswicd method", {
  ar <- create_age_rate(canregs[[1]], year, sex)

  expect_s3_class(ar, "age_rate")
  expect_named(ar, c("year", "sex", "cancer", "agegrp", "cases", "rate"))
  expect_setequal(ar$sex, c(0L, 1L, 2L))
  expect_equal(unique(ar$cancer), "60")
  expect_gt(sum(ar$cases), 0)
})

test_that("create_age_rate.canregs dispatches and collapses correctly", {
  ar <- create_age_rate(fbsws, year, sex, collapse = TRUE, format = "wide")

  expect_s3_class(ar, "age_rate")
  expect_contains(names(ar), c("regicode", "year", "sex", "cancer", "f0", "r0"))
  expect_equal(nrow(ar), length(fbsws) * 3)
  expect_setequal(ar$sex, c(0L, 1L, 2L))
  expect_gt(sum(ar$f0), 0)
})
