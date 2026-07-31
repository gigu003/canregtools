test_that("create_quality.canreg works with default parameters", {
  qua <- create_quality(fbsws, year, sex)

  expect_s3_class(qua, "quality")
  expect_contains(
    names(qua),
    c(
      "areacode",
      "year",
      "sex",
      "cancer",
      "rks",
      "fbs",
      "inci",
      "sws",
      "mort",
      "mi",
      "mv",
      "dco",
      "ub",
      "sub"
    )
  )
  expect_equal(nrow(qua), length(fbsws) * 3)
  expect_setequal(qua$sex, c(0L, 1L, 2L))
  expect_equal(unique(qua$cancer), "60")
  expect_gt(sum(qua$rks), 0)
  expect_equal(all(qua$mi >= 0), TRUE)
})

test_that("create_quality.fbswicds can return a list when collapse is FALSE", {
  qua <- create_quality(fbsws, year, sex, collapse = FALSE)

  expect_s3_class(qua, "qualities")
  expect_length(qua, length(fbsws))
  expect_equal(
    unname(purrr::map_lgl(qua, inherits, "quality")),
    rep(TRUE, length(qua))
  )
})
