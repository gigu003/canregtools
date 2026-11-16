test_that("create_quality.canreg works with default parameters", {
  qua <- create_quality(fbsws, year, sex)

  expect_s3_class(qua, "quality")
  expect_contains(
    names(qua),
    c(
      "regicode",
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

test_that("create_quality preserves undefined values for zero denominators", {
  fbsw <- fbsws[[1]]
  fbsw$pop$rks <- 0L

  qua <- create_quality(fbsw, year, sex)

  expect_true(all(is.na(qua$inci)))
  expect_true(all(is.na(qua$mort)))
})

test_that("intermediate fbswicd ordering can be skipped safely", {
  fbsw <- fbsws[[1]]
  arranged <- pre_deal_fbswicd(fbsw, year, sex, cancer, .arrange = TRUE)
  unsorted <- pre_deal_fbswicd(fbsw, year, sex, cancer, .arrange = FALSE)

  expect_equal(
    dplyr::arrange(arranged, year, sex, cancer, agegrp),
    dplyr::arrange(unsorted, year, sex, cancer, agegrp)
  )
})

test_that("quality precision is consistent for single and multiple registries", {
  expected <- create_quality(fbsws[[1]], year, sex, decimal = 6)
  listed <- create_quality(fbsws, year, sex, decimal = 6, collapse = FALSE)
  collapsed <- create_quality(fbsws, year, sex, decimal = 6)
  raw <- create_quality(canregs, year, sex, decimal = 6, collapse = FALSE)
  expect_equal(listed[[1]], expected)
  expect_equal(raw[[1]], expected)
  expect_equal(collapsed, cr_merge(listed))
})
