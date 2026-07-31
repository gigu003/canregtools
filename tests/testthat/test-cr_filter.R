test_that("cr_filter", {
  res <- cr_filter(canregs, substr(icd10, 1, 3) == "C73")
  res2 <- cr_filter(canregs[[1]], year == 2021)

  expect_s3_class(res, "canregs")
  expect_s3_class(res2, "canreg")
  expect_equal(
    unname(purrr::map_lgl(res, ~ all(substr(.x$FBcases$icd10, 1, 3) == "C73"))),
    rep(TRUE, length(res))
  )
})

test_that("cr_filter supports fbswicd objects", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws[[1]], year == year_value)

  expect_s3_class(res, "fbswicd")
  expect_equal(unique(res$fbswicd$year), year_value)
  expect_equal(unique(res$sitemorp$year), year_value)
  expect_equal(unique(res$pop$year), year_value)
})

test_that("cr_filter skips fbswicd components without requested variables", {
  res <- cr_filter(fbsws[[1]], cancer == "60")

  expect_s3_class(res, "fbswicd")
  expect_equal(unique(res$fbswicd$cancer), "60")
  expect_setequal(unique(res$sitemorp$cancer), character())
  expect_identical(res$pop, fbsws[[1]]$pop)
})

test_that("cr_filter supports fbswicds objects", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws, year == year_value)

  expect_s3_class(res, "fbswicds")
  expect_gt(length(res), 0)
  expect_equal(
    unname(purrr::map_lgl(res, inherits, "fbswicd")),
    rep(TRUE, length(res))
  )
})

test_that("cr_filter part controls fbswicd components", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws[[1]], year == year_value, part = "fbswicd")

  expect_equal(unique(res$fbswicd$year), year_value)
  expect_identical(res$sitemorp, fbsws[[1]]$sitemorp)
  expect_identical(res$pop, fbsws[[1]]$pop)
})

test_that("cr_filter validates fbswicd part names", {
  expect_snapshot(error = TRUE, {
    cr_filter(fbsws[[1]], year == 2021, part = "FBcases")
  })
})

test_that("cr_filter drop works for fbswicd cancer components", {
  res <- cr_filter(fbsws[[1]], drop = "total")

  expect_equal(intersect(res$fbswicd$cancer, c("60", "61")), character())
  expect_equal(intersect(res$sitemorp$cancer, c("60", "61")), character())
})
