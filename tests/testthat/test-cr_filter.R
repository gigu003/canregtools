test_that("cr_filter", {
  res <- cr_filter(canregs, substr(icd10,1,3) == "C73")
  res2 <- cr_filter(canregs[[1]], year == 2021)

  expect_s3_class(res, "canregs")
  expect_s3_class(res2, "canreg")
})

test_that("cr_filter supports fbswicd objects", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws[[1]], year == year_value)

  expect_s3_class(res, "fbswicd")
  expect_true(all(res$fbswicd$year == year_value))
  expect_true(all(res$sitemorp$year == year_value))
  expect_true(all(res$pop$year == year_value))
})

test_that("cr_filter skips fbswicd components without requested variables", {
  res <- cr_filter(fbsws[[1]], cancer == "60")

  expect_s3_class(res, "fbswicd")
  expect_true(all(res$fbswicd$cancer == "60"))
  expect_true(all(res$sitemorp$cancer == "60"))
  expect_identical(res$pop, fbsws[[1]]$pop)
})

test_that("cr_filter supports fbswicds objects", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws, year == year_value)

  expect_s3_class(res, "fbswicds")
  expect_true(length(res) > 0)
  expect_true(all(purrr::map_lgl(res, inherits, "fbswicd")))
})

test_that("cr_filter part controls fbswicd components", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws[[1]], year == year_value, part = "fbswicd")

  expect_true(all(res$fbswicd$year == year_value))
  expect_identical(res$sitemorp, fbsws[[1]]$sitemorp)
  expect_identical(res$pop, fbsws[[1]]$pop)
})

test_that("cr_filter validates fbswicd part names", {
  expect_error(
    cr_filter(fbsws[[1]], year == 2021, part = "FBcases"),
    "Unsupported part name"
  )
})

test_that("cr_filter drop works for fbswicd cancer components", {
  res <- cr_filter(fbsws[[1]], drop = "total")

  expect_false(any(res$fbswicd$cancer %in% c("60", "61")))
  expect_false(any(res$sitemorp$cancer %in% c("60", "61")))
})
