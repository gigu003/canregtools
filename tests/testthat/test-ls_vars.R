test_that("ls_attrs lists built-in reframe attributes", {
  expect_equal(
    ls_attrs(),
    c("registry", "region", "area_type", "province", "city")
  )
})

test_that("ls_vars returns documented variable dictionaries", {
  res <- ls_vars("reframe")

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("code", "cname", "ename", "abbr_cn", "abbr_en"))
  expect_contains(res$code, ls_attrs())
})

test_that("ls_vars validates dictionary type", {
  expect_snapshot(error = TRUE, {
    ls_vars("not_a_type")
  })
})
