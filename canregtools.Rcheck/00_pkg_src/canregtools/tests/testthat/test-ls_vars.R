test_that("multiplication works", {
  res <- ls_attrs()
  expect_equal(res, c("registry", "region", "area_type", "province", "city"))
})
