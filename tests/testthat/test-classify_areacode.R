test_that("classify_areacode correctly classifies valid area codes", {
  res <- classify_areacode(c("110000", "320500", "440300"))
  
  expect_named(res, c("areacode", "registry", "province", "city", "area_type", "region"))
  expect_length(res$areacode, 3)
  expect_true(all(nchar(res$province) == 6))
  expect_true(all(grepl("^91|^92", res$area_type)))  # urban/rural
})

test_that("classify_areacode returns NA for invalid codes", {
  res <- classify_areacode(c("999999", "123"))
  expect_true(all(is.na(res$areacode)))
})