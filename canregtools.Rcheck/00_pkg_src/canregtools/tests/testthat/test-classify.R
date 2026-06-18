test_that("classify_areacode2 can return original area codes", {
  areacodes <- c("110000", "320500", "bad")
  result <- classify_areacode2(areacodes, attr = "areacode")

  expect_named(result, "areacode")
  expect_equal(result$areacode, areacodes)
})

test_that("classify_morp returns 99 for unmatched morphology codes", {
  result <- classify_morp(c("8140", "not_found"))

  expect_equal(result[2], "99")
  expect_type(result, "character")
})
