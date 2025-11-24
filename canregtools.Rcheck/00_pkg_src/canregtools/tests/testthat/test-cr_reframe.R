test_that("cr_reframe.canregs stratifies by registry", {
  data("canregs")
  result <- cr_reframe(canregs, strat = "registry")
  expect_s3_class(result, "canregs")
  expect_true(all(names(result) %in% unique(classify_areacode(unlist(purrr::map(canregs, "areacode")))$registry)))
})


test_that("cr_reframe.fbswicds returns correct class and names", {
  result <- cr_reframe(fbsws, strat = "city")
  expect_s3_class(result, "fbswicds")
  expect_true(all(names(result) %in% unique(classify_areacode(unlist(purrr::map(fbsws, "areacode")))$city)))
})
