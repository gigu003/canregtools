test_that("cr_reframe.canregs stratifies by registry", {
  data("canregs")
  result <- cr_reframe(canregs, strat = "registry")
  expect_s3_class(result, "canregs")
  expect_true(all(names(result) %in% unique(classify_areacode(unlist(purrr::map(canregs, "areacode")))$registry)))
})

test_that("cr_reframe.canregs stratifies by province", {
  data("canregs")
  result <- cr_reframe(canregs, strat = "province")
  expect_s3_class(result, "canreg")
  expect_false(all(names(result) %in% unique(classify_areacode(unlist(purrr::map(canregs, "areacode")))$province)))
})

test_that("cr_reframe.fbswicds returns correct class and names", {
  data("canregs")
  fbsw <- count_canreg(canregs, cancer_type = "small")
  result <- cr_reframe(fbsw, strat = "city")
  expect_s3_class(result, "fbswicds")
  expect_true(all(names(result) %in% unique(classify_areacode(unlist(purrr::map(fbsw, "areacode")))$city)))
})

test_that("cr_reframe returns a single object when only one level", {
  data("canregs")
  # Use a dummy stratification that results in one group
  single_group <- cr_reframe(canregs, strat = "country")  # assuming all are in same country
  expect_true(inherits(single_group, "canregs") || inherits(single_group, "fbswicds"))
})

test_that("cr_reframe returns a single object when only one level", {
  data("canregs")
  fbswicds <- count_canreg(canregs)
  # Use a dummy stratification that results in one group
  fbsw <- cr_reframe(fbswicds, strat = "province")  # assuming all are in same country
  expect_true(inherits(fbsw, "fbswicd"))
})

test_that("full_fbswicd", {
  data("canregs")
  fbswicd <- count_canreg(canregs[[1]])
  result <- full_fbswicd(fbswicd)
  expect_s3_class(result, "fbswicd")
  expect_named(result, c("areacode", "fbswicd", "sitemorp", "pop"))
  expect_true(all(c("province", "city", "registry") %in% names(result$fbswicd)))
  expect_true(all(c("province", "city", "registry") %in% names(result$sitemorp)))
  expect_true(all(c("province", "city", "registry") %in% names(result$pop)))
})