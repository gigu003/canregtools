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

test_that("cr_reframe.canregs supports custom dictionaries from write_registry", {
  data("canregs")
  dict_name <- "urban_test_cr_reframe"
  on.exit(del_dict_files(dict_name), add = TRUE)
  
  areacodes <- unique(unlist(purrr::map(canregs, purrr::pluck("areacode"))))
  custom_dict <- rep("UrbanCustom", length(areacodes))
  names(custom_dict) <- areacodes
  write_registry(custom_dict, dict = dict_name)
  
  classified <- classify_areacode2(areacodes, attr = dict_name)[[dict_name]]
  expect_equal(unique(classified), "UrbanCustom")
  
  result <- cr_reframe(canregs, strat = dict_name)
  expect_s3_class(result, "canreg")
  expect_equal(result$areacode, "UrbanCustom")
})
