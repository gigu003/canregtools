test_that("cr_reframe.canregs stratifies by registry", {
  result <- cr_reframe(canregs, strat = "registry")
  expected_names <- unique(classify_areacode(
    unlist(purrr::map(canregs, "areacode")),
    attr = "registry"
  )[["registry"]])

  expect_s3_class(result, "canregs")
  expect_setequal(names(result), expected_names)
  expect_true(all(purrr::map_lgl(result, inherits, "canreg")))
})

test_that("cr_reframe.fbswicds stratifies by city", {
  result <- cr_reframe(fbsws, strat = "city")
  expected_names <- unique(classify_areacode(
    unlist(purrr::map(fbsws, "areacode")),
    attr = "city"
  )[["city"]])

  expect_s3_class(result, "fbswicds")
  expect_setequal(names(result), expected_names)
  expect_true(all(purrr::map_lgl(result, inherits, "fbswicd")))
})

test_that("cr_reframe.canregs supports multiple stratification variables", {
  result <- cr_reframe(canregs, strat = c("province", "area_type"))
  areacodes <- unique(unlist(purrr::map(canregs, "areacode")))
  expected_names <- unique(unlist(classify_areacode(
    areacodes,
    attr = c("province", "area_type")
  )))

  expect_s3_class(result, "canregs")
  expect_setequal(names(result), expected_names)
})

test_that("cr_reframe.canregs returns canreg when stratification has one group", {
  dict_name <- "test_cr_reframe_single_group"
  on.exit(del_dict_files(dict_name), add = TRUE)

  areacodes <- unique(unlist(purrr::map(canregs, purrr::pluck("areacode"))))
  custom_dict <- rep("SingleGroup", length(areacodes))
  names(custom_dict) <- areacodes
  write_registry(custom_dict, dict = dict_name)

  result <- cr_reframe(canregs, strat = dict_name)

  expect_s3_class(result, "canreg")
  expect_equal(result$areacode, "SingleGroup")
})

test_that("cr_reframe.fbswicds returns fbswicd when stratification has one group", {
  dict_name <- "test_cr_reframe_fbsw_single_group"
  on.exit(del_dict_files(dict_name), add = TRUE)

  areacodes <- unique(unlist(purrr::map(fbsws, purrr::pluck("areacode"))))
  custom_dict <- rep("SingleGroup", length(areacodes))
  names(custom_dict) <- areacodes
  write_registry(custom_dict, dict = dict_name)

  result <- cr_reframe(fbsws, strat = dict_name)

  expect_s3_class(result, "fbswicd")
  expect_equal(result$areacode, "SingleGroup")
})

test_that("cr_reframe propagates missing custom dictionary errors", {
  dict_name <- "test_cr_reframe_missing_custom"
  on.exit(del_dict_files(dict_name), add = TRUE)
  del_dict_files(dict_name)

  expect_snapshot(error = TRUE, {
    cr_reframe(canregs, strat = dict_name)
  })
})
