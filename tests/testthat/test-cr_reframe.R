test_that("cr_reframe.canregs stratifies by registry", {
  result <- cr_reframe(canregs, strat = "registry")
  expected_names <- unique(classify_areacode(
    unlist(purrr::map(canregs, "regicode")),
    attr = "registry"
  )[["registry"]])

  expect_s3_class(result, "canregs")
  expect_setequal(names(result), expected_names)
  expect_true(all(purrr::map_lgl(result, inherits, "canreg")))
})

test_that("cr_reframe keeps international regicodes as registry strata", {
  first <- canregs[[1]]
  second <- canregs[[2]]
  first$regicode <- "IARC-GB-001"
  second$regicode <- "IARC-NO-001"
  international <- structure(
    list(first = first, second = second),
    class = c("canregs", "list")
  )

  result <- cr_reframe(international, strat = "registry")

  expect_setequal(names(result), c("IARC-GB-001", "IARC-NO-001"))
  expect_setequal(purrr::map_chr(result, "regicode"), names(result))
})

test_that("cr_reframe.fbswicds stratifies by city", {
  result <- cr_reframe(fbsws, strat = "city")
  expected_names <- unique(classify_areacode(
    unlist(purrr::map(fbsws, "regicode")),
    attr = "city"
  )[["city"]])

  expect_s3_class(result, "fbswicds")
  expect_setequal(names(result), expected_names)
  expect_true(all(purrr::map_lgl(result, inherits, "fbswicd")))
})

test_that("cr_reframe.fbswicds stratifies by registry", {
  result <- cr_reframe(fbsws, strat = "registry")
  expected_names <- unique(classify_areacode(
    unlist(purrr::map(fbsws, "regicode")),
    attr = "registry"
  )[["registry"]])

  expect_s3_class(result, "fbswicds")
  expect_setequal(names(result), expected_names)
  expect_true(all(purrr::map_lgl(result, inherits, "fbswicd")))
})

test_that("cr_reframe.canregs supports multiple stratification variables", {
  result <- cr_reframe(canregs, strat = c("province", "area_type"))
  regicodes <- unique(unlist(purrr::map(canregs, "regicode")))
  expected_names <- unique(unlist(classify_areacode(
    regicodes,
    attr = c("province", "area_type")
  )))

  expect_s3_class(result, "canregs")
  expect_setequal(names(result), expected_names)
})

test_that("cr_reframe.canregs returns canreg when stratification has one group", {
  dict_name <- "test_cr_reframe_single_group"
  on.exit(del_dict_files(dict_name), add = TRUE)

  regicodes <- unique(unlist(purrr::map(canregs, purrr::pluck("regicode"))))
  custom_dict <- rep("SingleGroup", length(regicodes))
  names(custom_dict) <- regicodes
  write_registry(custom_dict, dict = dict_name)

  result <- cr_reframe(canregs, strat = dict_name)

  expect_s3_class(result, "canreg")
  expect_equal(result$regicode, "SingleGroup")
})

test_that("cr_reframe.fbswicds returns fbswicd when stratification has one group", {
  dict_name <- "test_cr_reframe_fbsw_single_group"
  on.exit(del_dict_files(dict_name), add = TRUE)

  regicodes <- unique(unlist(purrr::map(fbsws, purrr::pluck("regicode"))))
  custom_dict <- rep("SingleGroup", length(regicodes))
  names(custom_dict) <- regicodes
  write_registry(custom_dict, dict = dict_name)

  result <- cr_reframe(fbsws, strat = dict_name)

  expect_s3_class(result, "fbswicd")
  expect_equal(result$regicode, "SingleGroup")
})

test_that("cr_reframe propagates missing custom dictionary errors", {
  dict_name <- "test_cr_reframe_missing_custom"
  on.exit(del_dict_files(dict_name), add = TRUE)
  del_dict_files(dict_name)

  expect_snapshot(error = TRUE, {
    cr_reframe(canregs, strat = dict_name)
  })
})

test_that("cr_reframe groups duplicate registry identifiers correctly", {
  input <- canregs
  input[[2]]$regicode <- input[[1]]$regicode
  input[[3]]$regicode <- input[[1]]$regicode

  result <- cr_reframe(input, strat = "regicode")

  expect_s3_class(result, "canreg")
  expect_equal(result$regicode, input[[1]]$regicode)
  expect_equal(
    sum(result$POP$rks),
    sum(input[[1]]$POP$rks) +
      sum(input[[2]]$POP$rks) +
      sum(input[[3]]$POP$rks)
  )
})

test_that("combine_tp accepts an empty frequency list", {
  expect_identical(combine_tp(list()), stats::setNames(integer(), character()))
})
