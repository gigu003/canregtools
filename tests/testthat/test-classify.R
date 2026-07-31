test_that("classify_icd10 maps ICD10 codes to cancer categories", {
  icd10 <- c("C15.2", "C33.4", "C80.9")

  expect_equal(
    classify_icd10(icd10, cancer_type = "big"),
    c("103", "110", "126")
  )
  expect_equal(
    classify_icd10(icd10, cancer_type = "small"),
    c("10", "21", "59")
  )
})

test_that("classify_icd10 validates cancer_type", {
  expect_snapshot(error = TRUE, {
    classify_icd10("C15.2", cancer_type = "invalid")
  })
})

test_that("classify_areacode returns requested built-in attributes", {
  areacodes <- c("110000", "320500", "bad")
  result <- classify_areacode(
    areacodes,
    attr = c("areacode", "province", "city", "region")
  )

  expect_named(result, c("areacode", "province", "city", "region"))
  expect_equal(result$areacode, areacodes)
  expect_equal(result$province, c("110000", "320000", NA_character_))
  expect_equal(result$city, c("110000", "320500", NA_character_))
  expect_equal(result$region, c("720000", "740000", NA_character_))
})

test_that("classify_areacode validates attr", {
  expect_snapshot(error = TRUE, {
    classify_areacode("110000", attr = character())
  })
})

test_that("classify_areacode supports custom dictionaries", {
  dict_name <- "test_classify_custom"
  on.exit(del_dict_files(dict_name), add = TRUE)

  custom_dict <- c("110000" = "Capital", "320500" = "Suzhou")
  write_registry(custom_dict, dict = dict_name)

  result <- classify_areacode(c("110000", "320500", "410000"), attr = dict_name)

  expect_named(result, dict_name)
  expect_equal(result[[dict_name]], c("Capital", "Suzhou", "410000"))
})

test_that("classify_areacode errors for missing custom dictionaries", {
  dict_name <- "test_classify_missing_custom"
  on.exit(del_dict_files(dict_name), add = TRUE)
  del_dict_files(dict_name)

  expect_snapshot(error = TRUE, {
    classify_areacode("110000", attr = dict_name)
  })
})

test_that("classify_morp maps morphology groups and unmatched codes", {
  expect_equal(
    classify_morp(c("8140", "9811", "not_found")),
    c("6", "52", "99")
  )
  expect_equal(
    classify_morp(c("9811", "9840", "9801", "not_found"), group = "125"),
    c("201", "202", "204", "99")
  )
  expect_type(classify_morp("8140"), "character")
})

test_that("classify_topo normalizes topography codes", {
  topo <- c("C15.0", "C15.1", "C15.2", "C18.1")

  expect_equal(
    classify_topo(topo, cancer_type = "big"),
    c("C15.3", "C15.4", "C15.5", "C18")
  )
  expect_equal(
    classify_topo(topo, cancer_type = "small"),
    rep(c("C15", "C18"), c(3, 1))
  )
})

test_that("classify_childhood returns main and subgroup codes", {
  topo <- c("C15.2", "C16.2", "C34.2")
  morp <- c("8000", "8040", "8170")
  beha <- c("3", "3", "3")

  expect_equal(
    classify_childhood(topo, morp, beha, type = "main"),
    c(99, 99, 7)
  )
  expect_equal(
    classify_childhood(topo, morp, beha, type = "sub"),
    c("999", "999", "072")
  )
})
