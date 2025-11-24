# tests/testthat/test-cr_select.R

test_that("cr_select for canregs selects by logical condition", {
  data("canregs")
  selected <- cr_select(canregs, mi > 0.5)
  expect_s3_class(selected, "canregs")
  expect_true(all(purrr::map_lgl(summary(selected, collapse = FALSE), ~ .x$mi > 0.5)))
})



test_that("cr_select for fbswicds selects by condition", {
  selected <- cr_select(fbsws, area_type == "920000")
  expect_s3_class(selected, "fbswicds")
})



test_that("cr_select for asrs selects by condition", {
  asr_obj <- create_asr(fbsws, collapse = FALSE)
  selected <- cr_select(asr_obj, cr > 300)
  expect_s3_class(selected, "asrs")
  expect_true(all(purrr::map_lgl(selected, ~ .x$cr > 300)))
})
