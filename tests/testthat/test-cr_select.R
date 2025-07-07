# tests/testthat/test-cr_select.R

test_that("cr_select for canregs selects by logical condition", {
  data("canregs")
  selected <- cr_select(canregs, mi > 0.5)
  expect_s3_class(selected, "canregs")
  expect_true(all(purrr::map_lgl(summary(selected, collapse = FALSE), ~ .x$mi > 0.5)))
})

test_that("cr_select for canregs selects by index", {
  data("canregs")
  selected <- cr_select(canregs, index = 1)
  expect_length(selected, 1)
  expect_s3_class(selected, "canregs")
})

test_that("cr_select for fbswicds selects by condition", {
  data("canregs")
  fbsws <- count_canreg(canregs)
  selected <- cr_select(fbsws, inci > 250)
  expect_s3_class(selected, "fbswicds")
  expect_true(all(purrr::map_lgl(create_quality(selected, collapse = FALSE), ~ .x$inci > 250)))
})

test_that("cr_select for fbswicds selects by index", {
  data("canregs")
  fbsws <- count_canreg(canregs)
  selected <- cr_select(fbsws, index = 1)
  expect_length(selected, 1)
  expect_s3_class(selected, "fbswicds")
})

test_that("cr_select for summaries filters correctly", {
  data("canregs")
  summ <- summary(canregs, collapse = FALSE)
  filtered <- cr_select(summ, mi > 0.5)
  expect_s3_class(filtered, "summaries")
  expect_true(all(purrr::map_lgl(filtered, ~ .x$mi > 0.5)))
})

test_that("cr_select for asrs selects by condition", {
  data("canregs")
  asr_obj <- create_asr(count_canreg(canregs), collapse = FALSE)
  selected <- cr_select(asr_obj, cr > 300)
  expect_s3_class(selected, "asrs")
  expect_true(all(purrr::map_lgl(selected, ~ .x$cr > 300)))
})
