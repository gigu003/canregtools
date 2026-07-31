test_that("cr_select for canregs selects by logical condition", {
  selected <- cr_select(canregs, mi > 0.5)

  expect_s3_class(selected, "canregs")
  expect_named(selected, "411721")

  selected_summary <- summary(selected, collapse = FALSE)
  expect_equal(unname(purrr::map_dbl(selected_summary, "mi")), 0.67)
})

test_that("cr_select for fbswicds selects by condition", {
  selected <- cr_select(fbsws, area_type == "920000")

  expect_s3_class(selected, "fbswicds")
  expect_named(selected, "411721")
  expect_equal(unique(purrr::map_chr(selected, "areacode")), "411721")
})

test_that("cr_select supports explicit indexes", {
  selected <- cr_select(canregs, index = names(canregs)[1:2])

  expect_s3_class(selected, "canregs")
  expect_named(selected, names(canregs)[1:2])
  expect_length(selected, 2)
})

test_that("cr_select for asrs selects by condition", {
  asr_obj <- create_asr(fbsws, collapse = FALSE)
  selected <- cr_select(asr_obj, cr > 300)

  expect_s3_class(selected, "asrs")
  expect_named(selected, "410103")
  expect_equal(
    unname(purrr::map_lgl(selected, ~ any(.x$cr > 300))),
    rep(TRUE, length(selected))
  )
})

test_that("cr_select for fbswicds can use quality conditions", {
  selected <- cr_select(fbsws, inci > 300)

  expect_s3_class(selected, "fbswicds")
  expect_named(selected, "410103")
})
