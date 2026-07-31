test_that("get_pop summarizes canregs population by sex", {
  pop <- get_pop(canregs, sum_by = "sex", collapse = TRUE)

  expect_s3_class(pop, "data.frame")
  expect_named(pop, c("areacode", "sex", "rks"))
  expect_setequal(pop$sex, c(1L, 2L))
  expect_equal(nrow(pop), length(canregs) * 2)
  expect_gt(sum(pop$rks), 0)
})

test_that("get_pop returns lists for canregs when collapse is FALSE", {
  pop <- get_pop(canregs, sum_by = "sex", collapse = FALSE)

  expect_type(pop, "list")
  expect_length(pop, length(canregs))
  expect_named(pop, names(canregs))
  expect_equal(
    unname(purrr::map_lgl(pop, ~ identical(names(.x), c("sex", "rks")))),
    rep(TRUE, length(pop))
  )
})
