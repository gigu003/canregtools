test_that("cr_clean.canregs applies recursively and preserves class", {
  data("canregs")
  cleaned <- cr_clean(canregs)
  expect_s3_class(cleaned, "canregs")
  expect_true(all(purrr::map_lgl(cleaned, ~ inherits(.x, "canreg"))))
})

test_that("cr_clean.POP applies recursively and preserves class", {
  pop <- dplyr::tibble(
    year = integer(),
    sex = integer(),
    agegrp = factor(levels = character()),  # 可设置 levels 保持一致性
    rks = integer(),
    death = integer()
  )
  class(pop) <- c("POP", class(pop))
  res <- cr_clean.POP(pop)
  expect_s3_class(res, "POP")
})
