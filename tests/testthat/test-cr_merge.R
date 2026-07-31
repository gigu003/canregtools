test_that("cr_merge.canregs preserves character areacodes in merged data", {
  reg_a <- canregs[[1]]
  reg_b <- canregs[[2]]
  reg_a$areacode <- "REG_A"
  reg_b$areacode <- "SITE-02"

  regs <- structure(
    list(first = reg_a, second = reg_b),
    class = c("canregs", "list")
  )

  merged <- cr_merge(regs)
  expect_s3_class(merged, "canreg")
  expect_identical(merged$areacode, c("REG_A", "SITE-02"))
  expect_type(merged$FBcases$areacode, "character")
  expect_setequal(unique(merged$FBcases$areacode), c("REG_A", "SITE-02"))
  expect_s3_class(merged$POP, "POP")

  counted <- count_canreg(regs)
  merged_fbsw <- cr_merge(counted)
  expect_s3_class(merged_fbsw, "fbswicd")
  expect_type(merged_fbsw$areacode, "character")
  expect_setequal(merged_fbsw$areacode, c("REG_A", "SITE-02"))
  expect_type(merged_fbsw$exclude$areacode, "character")
})

test_that("cr_merge combines derived list classes into data frames", {
  asrs <- create_asr(fbsws, year, sex, collapse = FALSE)
  qualities <- create_quality(fbsws, year, sex, collapse = FALSE)
  age_rates <- create_age_rate(fbsws, year, sex, collapse = FALSE)

  expect_s3_class(cr_merge(asrs), "asr")
  expect_s3_class(cr_merge(qualities), "quality")
  expect_s3_class(cr_merge(age_rates), "age_rate")
  expect_contains(names(cr_merge(asrs)), "areacode")
})
