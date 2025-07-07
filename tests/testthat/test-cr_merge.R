test_that("cr_merge.canregs merges canregs into canreg", {
  data("canregs", package = "canregtools")
  res <- cr_merge(canregs)
  
  expect_s3_class(res, "canreg")
  expect_true(all(c("FBcases", "SWcases", "POP") %in% names(res)))
  expect_s3_class(res$POP, "POP")
})

test_that("cr_merge.fbswicds merges fbswicds into fbswicd", {
  data("canregs", package = "canregtools")
  fbsws <- count_canreg(canregs)
  res <- cr_merge(fbsws)
  
  expect_s3_class(res, "fbswicd")
  expect_true(all(c("fbswicd", "sitemorp", "pop") %in% names(res)))
})

test_that("cr_merge.asrs merges asrs into asr", {
  data("canregs", package = "canregtools")
  asrs <- create_asr(canregs, year, sex, cancer, collapse = FALSE)
  res <- cr_merge(asrs)
  
  expect_s3_class(res, "asr")
})

test_that("cr_merge.qualities merges qualities into quality", {
  data("canregs", package = "canregtools")
  quas <- create_quality(canregs, year, sex, cancer, collapse = FALSE)
  res <- cr_merge(quas)
  
  expect_s3_class(res, "quality")
})

test_that("cr_merge.age_rates merges age_rates into age_rate", {
  data("canregs", package = "canregtools")
  agerates <- create_age_rate(canregs, year, sex, cancer, collapse = FALSE)
  res <- cr_merge(agerates)
  
  expect_s3_class(res, "age_rate")
})

test_that("cr_merge.summaries merges summaries into summary", {
  data("canregs", package = "canregtools")
  summs <- summary(canregs, collapse = FALSE)
  res <- cr_merge(summs)
  
  expect_s3_class(res, "summary")
  expect_true(is.list(res))
})
