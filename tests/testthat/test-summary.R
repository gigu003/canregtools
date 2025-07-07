test_that("summary.canreg returns expected structure", {
  data("canregs", package = "canregtools")
  data <- canregs[[1]]
  result <- summary(data)
  expect_s3_class(result, "summary")
  expect_named(result, c(
    "areacode", "rks", "fbs", "inci", "sws", "mort", "mi",
    "mv", "dco", "death", "rks_year",
    "inci_vars", "miss_r_vars_inci",
    "mort_vars", "miss_r_vars_mort"
  ))
  expect_type(result$rks, "double")
  expect_type(result$fbs, "integer")
  expect_type(result$mv, "double")
  expect_true(result$mv >= 0)
})

test_that("summary.canreg handles missing population correctly", {
  data("canregs", package = "canregtools")
  data <- canregs[[1]]
  data$POP$rks <- NA
  
  result <- summary(data)
  expect_equal(result$rks, 0)
  expect_equal(result$inci, 0)
  expect_equal(result$mort, 0)
})

test_that("summary.canregs returns a list of summaries", {
  data("canregs", package = "canregtools")
  result <- summary(canregs, collapse = FALSE)
  
  expect_type(result, "list")
  expect_length(result, length(canregs))
  expect_s3_class(result[[1]], "summary")
})

test_that("summary.canregs collapse = TRUE returns merged tibble", {
  data("canregs", package = "canregtools")
  result <- summary(canregs, collapse = TRUE)
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("rks", "fbs", "sws") %in% names(result)))
})
