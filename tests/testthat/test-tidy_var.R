test_that("tidy_var works with valid occupation codes (Chinese full)", {
  x <- c("11", "13", "17")
  res <- tidy_var(x, var_name = "occu", lang = "cn", label_type = "full")
  expect_type(res, "character")
  expect_length(res, 3)
})

test_that("tidy_var works with English full labels", {
  x <- c("11", "13", "17")
  res <- tidy_var(x, var_name = "occu", lang = "en", label_type = "full")
  expect_type(res, "character")
})

test_that("tidy_var works with Chinese abbreviation", {
  x <- c("11", "13", "17")
  res <- tidy_var(x, var_name = "occu", lang = "cn", label_type = "abbr")
  expect_type(res, "character")
})

test_that("tidy_var returns factor when as_factor = TRUE", {
  x <- c("11", "13", "17")
  res <- tidy_var(x, var_name = "occu", lang = "en", as_factor = TRUE)
  expect_s3_class(res, "factor")
})

test_that("tidy_var handles invalid codes with NA", {
  x <- c("invalid", "99")
  res <- tidy_var(x, var_name = "occu")
  expect_true(all(is.na(res)))
})

test_that("tidy_var returns ICD-10 codes when lang = 'icd10'", {
  x <- c("1", "2", "3")
  res <- tidy_var(x, var_name = "cancer", lang = "icd10")
  expect_type(res, "character")
})

test_that("tidy_var returns original code when lang = 'code'", {
  x <- c("1", "2", "3")
  res <- tidy_var(x, var_name = "cancer", lang = "code")
  expect_equal(res, x)
})

test_that("tidy_var errors when var_name is unsupported", {
  expect_error(tidy_var(c("11"), var_name = "unknown"))
})

