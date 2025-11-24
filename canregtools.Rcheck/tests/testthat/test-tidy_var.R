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

test_that("tidy_sex handles basic gender terms", {
  input <- c("male", "female", "man", "woman", "men", "women")
  expect_equal(tidy_sex(input), c(1, 2, 1, 2, 1, 2))
})

test_that("tidy_sex handles numeric input as string", {
  input <- c("1", "2", "0")
  expect_equal(tidy_sex(input), c(1, 2, 0))
})

test_that("tidy_sex handles Chinese gender labels", {
  input <- c("男", "女", "合", "合计")
  expect_equal(tidy_sex(input), c(1, 2, 0, 0))
})

test_that("tidy_sex returns factors with Chinese labels", {
  input <- c("male", "female", "total")
  result <- tidy_sex(input, lang = "cn", as_factor = TRUE)
  expect_true(is.factor(result))
  expect_equal(levels(result), c("合计", "男性", "女性"))
})

test_that("tidy_sex returns factors with English labels", {
  input <- c("male", "female", "total")
  result <- tidy_sex(input, lang = "en", as_factor = TRUE)
  expect_true(is.factor(result))
  expect_equal(levels(result), c("Total", "Male", "Female"))
})

test_that("tidy_sex handles unknown input as NA", {
  input <- c("unknown", "other", "x")
  expect_equal(tidy_sex(input), rep(NA_integer_, 3))
})

# Define test cases for tidy_age function
test_that("tidy_age returns expected results", {
  # Test case 1: Check default unit ('year')
  agedes <- c("50岁3月", "19岁25天", "1岁30天", "3岁1月", "60天")
  expect_result <- c(50, 19, 1, 3, 0)
  expect_equal(tidy_age(agedes), expect_result)
  
  # Test case 2: Check unit 'month'
  expect_result_month <- c(602, 228, 12, 36, 1)
  expect_equal(tidy_age(agedes, unit = "month"), expect_result_month)
  
  # Test case 3: Check unit 'day'
  expect_result_day <- c(18354, 6965, 395, 1126, 60)
  expect_equal(tidy_age(agedes, unit = "day"), expect_result_day)
})
