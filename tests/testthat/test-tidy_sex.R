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
