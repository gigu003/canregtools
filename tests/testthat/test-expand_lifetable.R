test_that("expand_lifetable works with valid input", {
  lx <- c(
    100000, 99498.39, 99294.62, 99173.88, 99047.59, 98840.46,
    98521.16, 98161.25, 97636.99, 96900.13, 95718.96, 93930.91,
    91463.21, 87131.41, 80525.02, 70907.59, 58090.75, 41630.48,
    24019.33
  ) / 100000
  
  result <- expand_lifetable(lx)
  
  expect_type(result, "list")
  expect_named(result, c("fitlx", "fitmx"))
  expect_length(result$fitlx, 100)
  expect_length(result$fitmx, 100)
  expect_true(all(result$fitlx >= 0))
  expect_true(all(is.finite(result$fitlx)))
  expect_true(all(is.finite(result$fitmx)))
  expect_true(abs(result$fitlx[1] - 1) < 1e-8) # starts at radix 1
})

test_that("expand_lifetable throws error with invalid input", {
  expect_error(expand_lifetable("not numeric"), "`lx` must be a numeric vector")
  expect_error(expand_lifetable(rep(1, 18)), "`lx` must have a length of 19")
  expect_error(expand_lifetable(c(rep(1, 18), -0.1)), "`lx` must contain only positive values")
})

test_that("fitlx is monotonically decreasing or constant", {
  lx <- c(
    100000, 99498.39, 99294.62, 99173.88, 99047.59, 98840.46,
    98521.16, 98161.25, 97636.99, 96900.13, 95718.96, 93930.91,
    91463.21, 87131.41, 80525.02, 70907.59, 58090.75, 41630.48,
    24019.33
  ) / 100000
  
  result <- expand_lifetable(lx)
  
  diffs <- diff(result$fitlx)
  expect_true(all(diffs <= 1e-8))  # Allow tiny numeric noise
})

