test_that("lt() returns correct output dimensions and types", {
  mx <- c(
    0.01685, 0.00085, 0.00044, 0.00045, 0.00064, 0.00086, 0.00103,
    0.00136, 0.00195, 0.00291, 0.00429, 0.00672, 0.00985, 0.01596,
    0.02605, 0.04536, 0.07247, 0.12078, 0.17957, 0.25938, 0.25989
  )
  result <- lt(mx = mx, sage = c(0, 1, seq(5, 95, 5)), sex = "total")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), length(mx))
  expect_named(result, c("age", "mx", "qx", "lx", "dx", "Lx", "Tx", "ex"))
  expect_true(all(result$qx >= 0 & result$qx <= 1))
})

test_that("expand_lx works with valid input", {
  lx <- c(
    100000, 99498.39, 99294.62, 99173.88, 99047.59, 98840.46,
    98521.16, 98161.25, 97636.99, 96900.13, 95718.96, 93930.91,
    91463.21, 87131.41, 80525.02, 70907.59, 58090.75, 41630.48,
    24019.33
  ) / 100000
  
  result <- expand_lx(lx, sage = c(0, 1, seq(5, 85, 5)))
  
  expect_type(result, "list")
  expect_named(result, c("fitlx", "fitmx"))
  expect_length(result$fitlx, 101)
  expect_length(result$fitmx, 101)
  expect_true(all(result$fitlx >= 0))
  expect_true(all(is.finite(result$fitlx)))
  expect_true(all(is.finite(result$fitmx)))
  expect_true(abs(result$fitlx[1] - 1) < 1e-8)
})


test_that("fitlx is monotonically decreasing or constant", {
  lx <- c(
    100000, 99498.39, 99294.62, 99173.88, 99047.59, 98840.46,
    98521.16, 98161.25, 97636.99, 96900.13, 95718.96, 93930.91,
    91463.21, 87131.41, 80525.02, 70907.59, 58090.75, 41630.48,
    24019.33
  ) / 100000
  
  result <- expand_lx(lx)
  
  diffs <- diff(result$fitlx)
  expect_true(all(diffs <= 1e-8))
})
