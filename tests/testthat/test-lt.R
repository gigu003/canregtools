test_that("lt() returns correct output dimensions and types", {
  mx <- c(
    0.01685,
    0.00085,
    0.00044,
    0.00045,
    0.00064,
    0.00086,
    0.00103,
    0.00136,
    0.00195,
    0.00291,
    0.00429,
    0.00672,
    0.00985,
    0.01596,
    0.02605,
    0.04536,
    0.07247,
    0.12078,
    0.17957,
    0.25938,
    0.25989
  )
  result <- lt(mx = mx, sage = c(0, 1, seq(5, 95, 5)), sex = "total")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), length(mx))
  expect_named(result, c("age", "mx", "qx", "lx", "dx", "Lx", "Tx", "ex"))
  expect_equal(all(result$qx >= 0 & result$qx <= 1), TRUE)
  expect_equal(result$lx[[1]], 100000)
  expect_equal(result$qx[[length(mx)]], 1)
  expect_equal(all(diff(result$lx) <= 0), TRUE)
})

test_that("expand_lx works with valid input", {
  lx <- c(
    100000,
    99498.39,
    99294.62,
    99173.88,
    99047.59,
    98840.46,
    98521.16,
    98161.25,
    97636.99,
    96900.13,
    95718.96,
    93930.91,
    91463.21,
    87131.41,
    80525.02,
    70907.59,
    58090.75,
    41630.48,
    24019.33
  ) /
    100000

  result <- expand_lx(lx, sage = c(0, 1, seq(5, 85, 5)))

  expect_type(result, "list")
  expect_named(result, c("fitlx", "fitmx"))
  expect_length(result$fitlx, 101)
  expect_length(result$fitmx, 101)
  expect_equal(all(result$fitlx >= 0), TRUE)
  expect_equal(all(is.finite(result$fitlx)), TRUE)
  expect_equal(all(is.finite(result$fitmx)), TRUE)
  expect_equal(result$fitlx[[1]], 1, tolerance = 1e-8)
})


test_that("fitlx is monotonically decreasing or constant", {
  lx <- c(
    100000,
    99498.39,
    99294.62,
    99173.88,
    99047.59,
    98840.46,
    98521.16,
    98161.25,
    97636.99,
    96900.13,
    95718.96,
    93930.91,
    91463.21,
    87131.41,
    80525.02,
    70907.59,
    58090.75,
    41630.48,
    24019.33
  ) /
    100000

  result <- expand_lx(lx)

  diffs <- diff(result$fitlx)
  expect_equal(all(diffs <= 1e-8), TRUE)
})

test_that("lt validates inputs", {
  expect_snapshot(error = TRUE, {
    lt(mx = c(0.1, 0.2), sage = c(0))
  })

  expect_snapshot(error = TRUE, {
    lt(mx = c(0.1, 0.2), sage = c(1, 0))
  })
})

test_that("expand_lx validates inputs and handles mx-like input", {
  expect_snapshot(error = TRUE, {
    expand_lx(c(1, 2), sage = c(0, 5))
  })

  expect_snapshot({
    result <- expand_lx(rep(0.01, 19))
  })
  expect_named(result, c("fitlx", "fitmx"))
})
