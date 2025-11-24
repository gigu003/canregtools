test_that("expand_age_pop works with default linear method", {
  grouped <- c(
    5053, 17743, 25541, 32509, 30530, 34806, 36846, 38691, 40056,
    39252, 37349, 30507, 26363, 21684, 15362, 11725, 7461, 3260, 915
  )
  
  result <- expand_age_pop(grouped)
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("x", "y"))
  expect_equal(length(result$x), 93)
  expect_equal(length(result$y), 93)
  expect_true(all(result$x == 0:92))
  expect_true(abs(sum(result$y) - sum(grouped)) <= 5)
})

test_that("expand_age_pop handles constant interpolation", {
  grouped <- rep(1000, 19)
  result <- expand_age_pop(grouped, method = "constant")
  expect_equal(length(result$y), 93)
  expect_true(abs(sum(result$y) - sum(grouped)) <= 5)
})

test_that("expand_age_pop fills NAs with zero", {
  grouped <- c(rep(NA, 5), rep(1000, 14))
  result <- expand_age_pop(grouped)
  expect_equal(sum(result$y), sum(grouped, na.rm = TRUE), tolerance = 10)
})

test_that("expand_age_pop returns all-zero output for wrong input length", {
  result <- expand_age_pop(1:10)
  expect_equal(sum(result$y), 0)
  expect_equal(nrow(result), 93)
})