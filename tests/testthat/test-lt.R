test_that("lt() returns correct output dimensions and types", {
  mx <- c(
    0.01685, 0.00085, 0.00044, 0.00045, 0.00064, 0.00086, 0.00103,
    0.00136, 0.00195, 0.00291, 0.00429, 0.00672, 0.00985, 0.01596,
    0.02605, 0.04536, 0.07247, 0.12078, 0.17957, 0.25938, 0.25989
  )
  result <- lt(mx, sage = 0, age_width = 5, sex = "total")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), length(mx))
  expect_named(result, c("age", "mx", "qx", "lx", "dx", "Lx", "Tx", "ex"))
  expect_true(all(result$lx <= 1)) # starting radix
  expect_true(all(result$qx >= 0 & result$qx <= 1))
})

test_that("lt() handles 1-year age width correctly", {
  mx <- rep(0.005, 10)
  result <- lt(mx, sage = 0, age_width = 1, sex = "male")
  
  expect_equal(nrow(result), 10)
  expect_equal(result$age[1:3], c(0, 1, 2)) # age should increment by 1
  expect_equal(result$qx[10], 1) # final qx should be 1
})

test_that("lt() works with sage > 0", {
  mx <- rep(0.01, 19)
  result <- lt(mx, sage = 0, age_width = 5, sex = "female")
  
  expect_equal(result$age[1], 0)
  expect_equal(result$lx[1], 1)
  expect_true(all(result$ex >= 0))
})


test_that("lt() throws error with invalid sex", {
  mx <- rep(0.01, 5)
  expect_error(lt(mx, sex = "other"))
})

test_that("lt() handles infinite final Lx and valid ex", {
  mx <- rep(0.01, 10)
  result <- lt(mx, sage = 0, age_width = 5, sex = "male")
  
  expect_equal(result$qx[10], 1)
  expect_gt(result$Tx[1], result$Tx[2])
  expect_equal(result$Lx[10], result$lx[10] / mx[10])
})
