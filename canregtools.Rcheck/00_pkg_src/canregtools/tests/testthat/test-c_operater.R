library(testthat)

test_that("c.canregs combines canregs objects correctly", {
  # 构造模拟 canreg 对象
  cr1 <- list(list(id = 1), list(id = 2))
  cr2 <- list(list(id = 3))
  class(cr1) <- c("canregs", "list")
  class(cr2) <- c("canregs", "list")
  
  combined <- c(cr1, cr2)
  
  expect_s3_class(combined, "canregs")
  expect_type(combined, "list")
  expect_length(combined, 3)
  expect_equal(combined[[1]]$id, 1)
  expect_equal(combined[[3]]$id, 3)
})

test_that("c.canregs throws error with non-canregs input", {
  cr <- list(list(id = 1))
  class(cr) <- c("canregs", "list")
  bad_input <- list(list(id = 99))
  class(bad_input) <- "not_canregs"
  expect_error(c(cr, bad_input), "All arguments must be 'canregs' objects")
})
