test_that("check_canreg.canreg returns a structured result", {
  data("canregs")
  result <- check_canreg(canregs[[1]])
  expect_s3_class(result, "check")
  expect_named(result, c("FBcases", "SWcases", "POP"))
})
test_that("check_canreg.canregs returns a structured result", {
  data("canregs")
  result <- check_canreg(canregs)
  expect_s3_class(result, "checks")
})
