test_that("cr_filter", {
  res <- cr_filter(canregs, substr(icd10,1,3) == "C73")
  res2 <- cr_filter(canregs[[1]], year == 2021)
})
