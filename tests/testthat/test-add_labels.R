test_that("cr_merge.canregs merges canregs into canreg", {
  data("canregs")
  qua1 <- create_quality(canregs)
  qua2 <- qua1 |> add_labels()
  
  expect_s3_class(qua1, "quality")
  expect_true(all(c("areacode", "name") %in% names(qua2)))
})