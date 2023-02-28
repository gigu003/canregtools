test_that("multiplication works", {
  icd10 <- c("C15.0", "C16.0", "C17.0", "C18.0", "C33.0",
             "D42.0", "D45.2")
  res <- c(3, 4, 26, 5, 10, 22, 25)
  res_cn <- factor(res, levels = c(1:26), labels = label[[2]]$cn)
  res_en <- factor(res, levels = c(1:26), labels = label[[2]]$en)
  expect_equal(classify_icd10(icd10), res_cn)
  expect_equal(classify_icd10(icd10, lang = "en"), res_en)
})
