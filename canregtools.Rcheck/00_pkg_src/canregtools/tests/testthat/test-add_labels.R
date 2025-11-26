test_that("cr_merge.canregs merges canregs into canreg", {
  data <- data.frame(
    sex = sample(c(1, 2), 100, replace = TRUE),
    cancer = sample(get_cancer("big"), 100, replace = TRUE)
    )

  res <- add_labels(data,
                    vars = c("sex", "cancer"),
                    lang = c("cn", "en"))
  expect_true(is.factor(res$sex_cn_en))
  expect_true(is.factor(res$cancer_cn_en))
  expect_true(all(c("sex_cn_en", "cancer_cn_en") %in% names(res)))
})
