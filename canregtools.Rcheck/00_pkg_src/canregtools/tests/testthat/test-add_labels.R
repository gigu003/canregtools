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

test_that("add_labels passes suffix to tidy_var for cancer labels", {
  data <- data.frame(cancer = c("1", "27"))

  res <- add_labels(data, vars = "cancer", lang = "cn", suffix = TRUE)

  expect_s3_class(res$cancer_cn, "factor")
  expect_equal(as.character(res$cancer_cn), c("唇癌", "卡波西肉瘤"))
})

test_that("add_labels supports cancer suffix with multiple languages", {
  data <- data.frame(cancer = c("1", "27"))

  res <- add_labels(
    data,
    vars = "cancer",
    lang = c("cn", "en"),
    sep = " / ",
    suffix = TRUE
  )

  expect_s3_class(res$cancer_cn_en, "factor")
  expect_equal(as.character(res$cancer_cn_en[1]), "唇癌 / Lip cancer")
  expect_equal(as.character(res$cancer_cn_en[2]), "卡波西肉瘤 / Kaposi's Sarcoma")
})
