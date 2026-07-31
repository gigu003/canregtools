test_that("add_labels creates factor labels for selected variables", {
  data <- data.frame(
    sex = c(1L, 2L),
    cancer = c("101", "102")
  )

  res <- add_labels(data, vars = c("sex", "cancer"), lang = c("cn", "en"))

  expect_s3_class(res$sex_cn_en, "factor")
  expect_s3_class(res$cancer_cn_en, "factor")
  expect_contains(names(res), c("sex_cn_en", "cancer_cn_en"))
  expect_equal(as.character(res$sex_cn_en), c("男性 Male", "女性 Female"))
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
  expect_equal(
    as.character(res$cancer_cn_en[2]),
    "卡波西肉瘤 / Kaposi's Sarcoma"
  )
})

test_that("add_labels falls back to default names when custom names mismatch", {
  data <- data.frame(sex = c(1L, 2L), cancer = c("101", "102"))

  expect_snapshot({
    res <- add_labels(data, vars = c("sex", "cancer"), names = "bad_name")
  })
  expect_contains(names(res), c("sex_cn", "cancer_cn"))
})
