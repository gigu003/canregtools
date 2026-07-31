test_that("create_site_morp always returns year, sex, and cancer strata", {
  sm <- data.frame(
    year = c(2021L, 2021L, 2021L),
    sex = c(1L, 2L, 1L),
    cancer = c("101", "101", "102"),
    stringsAsFactors = FALSE
  )
  sm$icd10 <- I(list(
    c("C15.1" = 2L, "C15.2" = 3L),
    c("C15.3" = 5L),
    c("C16.1" = 7L)
  ))
  sm$morp <- I(list(
    c("8140" = 2L),
    c("8140" = 5L),
    c("8000" = 7L)
  ))
  fbsw <- structure(list(sitemorp = sm), class = c("fbswicd", "list"))

  by_year <- create_site_morp(fbsw, year, class_morp = FALSE)
  expect_contains(names(by_year), c("year", "sex", "cancer", "icd10", "count"))
  expect_equal(intersect(by_year$year, 9000L), integer())
  expect_equal(unique(by_year$sex), 0L)
  expect_equal(unique(by_year$cancer), "60")

  by_year_sex <- create_site_morp(fbsw, year, sex, class_morp = FALSE)
  expect_contains(
    names(by_year_sex),
    c("year", "sex", "cancer", "icd10", "count")
  )
  expect_equal(intersect(by_year_sex$year, 9000L), integer())
  expect_equal(sort(unique(by_year_sex$sex)), c(0L, 1L, 2L))
  expect_equal(unique(by_year_sex$cancer), "60")

  by_year_cancer <- create_site_morp(fbsw, year, cancer, class_morp = FALSE)
  expect_contains(
    names(by_year_cancer),
    c("year", "sex", "cancer", "icd10", "count")
  )
  expect_equal(intersect(by_year_cancer$year, 9000L), integer())
  expect_equal(unique(by_year_cancer$sex), 0L)
  expect_equal(sort(unique(by_year_cancer$cancer)), c("101", "102", "60"))

  by_sex_cancer <- create_site_morp(fbsw, sex, cancer, class_morp = FALSE)
  expect_equal(unique(by_sex_cancer$year), 9000L)
  expect_equal(sort(unique(by_sex_cancer$sex)), c(0L, 1L, 2L))
  expect_equal(sort(unique(by_sex_cancer$cancer)), c("101", "102", "60"))
  expect_equal(
    dplyr::filter(
      by_sex_cancer,
      sex == 0L,
      cancer == "60",
      icd10 == "C15"
    )$count,
    10L
  )
})

test_that("create_site_morp collapses ICD-10 to three characters for selected cancers", {
  sm <- data.frame(
    year = c(2021L, 2021L, 2021L),
    sex = c(1L, 1L, 1L),
    cancer = c("101", "126", "102"),
    stringsAsFactors = FALSE
  )
  sm$icd10 <- I(list(
    c("C15.1" = 2L, "C15.2" = 3L, "C16.1" = 4L),
    c("C18.1" = 5L, "C18.2" = 6L),
    c("C18.1" = 5L, "C18.2" = 6L)
  ))
  sm$morp <- I(list(
    c("8140" = 9L),
    c("8000" = 11L),
    c("8000" = 11L)
  ))
  fbsw <- structure(list(sitemorp = sm), class = c("fbswicd", "list"))

  res <- create_site_morp(fbsw, year, sex, cancer, class_morp = FALSE)
  res_101 <- dplyr::filter(res, sex == 1L, cancer == "101")
  expect_equal(dplyr::filter(res_101, icd10 == "C15")$count, 5L)
  expect_equal(dplyr::filter(res_101, icd10 == "C16")$count, 4L)
  expect_equal(grep("\\.", res_101$icd10, value = TRUE), character())

  res_126 <- dplyr::filter(res, sex == 1L, cancer == "126")
  expect_equal(dplyr::filter(res_126, icd10 == "C18")$count, 11L)
  expect_equal(grep("\\.", res_126$icd10, value = TRUE), character())

  res_102 <- dplyr::filter(res, sex == 1L, cancer == "102")
  expect_setequal(
    intersect(c("C18.1", "C18.2"), res_102$icd10),
    c("C18.1", "C18.2")
  )
})

test_that("create_site_morp normalises special and incomplete ICD-10 codes", {
  sm <- data.frame(
    year = c(2021L, 2021L),
    sex = c(1L, 1L),
    cancer = c("102", "102"),
    stringsAsFactors = FALSE
  )
  sm$icd10 <- I(list(
    c("C15" = 2L, "C15.1" = 3L, "C80" = 4L, "C80.9" = 5L),
    c("D24" = 6L, "D24.0" = 7L)
  ))
  sm$morp <- I(list(
    c("8140" = 14L),
    c("8000" = 13L)
  ))
  fbsw <- structure(list(sitemorp = sm), class = c("fbswicd", "list"))

  res <- create_site_morp(fbsw, year, sex, cancer, class_morp = FALSE)

  res_102 <- dplyr::filter(res, year == 2021L, sex == 1L, cancer == "102")
  expect_equal(dplyr::filter(res_102, icd10 == "C15.9")$count, 2L)
  expect_equal(dplyr::filter(res_102, icd10 == "C15.1")$count, 3L)
  expect_equal(dplyr::filter(res_102, icd10 == "C80")$count, 9L)
  expect_equal(dplyr::filter(res_102, icd10 == "D24")$count, 13L)
  expect_equal(intersect(res$icd10, c("C80.9", "D24.0")), character())
})
