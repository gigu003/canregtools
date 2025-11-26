test_that("basic barchart works without group and facet", {
  set.seed(123)
  cancers <- c("Lung", "Breast", "Colorectal", "Prostate", "Stomach",
               "Liver", "Pancreas", "Bladder", "Skin", "Thyroid")
  years <- c(2015, 2020)
  sexes <- c("Male", "Female")
  cr_data <- expand.grid(
    cancer = cancers,
    year = years,
    sex = sexes
  )
  cr_data$cr <- round(runif(nrow(cr_data), min = 10, max = 100), 1)
  expect_silent(draw_barchart(cr_data, x = cancer, y = cr, group = sex,
                              facet = year))
})
