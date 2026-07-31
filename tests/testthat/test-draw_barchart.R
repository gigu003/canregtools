test_that("basic barchart works without group and facet", {
  cancers <- c(
    "Lung",
    "Breast",
    "Colorectal",
    "Prostate",
    "Stomach",
    "Liver",
    "Pancreas",
    "Bladder",
    "Skin",
    "Thyroid"
  )
  years <- c(2015, 2020)
  sexes <- c("Male", "Female")
  cr_data <- expand.grid(
    cancer = cancers,
    year = years,
    sex = sexes
  )
  cr_data$cr <- seq_len(nrow(cr_data))

  result <- with_test_pdf_device(
    draw_barchart(
      cr_data,
      x = cancer,
      y = cr,
      group = sex,
      facet = year
    )
  )

  expect_type(result, "list")
  expect_named(result, "mfrow")
  expect_equal(result$mfrow, c(1L, 1L))
})

test_that("draw_barchart supports one-panel charts without group or facet", {
  data <- data.frame(
    cancer = c("Lung", "Breast", "Liver"),
    cr = c(10, 20, 15)
  )

  result <- with_test_pdf_device(draw_barchart(
    data,
    x = cancer,
    y = cr,
    topn = 2
  ))

  expect_type(result, "list")
  expect_equal(result$mfrow, c(1L, 1L))
})
