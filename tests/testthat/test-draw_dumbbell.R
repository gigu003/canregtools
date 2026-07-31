test_that("draw_dumbbell works with example data", {
  cancer_en <- paste0("Cancer_", LETTERS[1:20])
  asr_lower_cn2000 <- seq(30, 68, by = 2)
  asr_upper_cn2000 <- asr_lower_cn2000 + 10
  df <- data.frame(
    cancer_en = cancer_en,
    asr_lower_cn2000 = asr_lower_cn2000,
    asr_upper_cn2000 = asr_upper_cn2000
  )

  result <- with_test_pdf_device(
    draw_dumbbell(
      data = df,
      x = cancer_en,
      y1 = asr_lower_cn2000,
      y2 = asr_upper_cn2000,
      topn = 10,
      sort = "desc"
    )
  )

  expect_null(result)
})

test_that("draw_dumbbell supports legends and grid lines", {
  df <- data.frame(
    cancer = c("A", "B", "C"),
    lower = c(1, 3, 2),
    upper = c(2, 5, 4)
  )

  result <- with_test_pdf_device(
    draw_dumbbell(
      df,
      x = cancer,
      y1 = lower,
      y2 = upper,
      legend = c("Lower", "Gap", "Upper"),
      gl = 2
    )
  )

  expect_type(result, "list")
})
