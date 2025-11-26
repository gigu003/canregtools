test_that("draw_dumbbell works with example data", {
  set.seed(123)
  cancer_en <- paste0("Cancer_", LETTERS[1:20])
  asr_lower_cn2000 <- round(runif(20, 30, 80), 1)
  asr_upper_cn2000 <- asr_lower_cn2000 + round(runif(20, 5, 20), 1)
  df <- data.frame(
    cancer_en = cancer_en,
    asr_lower_cn2000 = asr_lower_cn2000,
    asr_upper_cn2000 = asr_upper_cn2000
  )
  
  expect_silent(draw_dumbbell(
    data = df,
    x = cancer_en,
    y1 = asr_lower_cn2000,
    y2 = asr_upper_cn2000,
    topn = 10,
    sort = "desc"
  ))
})