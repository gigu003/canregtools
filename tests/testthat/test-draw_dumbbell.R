test_that("draw_dumbbell works with example data", {
  data("canregs", package = "canregtools")
  asr <- create_asr(canregs[[1]], year, cancer, show_ci = TRUE) |>
    drop_others() |>
    drop_total() |>
    add_labels(lang = "en", label_type = "abbr")
  
  expect_silent(draw_dumbbell(
    data = asr,
    x = site,
    y1 = asr_lower_cn2000,
    y2 = asr_upper_cn2000,
    topn = 10,
    sort = "desc"
  ))
})