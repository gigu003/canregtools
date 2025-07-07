test_that("draw_pyramid", {
  data("canregs", package = "canregtools")
  pop <- canregs[[1]]$POP
  expect_silent(draw_pyramid(pop, agegrp, rks, sex, cgap = 0.2))
})


