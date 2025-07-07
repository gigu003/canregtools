test_that("classify_childhood returns correct length and type", {
  topo <- c("C15.2", "C16.2", "C34.2")
  morp <- c("8000", "8040", "8170")
  beha <- c(3, 3, 3)
  
  main_group <- classify_childhood(topo, morp, beha, type = "main")
  sub_group <- classify_childhood(topo, morp, beha, type = "sub")
  
  expect_type(main_group, "double")
  expect_type(sub_group, "character")
  expect_length(main_group, 3)
  expect_length(sub_group, 3)
  expect_true(all(main_group >= 1 & main_group <= 12 | main_group == 999))
})