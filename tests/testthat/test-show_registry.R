test_that("show_registry returns correct areacodes for valid inputs", {
  # Numeric and character input should give same results
  expect_equal(
    show_registry(c(1, 2)), 
    show_registry(c("1", "2"))
  )
  
  # Invalid type should return character(0)
  expect_equal(
    show_registry(9), 
    character(0)
  )
  
  # Partial match
  expect_equal(
    show_registry("3"), 
    "411025"
  )
})