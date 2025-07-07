test_that("create_report handles invalid template correctly", {
  mock_data <- list(FBcases = data.frame(), SWcases = data.frame(), POP = data.frame(), areacode = "test")
  class(mock_data) <- "canreg"
  
  expect_error(
    create_report(mock_data, template = "nonexistent_template"),
    "Template not found"
  )
})


