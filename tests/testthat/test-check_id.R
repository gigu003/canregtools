test_that("check_id returns expected results", {
  
  # Set a Chinese ID number.
  ids <- c("412726840724123", "110101199003070975",
           "310101199001010101", "440101199102030303","412726198407240038")
  
  # Test case 1: Check logical result
  ids_logical <- c(FALSE, FALSE, FALSE, FALSE, TRUE)
  expect_equal(check_id(ids), ids_logical)
  
  # Test case 2: Check date result
  expect_date <- as.Date(c("1984-07-24","1990-03-07", "1990-01-01",
                           "1991-02-03", "1984-07-24"))
  expect_equal(check_id(ids, return = "date"), expect_date)
  
  # Test case 3: Check areacode result
  expect_areacode <- c("412726", "110101", "310101", "440101", "412726")
  expect_equal(check_id(ids, return = "areacode"), expect_areacode)
  
  # Test case 4: Check formatted result
  expect_formatted <- c("412726198407241233", NA, NA, NA,
                        "412726198407240038")
  expect_equal(check_id(ids, return = "formatted"), expect_formatted)
  
})