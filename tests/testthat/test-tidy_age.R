# Define test cases for tidy_age function
test_that("tidy_age returns expected results", {
  # Test case 1: Check default unit ('year')
  agedes <- c("50岁3月", "19岁25天", "1岁30天", "3岁1月", "60天")
  expect_result <- c(50, 19, 1, 3, 0)
  expect_equal(tidy_age(agedes), expect_result)

  # Test case 2: Check unit 'month'
  expect_result_month <- c(602, 228, 12, 36, 1)
  expect_equal(tidy_age(agedes, unit = "month"), expect_result_month)

  # Test case 3: Check unit 'day'
  expect_result_day <- c(18354, 6965, 395, 1126, 60)
  expect_equal(tidy_age(agedes, unit = "day"), expect_result_day)
})
