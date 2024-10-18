# Define test cases for tidy_sex function
test_that("tidy_sex returns expected results", {
  
  # Test case 1: Check default language ('cn')
  gender_default <- c("male", "men", "women", "female",
                      "women", "man", "1", "2", "0")
  expect_result_default <- factor(c(1, 1, 2, 2, 2, 1, 1, 2, 0),
                                  levels = c(0, 1, 2),
                                  labels = c("\u5408\u8ba1", "\u7537\u6027",
                                             "\u5973\u6027"))
  expect_equal(tidy_sex(gender_default,as_factor = TRUE), expect_result_default)
  
  # Test case 2: Check language 'en'
  expect_result_en <- factor(c(1, 1, 2, 2, 2, 1, 1, 2, 0),
                             levels = c(0, 1, 2),
                             labels = c("Total", "Male", "Female"))
  expect_equal(tidy_sex(gender_default, lang = "en",as_factor = TRUE), expect_result_en)
  
  # Test case 3: Check unsupported language
  expect_unsupported <- factor(c(1, 1, 2, 2, 2, 1, 1, 2, 0),
                              levels = c(0, 1, 2),
                              labels = c("\u5408\u8ba1", "\u7537\u6027",
                                         "\u5973\u6027"))
  expect_warning(tidy_sex(gender_default, lang = "Unsupported language",
                          as_factor = TRUE))
  expect_equal(tidy_sex(gender_default, as_factor = TRUE), expect_unsupported)
  
})