# Define test cases for tidy_sex function
test_that("tidy_sex returns expected results", {
  
  # Test case 1: Check default language ('cn')
  gender_default <- c("male", "men", "women", "female",
                      "women", "man", "1", "2")
  expect_result_default <- factor(c(1, 1, 2, 2, 2, 1, 1, 2),
                                  levels = c(1, 2),
                                  labels = c("\u7537\u6027", "\u5973\u6027"))
  expect_equal(tidy_sex(gender_default), expect_result_default)
  
  # Test case 2: Check language 'en'
  expect_result_en <- factor(c(1, 1, 2, 2, 2, 1, 1, 2),
                             levels = c(1, 2),
                             labels = c("Male", "Female"))
  expect_equal(tidy_sex(gender_default, lang = "en"), expect_result_en)
  
  # Test case 3: Check unsupported language
  expect_unsupported <- factor(c(1, 1, 2, 2, 2, 1, 1, 2),
                              levels = c(1, 2),
                              labels = c("\u7537\u6027", "\u5973\u6027"))
  expect_warning(tidy_sex(gender_default, lang = "Unsupported language"))
  expect_equal(tidy_sex(gender_default), expect_unsupported)
  
})