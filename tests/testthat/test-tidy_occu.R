# Define test cases for tidy_occu function
test_that("tidy_occu returns expected results", {
  
  # Test case 1: Check default language ('cn')
  
  occupations <- c("国家公务员", "专业技术人员", "职员", "企业管理人员",
                   "工人", "农民", "学生", "现役军人", "自由职业者",
                   "个体经营者", "无业人员", "退（离）休人员")
  expect_result <- factor(c(11, 13, 17, 21, 24, 27, 31,
                            37, 51, 54,70, 80),
                          levels = occu_map$code,
                          labels = occu_map$cname)
  expect_equal(tidy_occu(occupations), expect_result)
  
  # Test case 2: Check language 'eng'
  expect_result_eng <- factor(c(11, 13, 17, 21, 24, 27, 31,
                            37, 51, 54,70, 80),
                          levels = occu_map$code,
                          labels = occu_map$ename)
  expect_equal(tidy_occu(occupations, lang = "eng"), expect_result_eng)
  
})