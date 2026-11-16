test_that("all test dictionaries are stored in the temporary directory", {
  expect_identical(dirname(dict_addr("regicode")), test_dictionary_dir)
  expect_identical(dirname(dict_addr("registry")), test_dictionary_dir)
  expect_equal(startsWith(test_dictionary_dir, tempdir()), TRUE)
})
