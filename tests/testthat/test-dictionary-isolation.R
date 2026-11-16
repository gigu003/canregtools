test_that("all test dictionaries are stored in the temporary directory", {
  expected <- normalizePath(
    test_dictionary_dir,
    winslash = "/",
    mustWork = TRUE
  )
  expect_identical(
    normalizePath(
      dirname(dict_addr("regicode")),
      winslash = "/",
      mustWork = TRUE
    ),
    expected
  )
  expect_identical(
    normalizePath(
      dirname(dict_addr("registry")),
      winslash = "/",
      mustWork = TRUE
    ),
    expected
  )
  temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  expect_identical(startsWith(expected, paste0(temp_root, "/")), TRUE)
})
