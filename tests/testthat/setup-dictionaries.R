test_dictionary_dir <- tempfile("canregtools-test-dictionaries-")
dir.create(test_dictionary_dir)
test_dictionary_options <- options(canregtools.dict_dir = test_dictionary_dir)
withr::defer(
  {
    options(test_dictionary_options)
    unlink(test_dictionary_dir, recursive = TRUE)
  },
  envir = teardown_env()
)
