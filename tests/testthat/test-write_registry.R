test_that("write_registry.character writes and returns tibble", {
  tmp_dict <- c("410302" = "410301", "410303" = "410301")
  dict <- write_registry(tmp_dict, dict = "registry")
  expect_equal(dict, NULL)
})

test_that("write_registry.list works correctly", {
  tmp_list <- list("410302" = "410301", "410303" = "410301")
  dict <- write_registry(tmp_list, dict = "registry", quiet = FALSE)
  expect_equal(unname(dict$value[dict$areacode == "410302"]), "410301")
})

test_that("write_registry.data.frame works with proper columns", {
  df <- data.frame(
    areacode = c("410302", "410303"),
    registry = c("410301", "410301"),
    area_type = c("urban", "urban")
  )
  write_registry(df, dict = "registry")
  dict <- ls_dict(dict = "registry")
  expect_true("410302" %in% dict$areacode)
})

test_that("write_registry.NULL writes default dict", {
  dict <- write_registry(NULL, dict = "registry")
  expect_s3_class(dict, NA)
})

test_that("write_registry throws error on invalid dict_type", {
  x <- c("410302" = "410301")
  expect_error(write_registry(x, dict = "invalid_type"))
})

test_that("write_registry throws error for unnamed character vector", {
  x <- c("410301", "410301")
  expect_error(write_registry(x, dict = "registry"))
})
