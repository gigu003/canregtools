test_that("write_registry handles named lists and data frames", {
  list_dict <- paste0("test_write_registry_list_", Sys.getpid())
  frame_dict <- paste0("test_write_registry_frame_", Sys.getpid())
  list_path <- canregtools:::dict_addr(list_dict)
  frame_path <- canregtools:::dict_addr(frame_dict)
  on.exit(unlink(c(list_path, frame_path)), add = TRUE)

  list_result <- write_registry(
    list(REG_A = "group_a", REG_B = "group_b"),
    dict = list_dict
  )
  expect_equal(
    list_result[list_result$regicode == "REG_A", "value"][[1]],
    "group_a"
  )

  frame <- setNames(
    data.frame(regicode = "REG_C", value = "group_c"),
    c("regicode", frame_dict)
  )
  frame_result <- write_registry(frame, dict = frame_dict)
  expect_equal(frame_result$regicode, "REG_C")
  expect_equal(frame_result$value, "group_c")
})

test_that("write_registry refreshes built-in defaults before applying updates", {
  dict <- canregtools:::dict_maps[["china_regicode_registry"]]
  dict_file <- canregtools:::dict_addr("registry")
  had_file <- file.exists(dict_file)
  old_dict <- if (had_file) readRDS(dict_file) else NULL
  on.exit(
    if (had_file) {
      saveRDS(old_dict, dict_file)
    } else {
      unlink(dict_file)
    },
    add = TRUE
  )

  custom_code <- paste0("TEST-", Sys.getpid())
  result <- write_registry(
    setNames("custom", custom_code),
    dict = "registry",
    cache_refresh = TRUE
  )

  default_code <- names(dict)[[1]]
  expect_equal(
    result$value[match(default_code, result$regicode)],
    unname(dict[[1]])
  )
  expect_equal(
    result$value[match(custom_code, result$regicode)],
    "custom"
  )
})

test_that("write_registry reports invalid inputs clearly", {
  expect_snapshot(error = TRUE, {
    write_registry(data.frame(regicode = "REG_A"), dict = "registry")
  })

  expect_snapshot(error = TRUE, {
    write_registry(c(REG_A = "one", REG_A = "two"), dict = "test")
  })

  expect_snapshot(error = TRUE, {
    write_registry(1, dict = "test")
  })
})
