test_that("write_regicode stores arbitrary registry identifiers", {
  on.exit(del_dict_files("regicode"), add = TRUE)

  write_regicode(
    data.frame(
      regicode = "IARC-GB-001",
      cname = "英国登记处",
      ename = "United Kingdom Registry",
      abbr_cn = "英国",
      abbr_en = "UK",
      stringsAsFactors = FALSE
    ),
    cache_refresh = TRUE
  )

  labels <- ls_dict("regicode")
  expect_equal(
    labels$ename[labels$regicode == "IARC-GB-001"],
    "United Kingdom Registry"
  )
})
