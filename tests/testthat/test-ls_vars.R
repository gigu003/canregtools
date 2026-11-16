test_that("ls_attrs lists built-in reframe attributes", {
  expect_equal(
    ls_attrs(),
    c("registry", "region", "area_type", "province", "city")
  )
})

test_that("ls_vars returns documented variable dictionaries", {
  res <- ls_vars("reframe")

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("code", "cname", "ename", "abbr_cn", "abbr_en"))
  expect_contains(res$code, ls_attrs())
})

test_that("ls_vars lists all standard population labels", {
  res <- ls_vars("std")

  expect_identical(
    res$code,
    c(
      "cn64",
      "cn82",
      "cn2000",
      "wld85",
      "wld2000",
      "eur76",
      "eur2013",
      "usa2000"
    )
  )
  expect_identical(
    res$abbr_en,
    c(
      "CN64",
      "CN82",
      "CN2000",
      "WLD85",
      "WLD2000",
      "EUR76",
      "EUR2013",
      "USA2000"
    )
  )
})

test_that("ls_vars validates dictionary type", {
  expect_snapshot(error = TRUE, {
    ls_vars("not_a_type")
  })
})

test_that("reframe variables include custom mappings without duplicate built-ins", {
  withr::local_options(canregtools.dict_dir = tempfile("reframe-vars-"))
  before <- ls_vars("reframe")
  mapping <- data.frame(
    regicode = "410103",
    hdi = "high",
    study_group = "A",
    area_type = "910000"
  )
  cr_map(mapping, attr = c("hdi", "study_group", "area_type"))
  write_regicode()
  result <- ls_vars("reframe")
  expect_equal(result[seq_len(nrow(before)), ], before)
  expect_setequal(setdiff(result$code, before$code), c("hdi", "study_group"))
  expect_identical(anyDuplicated(result$code), 0L)
  expect_equal(result$cname[result$code == "hdi"], "hdi")
  expect_equal(result$ename[result$code == "hdi"], "hdi")
  expect_identical(result$code[result$code == "regicode"], "regicode")
  expect_identical(sum(result$code == "area_type"), 1L)
})

test_that("reframe discovery follows project changes and deletions without writing", {
  project_a <- tempfile("reframe-project-a-")
  project_b <- tempfile("reframe-project-b-")
  withr::local_options(canregtools.dict_dir = project_a)
  cr_map(c("410103" = "high"), attr = "hdi")
  expect_contains(ls_vars("reframe")$code, "hdi")
  files <- ls_dict_files(full.names = TRUE)
  before <- tools::md5sum(files)
  ls_vars("reframe")
  expect_identical(tools::md5sum(files), before)
  options(canregtools.dict_dir = project_b)
  expect_identical("hdi" %in% ls_vars("reframe")$code, FALSE)
  expect_identical(dir.exists(project_b), FALSE)
  options(canregtools.dict_dir = project_a)
  del_dict_files("hdi")
  expect_identical("hdi" %in% ls_vars("reframe")$code, FALSE)
})

test_that("legacy grouping discovery does not migrate or include backups", {
  withr::local_options(canregtools.dict_dir = tempfile("reframe-legacy-"))
  dir.create(getOption("canregtools.dict_dir"))
  path <- dict_addr("legacy_group")
  saveRDS(c("410103" = "A"), path)
  saveRDS(c("410103" = "B"), paste0(path, ".backup-test"))
  before <- tools::md5sum(path)
  expect_contains(ls_vars("reframe")$code, "legacy_group")
  expect_identical(tools::md5sum(path), before)
  expect_identical(sum(ls_vars("reframe")$code == "legacy_group"), 1L)
  writeBin(charToRaw("corrupt"), dict_addr("broken"))
  expect_snapshot({
    result <- ls_vars("reframe")
  })
  expect_identical("broken" %in% result$code, FALSE)
})

test_that("custom grouping names do not shadow summary selection metrics", {
  withr::local_options(canregtools.dict_dir = tempfile("reframe-metric-name-"))
  expected <- cr_select(canregs, mi > 0.5)
  cr_map(c("410103" = "custom"), attr = "mi")
  expect_contains(ls_vars("reframe")$code, "mi")
  expect_equal(cr_select(canregs, mi > 0.5), expected)
})
