test_that("labels are used immediately by all label readers", {
  withr::local_options(canregtools.dict_dir = tempfile("label-readers-"))
  labels <- data.frame(
    regicode = c("410103", "IARC-GB-001"),
    cname = c("新名称", "英国"),
    ename = c("New name", "UK registry"),
    abbr_cn = c("新名", "英"),
    abbr_en = c("New", "UK")
  )
  write_regicode(labels)
  expect_identical(
    tidy_var(labels$regicode, "regicode", lang = "cn"),
    labels$cname
  )
  expect_identical(
    tidy_var(labels$regicode, "regicode", lang = "en"),
    labels$ename
  )
  expect_identical(
    add_labels(
      labels["regicode"],
      vars = "regicode",
      as_factor = FALSE
    )$regicode_cn,
    labels$cname
  )
  expect_identical(
    ls_vars("regicode")$cname[match(labels$regicode, ls_vars("regicode")$code)],
    labels$cname
  )
  stored <- readRDS(dict_addr("regicode"))
  expect_equal(stored$overrides, dplyr::as_tibble(labels))
  expect_gt(nrow(ls_dict("regicode")), nrow(labels))
})

test_that("new built-in defaults remain visible without materializing them", {
  withr::local_options(canregtools.dict_dir = tempfile("dict-defaults-"))
  write_registry(c(LOCAL = "custom"), dict = "test")
  expect_identical(readRDS(dict_addr("test"))$overrides, c(LOCAL = "custom"))
  testthat::local_mocked_bindings(registry_dict_defaults = function(dict) {
    c(LOCAL = "built-in", ADDED = "new default")
  })
  expect_identical(
    read_registry_dictionary("test"),
    c(LOCAL = "custom", ADDED = "new default")
  )
})

test_that("legacy files are read unchanged and backed up before migration", {
  withr::local_options(canregtools.dict_dir = tempfile("dict-legacy-"))
  dir.create(dirname(dict_addr("test")))
  legacy <- c(OLD = "preserved", SAME = "old default")
  path <- dict_addr("test")
  saveRDS(legacy, path)
  before <- tools::md5sum(path)
  expect_identical(read_registry_dictionary("test"), legacy)
  expect_identical(tools::md5sum(path), before)
  write_registry(c(NEW = "added"), dict = "test")
  backups <- list.files(dirname(path), pattern = "backup-", full.names = TRUE)
  expect_length(backups, 1L)
  expect_identical(readRDS(backups), legacy)
  expect_identical(readRDS(path)$overrides, c(legacy, NEW = "added"))
  expect_identical(ls_dict_files(), "test_dict.rds")
  write_registry(c(NEW = "changed"), dict = "test")
  expect_length(list.files(dirname(path), pattern = "backup-"), 1L)
})

test_that("legacy label tables preserve labels across migration and reset", {
  withr::local_options(canregtools.dict_dir = tempfile("labels-legacy-"))
  dir.create(dirname(dict_addr("regicode")))
  legacy <- registry_dict_defaults("regicode")
  legacy$cname[1] <- "旧自定义名"
  saveRDS(legacy, dict_addr("regicode"))
  write_regicode()
  expect_equal(readRDS(dict_addr("regicode"))$overrides, legacy)
  expect_identical(
    tidy_var(legacy$regicode[1], "regicode", lang = "cn"),
    "旧自定义名"
  )
  reset_dict("regicode")
  expect_equal(
    read_registry_dictionary("regicode"),
    registry_dict_defaults("regicode")
  )
  expect_equal(nrow(readRDS(dict_addr("regicode"))$overrides), 0L)
  expect_length(
    list.files(dirname(dict_addr("regicode")), pattern = "backup-"),
    2L
  )
})

test_that("project directories isolate dictionaries and reads do not create files", {
  project_a <- tempfile("dict-project-a-")
  project_b <- tempfile("dict-project-b-")
  withr::local_options(canregtools.dict_dir = project_a)
  expect_gt(nrow(ls_dict("registry")), 0L)
  expect_identical(dir.exists(project_a), FALSE)
  expect_length(ls_dict_files(), 0L)
  write_registry(c("410103" = "GROUP-A"))
  expect_identical(classify_areacode("410103", "registry")$registry, "GROUP-A")
  expect_identical(dirname(ls_dict_files(full.names = TRUE)), project_a)
  expect_identical(
    ls_dict_files(with_info = TRUE)$path,
    ls_dict_files(full.names = TRUE)
  )
  options(canregtools.dict_dir = project_b)
  expect_equal(
    ls_dict("registry"),
    registry_dict_table(registry_dict_defaults("registry"), "registry")
  )
  expect_identical(dir.exists(project_b), FALSE)
})

test_that("invalid inputs cannot overwrite valid dictionary files", {
  withr::local_options(canregtools.dict_dir = tempfile("dict-validation-"))
  labels <- registry_dict_defaults("regicode")[1, ]
  write_regicode(labels)
  path <- dict_addr("regicode")
  before <- tools::md5sum(path)
  invalid <- list(
    function() write_regicode(rbind(labels, labels)),
    function() write_regicode(transform(labels, regicode = NA_character_)),
    function() write_regicode(transform(labels, regicode = "  ")),
    function() write_regicode(transform(labels, cname = 1)),
    function() write_registry(c(A = "B"), dict = "regicode"),
    function() write_registry(c(A = "B"), dict = "Regicode"),
    function() write_registry(c(A = "B"), dict = "../outside"),
    function() write_registry(c(A = NA_character_), dict = "test"),
    function() write_registry(list(A = c("one", "two")), dict = "test")
  )
  for (attempt in invalid) {
    err <- tryCatch(attempt(), error = identity)
    expect_s3_class(err, "error")
    expect_identical(tools::md5sum(path), before)
  }
})

test_that("corrupt and incompatible dictionaries are never replaced", {
  withr::local_options(canregtools.dict_dir = tempfile("dict-corrupt-"))
  dir.create(dirname(dict_addr("test")))
  path <- dict_addr("test")
  corrupt <- list(charToRaw("not an RDS file"), NULL)
  for (bytes in corrupt) {
    if (is.null(bytes)) {
      saveRDS(data.frame(wrong = "schema"), path)
    } else {
      writeBin(bytes, path)
    }
    before <- tools::md5sum(path)
    for (refresh in c(FALSE, TRUE)) {
      err <- tryCatch(
        write_registry(c(A = "B"), dict = "test", cache_refresh = refresh),
        error = identity
      )
      expect_s3_class(err, "error")
      expect_match(
        conditionMessage(err),
        "Original file retained",
        fixed = TRUE
      )
      expect_identical(tools::md5sum(path), before)
    }
    expect_identical(dir.exists(paste0(path, ".lock")), FALSE)
  }
})

test_that("writer locks prevent concurrent read-modify-write updates", {
  withr::local_options(canregtools.dict_dir = tempfile("dict-lock-"))
  write_registry(c(A = "one"), dict = "test")
  path <- dict_addr("test")
  before <- tools::md5sum(path)
  lock <- paste0(path, ".lock")
  dir.create(lock)
  err <- tryCatch(write_registry(c(B = "two"), dict = "test"), error = identity)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "locked by another writer", fixed = TRUE)
  expect_identical(tools::md5sum(path), before)
  delete_error <- tryCatch(del_dict_files("test"), error = identity)
  expect_s3_class(delete_error, "error")
  expect_identical(tools::md5sum(path), before)
  unlink(lock, recursive = TRUE)
  write_registry(c(B = "two"), dict = "test")
  expect_identical(read_registry_dictionary("test"), c(A = "one", B = "two"))
  expect_length(
    list.files(dirname(path), all.files = TRUE, pattern = "^\\.dictionary-"),
    0L
  )
})

test_that("explicit resets clear overrides and retain backups", {
  withr::local_options(canregtools.dict_dir = tempfile("dict-reset-"))
  write_registry(c(A = "one"), dict = "test")
  original <- readRDS(dict_addr("test"))
  reset_dict("test")
  expect_identical(read_registry_dictionary("test"), character())
  backup <- list.files(
    dirname(dict_addr("test")),
    pattern = "backup-",
    full.names = TRUE
  )
  expect_identical(readRDS(backup), original)
  write_registry(c(B = "two"), dict = "test", cache_refresh = TRUE)
  expect_identical(read_registry_dictionary("test"), c(B = "two"))
})

test_that("version and dictionary type mismatches cannot be overwritten", {
  withr::local_options(canregtools.dict_dir = tempfile("dict-schema-"))
  write_registry(c(A = "one"), dict = "test")
  path <- dict_addr("test")
  original <- readRDS(path)
  incompatible <- list(
    utils::modifyList(original, list(version = 2L)),
    utils::modifyList(original, list(kind = "labels")),
    utils::modifyList(original, list(dict = "other"))
  )
  for (stored in incompatible) {
    saveRDS(stored, path)
    before <- tools::md5sum(path)
    err <- tryCatch(
      write_registry(c(B = "two"), dict = "test"),
      error = identity
    )
    expect_s3_class(err, "error")
    expect_identical(tools::md5sum(path), before)
  }
})

test_that("area-type defaults and overrides do not depend on file initialization", {
  withr::local_options(canregtools.dict_dir = tempfile("area-type-defaults-"))
  expect_identical(classify_areacode("410307", "area_type")$area_type, "920000")
  expect_identical(file.exists(dict_addr("area_type")), FALSE)
  write_registry(c("410307" = "910000"), dict = "area_type")
  expect_identical(classify_areacode("410307", "area_type")$area_type, "910000")
  reset_dict("area_type")
  expect_identical(classify_areacode("410307", "area_type")$area_type, "920000")
})
