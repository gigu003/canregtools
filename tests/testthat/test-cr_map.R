test_that("cr_map accepts all supported input forms and preserves compatibility", {
  withr::local_options(canregtools.dict_dir = tempfile("cr-map-inputs-"))
  mapping <- c("410103" = "A", "410307" = "B")
  vector <- cr_map(mapping, attr = "custom")
  frame <- cr_map(
    data.frame(regicode = names(mapping), custom = unname(mapping)),
    attr = "custom"
  )
  listed <- cr_map(as.list(mapping), attr = "custom")
  expect_equal(vector, frame)
  expect_equal(listed, frame)
  expect_equal(write_registry(mapping, dict = "custom"), frame)
  expect_equal(ls_dict("custom"), frame)
  expect_identical(withVisible(cr_map(mapping, attr = "custom"))$visible, FALSE)
})

test_that("cr_map mappings control cr_reframe for raw and counted registries", {
  withr::local_options(canregtools.dict_dir = tempfile("cr-map-reframe-"))
  mapping <- data.frame(regicode = names(canregs), custom = c("A", "A", "B"))
  cr_map(mapping, attr = "custom")
  raw <- cr_reframe(canregs, strat = "custom")
  counted <- cr_reframe(fbsws, strat = "custom")
  expect_named(raw, c("A", "B"))
  expect_named(counted, c("A", "B"))
  expect_equal(
    nrow(raw$A$FBcases),
    sum(vapply(canregs[1:2], function(x) nrow(x$FBcases), integer(1)))
  )
  expect_equal(
    sum(counted$A$fbswicd$fbs),
    sum(vapply(fbsws[1:2], function(x) sum(x$fbswicd$fbs), numeric(1)))
  )
})

test_that("cr_map user overrides take precedence and refresh clears earlier edits", {
  withr::local_options(canregtools.dict_dir = tempfile("cr-map-priority-"))
  cr_map(c("410307" = "910000"), attr = "area_type")
  expect_identical(classify_areacode("410307", "area_type")$area_type, "910000")
  cr_map(c("410103" = "920000"), attr = "area_type", cache_refresh = TRUE)
  expect_identical(
    classify_areacode(c("410307", "410103"), "area_type")$area_type,
    c("920000", "920000")
  )
  expect_length(
    list.files(getOption("canregtools.dict_dir"), pattern = "backup-"),
    1L
  )
})

test_that("cr_map rejects invalid and derived attributes without writing", {
  withr::local_options(canregtools.dict_dir = tempfile("cr-map-invalid-"))
  expect_snapshot(
    error = TRUE,
    cr_map(data.frame(regicode = "410103"), attr = "custom")
  )
  expect_snapshot(
    error = TRUE,
    cr_map(c("410103" = "A", "410103" = "B"), attr = "custom")
  )
  expect_snapshot(error = TRUE, cr_map(c("410103" = "A"), attr = "province"))
  expect_length(ls_dict_files(), 0L)
})

test_that("cr_map batches selected columns and returns effective dictionaries", {
  withr::local_options(canregtools.dict_dir = tempfile("cr-map-batch-"))
  mapping <- data.frame(
    regicode = names(canregs),
    area_type = c("910000", "910000", "920000"),
    registry = c("410100", "410300", "411700"),
    hdi = c("high", "high", "low"),
    unused = NA_character_
  )
  attrs <- c("area_type", "registry", "hdi")
  result <- cr_map(mapping, attr = attrs)
  expect_named(result, attrs)
  for (attr in attrs) {
    expect_equal(result[[attr]], ls_dict(attr))
    expect_identical(
      result[[attr]]$value[match(mapping$regicode, result[[attr]]$regicode)],
      mapping[[attr]]
    )
  }
  expect_identical(withVisible(cr_map(mapping, attr = attrs))$visible, FALSE)
  expect_named(cr_reframe(canregs, strat = "hdi"), c("high", "low"))
  expect_length(ls_dict_files(), 3L)
  cr_map(mapping[1, ], attr = attrs, cache_refresh = TRUE)
  expect_identical(ls_dict("hdi")$regicode, mapping$regicode[1])
  expect_length(
    list.files(getOption("canregtools.dict_dir"), pattern = "backup-"),
    3L
  )
})

test_that("batch validation does not change earlier valid targets", {
  withr::local_options(canregtools.dict_dir = tempfile("cr-map-batch-invalid-"))
  cr_map(c("410103" = "old"), attr = "first")
  path <- dict_addr("first")
  before <- tools::md5sum(path)
  mapping <- data.frame(regicode = "410103", first = "new", second = "value")
  attempts <- list(
    function() cr_map(mapping[1:2], attr = c("first", "second")),
    function() {
      cr_map(
        transform(mapping, second = NA_character_),
        attr = c("first", "second")
      )
    },
    function() cr_map(mapping, attr = c("first", "first")),
    function() cr_map(mapping, attr = c("first", "FIRST")),
    function() cr_map(mapping, attr = c("first", "province")),
    function() cr_map(c("410103" = "new"), attr = c("first", "second")),
    function() cr_map(mapping, attr = c("first", "second"), quiet = NA),
    function() cr_map(mapping, attr = character())
  )
  for (attempt in attempts) {
    expect_s3_class(tryCatch(attempt(), error = identity), "error")
    expect_identical(tools::md5sum(path), before)
    expect_identical(file.exists(dict_addr("second")), FALSE)
  }
})

test_that("batch preflight protects earlier targets from corrupt files and locks", {
  withr::local_options(
    canregtools.dict_dir = tempfile("cr-map-batch-preflight-")
  )
  mapping <- data.frame(regicode = "410103", first = "new", second = "new")
  cr_map(c("410103" = "old"), attr = "first")
  paths <- vapply(c("first", "second"), dict_addr, character(1))
  writeBin(charToRaw("corrupt"), paths[2])
  before <- tools::md5sum(paths)
  err <- tryCatch(
    cr_map(mapping, attr = c("first", "second")),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_identical(tools::md5sum(paths), before)
  expect_identical(dir.exists(paste0(paths, ".lock")), c(FALSE, FALSE))
  dir.create(paste0(paths[2], ".lock"))
  err <- tryCatch(
    cr_map(mapping, attr = c("first", "second")),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_identical(tools::md5sum(paths), before)
  expect_identical(dir.exists(paste0(paths, ".lock")), c(FALSE, TRUE))
})

test_that("batch write errors restore original bytes and remove new dictionaries", {
  withr::local_options(
    canregtools.dict_dir = tempfile("cr-map-batch-rollback-")
  )
  mapping <- data.frame(
    regicode = "410103",
    first = "new",
    second = "new",
    third = "new"
  )
  cr_map(c("410103" = "old"), attr = "first")
  first_path <- dict_addr("first")
  before <- tools::md5sum(first_path)
  real_save <- save_registry_dictionary
  testthat::local_mocked_bindings(save_registry_dictionary = function(
    value,
    path
  ) {
    real_save(value, path)
    if (value$dict == "second") stop("simulated write error")
  })
  err <- tryCatch(
    cr_map(mapping, attr = c("first", "second", "third")),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_identical(conditionMessage(err), "simulated write error")
  expect_identical(tools::md5sum(first_path), before)
  expect_identical(ls_dict_files(), "first_dict.rds")
  expect_length(
    list.files(
      getOption("canregtools.dict_dir"),
      pattern = "^\\.dictionary-",
      all.files = TRUE
    ),
    0L
  )
  expect_length(
    list.files(getOption("canregtools.dict_dir"), pattern = "\\.lock$"),
    0L
  )
})
