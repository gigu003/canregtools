#' Write registry attributes to a user-defined dictionary
#'
#' Compatibility interface for [cr_map()], using `dict` instead of `attr`.
#' New grouping workflows can use `cr_map()` directly.
#'
#' Stores mappings from registry identifiers to their attributes, e.g.,
#' China-specific registry names or area types, in a dictionary file. This supports
#' consistent labeling and downstream processing in cancer registry data.
#'
#' @param x A named list, a named character vector or a data frame that
#'   includes the columns `regicode` and the target attribute
#'   e.g., `registry`, `area_type`.
#' @param dict A character string specifying the dictionary to update.
#'   Built-in defaults are available for "registry" and "area_type"; any other
#'   name is treated as a user-defined dictionary name. The name `regicode` is
#'   reserved for [write_regicode()].
#' @param cache_refresh If TRUE, refresh the dictionary to default values before
#'   updating. User-supplied mappings are then applied on top of those defaults.
#' @param quiet If TRUE, suppresses update and backup messages and returns
#'   the updated dictionary invisibly.
#' @return A tibble with two columns: `regicode` and the corresponding
#'   attribute values. The result is returned invisibly when `quiet = TRUE`.
#'
#' @details
#' This function allows users to build or update local mapping dictionaries for
#' registry-level attributes. It supports multiple input formats and updates
#' files saved in the user-specific R configuration directory. Registry
#' identifiers must be non-empty and unique within one update.
#' Set `options(canregtools.dict_dir = "path/to/dictionaries")` to isolate
#' dictionaries for a project. By default the user configuration directory is
#' used. Only user overrides are stored; current built-in defaults are merged
#' when reading. Legacy dictionaries are backed up and migrated on their next
#' update, retaining all legacy entries as overrides because their origin is
#' unknown. Corrupt files stop the update and are retained. Writes use a
#' verified temporary file and replacement, with a lock against concurrent
#' writers. Use [reset_dict()] to clear overrides (with a backup).
#' `cache_refresh = TRUE` retains its reset-before-update behavior.
#'
#' @export
#' @examples
#' withr::with_options(
#'   list(canregtools.dict_dir = tempfile("canregtools-example-")),
#'   write_registry(list("410302" = "410301"))
#' )
#'
write_registry <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
) {
  UseMethod("write_registry", x)
}

#' @rdname write_registry
#' @method write_registry default
#' @export
write_registry.default <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
) {
  stop(
    "`x` must be NULL, a named list, a named character vector, or a data frame.",
    call. = FALSE
  )
}

#' @rdname write_registry
#' @method write_registry data.frame
#' @export
#' @examples
#' # Registry attributes stored in data frame
#' registry_dict <- data.frame(
#' regicode = c("410302", "410303", "410304", "410305", "410306", "410307"),
#' registry = c(rep("410301", 5), "410300"),
#' area_type = rep("urban", 6)
#' )
#' withr::with_options(
#'   list(canregtools.dict_dir = tempfile("canregtools-example-")),
#'   write_registry(registry_dict)
#' )
#'
write_registry.data.frame <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
) {
  validate_registry_dict_name(dict)
  required <- c("regicode", dict)
  if (!all(required %in% names(x))) {
    stop(
      "`x` must contain columns ",
      paste0("`", required, "`", collapse = " and "),
      ".",
      call. = FALSE
    )
  }

  res <- as.character(x[[dict]])
  names(res) <- as.character(x[["regicode"]])
  write_registry.character(
    res,
    dict = dict,
    cache_refresh = cache_refresh,
    quiet = quiet
  )
}

#' @rdname write_registry
#' @method write_registry list
#' @export
#' @examples
#' # Registry attributes stored in list
#' dict <- list(
#' '410302' = '410301',
#' '410303' = '410301',
#' '410304' = '410301',
#' '410305' = '410301',
#' '410306' = '410301',
#' '410307' = '410301'
#' )
#' withr::with_options(
#'   list(canregtools.dict_dir = tempfile("canregtools-example-")),
#'   write_registry(dict)
#' )
#'
write_registry.list <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
) {
  if (is.null(names(x))) {
    stop("`x` must be a named list.", call. = FALSE)
  }

  if (
    !all(vapply(
      x,
      function(value) {
        is.atomic(value) && length(value) == 1L
      },
      logical(1)
    ))
  ) {
    stop("Each dictionary value must be a single atomic value.", call. = FALSE)
  }
  c_x <- vapply(x, as.character, character(1))
  names(c_x) <- names(x)
  write_registry.character(
    c_x,
    dict = dict,
    cache_refresh = cache_refresh,
    quiet = quiet
  )
}

#' @rdname write_registry
#' @method write_registry NULL
#' @export
#' @examples
#' # Registry attributes using built-in information
#' dict <- NULL
#' withr::with_options(
#'   list(canregtools.dict_dir = tempfile("canregtools-example-")),
#'   write_registry(dict)
#' )
#'
write_registry.NULL <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
) {
  write_registry.character(
    x = NULL,
    dict = dict,
    cache_refresh = cache_refresh,
    quiet = quiet
  )
}

#' @rdname write_registry
#' @method write_registry character
#' @export
#' @examples
#' # Registry attributes stored in named character vector with regicode as
#' # name and attributes as values
#' dict <- rep("410301", 5)
#' names(dict) <- c("410302", "410303", "410304", "410305", "410306")
#' withr::with_options(
#'   list(canregtools.dict_dir = tempfile("canregtools-example-")),
#'   write_registry(dict)
#' )
#'
write_registry.character <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
) {
  validate_registry_dict_name(dict)
  validate_registry_flag(cache_refresh, "cache_refresh")
  validate_registry_flag(quiet, "quiet")
  validate_registry_mapping(x)

  if (tolower(dict) == "regicode") {
    stop("Use `write_regicode()` to update registry labels.", call. = FALSE)
  }
  result <- update_registry_dictionary(
    dict,
    x %||% character(),
    cache_refresh,
    quiet
  )
  if (quiet) invisible(result) else result
}

validate_registry_dict_name <- function(dict) {
  if (
    !is.character(dict) || length(dict) != 1 || is.na(dict) || !nzchar(dict)
  ) {
    stop(
      "`dict` must be a non-empty character string of length 1.",
      call. = FALSE
    )
  }
  if (
    dict %in%
      c(".", "..") ||
      grepl("/", dict, fixed = TRUE) ||
      grepl(intToUtf8(92), dict, fixed = TRUE) ||
      !identical(trimws(dict), dict)
  ) {
    stop(
      "Dictionary names must not contain path separators or surrounding whitespace.",
      call. = FALSE
    )
  }
}

validate_registry_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(
      "`",
      name,
      "` must be a single non-missing logical value.",
      call. = FALSE
    )
  }
}

validate_registry_mapping <- function(x) {
  if (is.null(x)) {
    return(invisible(NULL))
  }

  if (!is.character(x) || (length(x) > 0 && is.null(names(x)))) {
    stop("`x` must be a named character vector.", call. = FALSE)
  }

  keys <- names(x)
  if (
    length(keys) > 0 &&
      (anyNA(keys) || any(!nzchar(trimws(keys))) || anyDuplicated(keys) > 0)
  ) {
    stop(
      "Registry identifiers must be non-missing, non-empty, and unique.",
      call. = FALSE
    )
  }

  if (anyNA(x)) {
    stop("Dictionary mapping values must not be missing.", call. = FALSE)
  }
  invisible(x)
}

#' Write custom registry identifier labels to dictionary
#'
#' Saves user-defined registry identifiers and their associated labels
#' (in both Chinese and English) to the local dictionary used by `canregtools`.
#' Changes are immediately used by [tidy_var()], [add_labels()], and
#' `ls_vars("regicode")`. See [write_registry()] for storage and migration details.
#'
#' @param x A data frame containing at least the following columns:
#'   `regicode`, `cname`, `ename`, `abbr_cn`, and `abbr_en`, all character.
#'   Registry identifiers must be non-missing, non-empty, and unique.
#' @param cache_refresh Logical. If TRUE, refresh the dictionary to default values
#'    before updating.
#'
#' @return Invisibly returns `NULL`. The function is called for its side effect.
#' @export
#' @examples
#' \dontrun{
#' dict <- data.frame(
#'   regicode = c("410302"),
#'   cname = c("\u8001\u57CE\u533A"),
#'   ename = c("Laocheng District"),
#'   abbr_cn = c("\u8001\u57CE"),
#'   abbr_en = c("Laocheng")
#' )
#' write_regicode(dict)
#' }
write_regicode <- function(x = NULL, cache_refresh = FALSE) {
  if (is.null(x)) {
    x <- empty_registry_overrides("regicode")
  }
  update_registry_dictionary("regicode", x, cache_refresh, quiet = TRUE)
  invisible(NULL)
}
