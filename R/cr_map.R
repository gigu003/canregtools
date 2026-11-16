#' Set registry grouping mappings for cr_reframe
#'
#' Adds or updates user mappings from registry identifiers to grouping
#' attributes used by [cr_reframe()]. User mappings take precedence over
#' built-in mappings.
#'
#' @param x A named character vector, named list of scalar values, or a data
#'   frame containing `regicode` and the column(s) named by `attr`. Multiple
#'   attributes require a data frame. Registry identifiers must be non-empty and unique within an update. `NULL` writes
#'   the current overrides without adding mappings.
#' @param attr One or more unique grouping attributes: `"registry"`,
#'   `"area_type"`, or custom names also supplied to `cr_reframe(strat = ...)`. The built-in attributes
#'   `regicode`, `province`, `city`, and `region` are derived from identifiers
#'   and cannot be configured here. Use [write_regicode()] for labels.
#' @param cache_refresh If `TRUE`, clear existing user overrides before applying
#'   `x`, retaining a backup. Defaults to `FALSE`.
#' @param quiet Suppress update and backup messages and return invisibly.
#' @return A tibble with `regicode` and `value` columns containing the effective
#'   mappings for one attribute, or a named list of such tibbles for multiple
#'   attributes, invisibly when `quiet = TRUE`.
#' @details
#' Set `options(canregtools.dict_dir = "path/to/dictionaries")` to isolate
#' mappings for a project. See [write_registry()] for storage, validation,
#' migration, and write protection. `write_registry()` remains available as a
#' compatible single-attribute interface using `dict` instead of `attr`.
#' Batch updates validate all inputs and targets under writer locks before
#' writing. On an R-level write error, prior files are restored and newly
#' created files removed. This is not a crash-safe multi-file transaction;
#' readers may observe intermediate states while files are replaced.
#' @seealso [cr_reframe()], [ls_dict()], [reset_dict()]
#' @export
#' @examples
#' withr::with_options(
#'   list(canregtools.dict_dir = tempfile("canregtools-example-")),
#'   {
#'     cr_map(c("410307" = "910000"), attr = "area_type")
#'     cr_map(list("410103" = "410100"), attr = "registry")
#'     mapping <- data.frame(
#'       regicode = c("410103", "410307", "411721"),
#'       custom = c("A", "A", "B"),
#'       hdi = c("high", "high", "low")
#'     )
#'     cr_map(mapping, attr = c("custom", "hdi"))
#'     grouped <- cr_reframe(canregs, strat = "custom")
#'     names(grouped)
#'   }
#' )
cr_map <- function(
  x = NULL,
  attr = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
) {
  if (
    !is.character(attr) ||
      !length(attr) ||
      anyNA(attr) ||
      anyDuplicated(tolower(attr))
  ) {
    stop(
      "`attr` must be a non-empty character vector of unique grouping names.",
      call. = FALSE
    )
  }
  for (name in attr) {
    validate_registry_dict_name(name)
  }
  if (any(tolower(attr) %in% c("regicode", "province", "city", "region"))) {
    stop(
      "`attr` must be 'registry', 'area_type', or a custom grouping name. ",
      "Derived attributes cannot be configured; use `write_regicode()` for labels.",
      call. = FALSE
    )
  }
  if (length(attr) == 1L) {
    return(write_registry(
      x,
      dict = attr,
      cache_refresh = cache_refresh,
      quiet = quiet
    ))
  }
  if (!is.data.frame(x)) {
    stop(
      "Multiple attributes require a data frame with `regicode` and all selected columns.",
      call. = FALSE
    )
  }
  required <- c("regicode", attr)
  if (anyDuplicated(names(x)) || !all(required %in% names(x))) {
    stop(
      "`x` must have unique column names and contain: ",
      paste(required, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  updates <- stats::setNames(
    lapply(attr, function(name) {
      column <- x[[name]]
      if (!is.atomic(column) || !is.null(dim(column))) {
        stop("Each mapping column must be an atomic vector.", call. = FALSE)
      }
      mapping <- stats::setNames(as.character(column), as.character(x$regicode))
      validate_registry_mapping(mapping)
      mapping
    }),
    attr
  )
  result <- update_registry_dictionary_batch(updates, cache_refresh, quiet)
  if (quiet) invisible(result) else result
}
