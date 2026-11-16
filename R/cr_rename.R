#' Rename registries in a registry collection
#'
#' `cr_rename()` renames selected elements of a `canregs` or `fbswicds` object
#' and updates the `regicode` stored inside each renamed registry element.
#'
#' @param x An object of class `canregs` or `fbswicds`.
#' @param mapping A named character vector. Names identify existing registry
#'   names in `x`, and values provide their new names.
#'
#' @return An object with the same class as `x`, with selected element names
#'   and their internal `regicode` values updated.
#' @export
#'
#' @examples
#' data("canregs")
#' renamed <- cr_rename(
#'   canregs,
#'   c("410103" = "REG_A", "410307" = "REG_B")
#' )
#' names(renamed)
#' renamed[["REG_A"]]$regicode
#'
#' summarized <- count_canreg(canregs)
#' renamed_summaries <- cr_rename(summarized, c("410103" = "REG_A"))
#' renamed_summaries[["REG_A"]]$regicode
cr_rename <- function(x, mapping) {
  UseMethod("cr_rename", x)
}

#' @rdname cr_rename
#' @method cr_rename canregs
#' @export
cr_rename.canregs <- function(x, mapping) {
  rename_registry_collection(x, mapping)
}

#' @rdname cr_rename
#' @method cr_rename fbswicds
#' @export
cr_rename.fbswicds <- function(x, mapping) {
  rename_registry_collection(x, mapping)
}

rename_registry_collection <- function(x, mapping) {
  if (!is.character(mapping) || is.null(names(mapping))) {
    stop("`mapping` must be a named character vector.", call. = FALSE)
  }

  if (length(mapping) == 0) {
    return(x)
  }

  old_names <- names(mapping)
  new_names <- unname(mapping)
  invalid_old <- is.na(old_names) | !nzchar(old_names)
  invalid_new <- is.na(new_names) | !nzchar(new_names)

  if (any(invalid_old) || any(invalid_new)) {
    stop(
      "Names and values in `mapping` must not be missing or empty.",
      call. = FALSE
    )
  }

  if (anyDuplicated(old_names)) {
    stop("Names in `mapping` must be unique.", call. = FALSE)
  }

  unknown_names <- setdiff(old_names, names(x))
  if (length(unknown_names) > 0) {
    stop(
      "Unknown registry name(s) in `mapping`: ",
      paste(unknown_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  result_names <- names(x)
  locations <- match(old_names, result_names)
  result_names[locations] <- new_names
  if (anyDuplicated(result_names)) {
    stop("Renaming would create duplicate registry names.", call. = FALSE)
  }

  for (i in seq_along(locations)) {
    x[[locations[[i]]]]$regicode <- new_names[[i]]
  }
  names(x) <- result_names

  structure(x, class = class(x))
}
