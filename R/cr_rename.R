#' Rename registries in a `canregs` object
#'
#' `cr_rename()` renames selected elements of a `canregs` object and updates
#' the `areacode` stored inside each renamed `canreg` element.
#'
#' @param x An object of class `canregs`.
#' @param mapping A named character vector. Names identify existing registry
#'   names in `x`, and values provide their new names.
#'
#' @return A `canregs` object with selected element names and their internal
#'   `areacode` values updated.
#' @export
#'
#' @examples
#' data("canregs")
#' renamed <- cr_rename(
#'   canregs,
#'   c("410103" = "REG_A", "410307" = "REG_B")
#' )
#' names(renamed)
#' renamed[["REG_A"]]$areacode
cr_rename <- function(x, mapping) {
  UseMethod("cr_rename", x)
}

#' @rdname cr_rename
#' @method cr_rename canregs
#' @export
cr_rename.canregs <- function(x, mapping) {
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
    x[[locations[[i]]]]$areacode <- new_names[[i]]
  }
  names(x) <- result_names

  structure(x, class = class(x))
}
