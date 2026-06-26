#' Filter rows from canregtools objects
#'
#' @rdname cr_filter
#'
#' @param .data An object with class of `canreg`, `canregs`, `fbswicd`,
#'   `fbswicds`, or a data frame-like object.

#' @param ... Filtering conditions passed to [dplyr::filter()]. These
#'   conditions are applied to selected components when all variables in the
#'   expressions are available in that component.
#' @param drop Drop specific cancer categories. Use `"total"` to remove total
#'   cancer categories and `"others"` to remove other/non-specific cancer
#'   categories when a selected component has a `cancer` column.
#' @param part A character vector specifying which components of `.data` to
#'   filter. For `canreg` objects, use one or more of `"FBcases"`, `"SWcases"`,
#'   or `"POP"`. For `fbswicd` objects, use one or more of `"fbswicd"`,
#'   `"sitemorp"`, `"pop"`, or `"exclude"`. Use `"all"` to filter all available
#'   components.
#' @return An object of the same class as `.data` (`canreg` or `canregs`) with
#'   the specified components filtered accordingly. The structure and component
#'   order are preserved.
#' @export
#'
cr_filter <- function(
    .data,
    ...,
    drop = c("none"),
    part = "all"
    ) {
  UseMethod("cr_filter", .data)
}

#' @rdname cr_filter
#' @method cr_filter canregs
#' @export
#'
cr_filter.canregs <- function(.data,
                              ...,
                               drop = c("none"),
                              part = "all"
                              ) {
  conds <- rlang::enquos(...)
  res <- purrr::map(
    .data,
    cr_filter,
    !!!conds,
    drop = drop,
    part = part
    ) |> compact()
  structure(
    res,
    class = class(.data)
  )
}

#' @rdname cr_filter
#' @method cr_filter canreg
#' @export
#'
cr_filter.canreg <- function(.data,
                             ...,
                              drop = c("none"),
                             part = "all"
                             ) {
  valid_parts <- dplyr::setdiff(names(.data), "areacode")
  part <- normalize_cr_filter_part(part, valid_parts)
  conds <- rlang::enquos(...)
  res <- .data
  expr_vars <- get_expr_vars(...)
  for (type in part) {
    added_year <- FALSE
    if (type == "POP") {
      updated_part <- pluck(res, type)
    } else if (type == "FBcases") {
      updated_part <- pluck(res, type)
      if ("year" %in% expr_vars && "year" %nin% names(updated_part)) {
        updated_part <- updated_part |>
          mutate(year = format(!!rlang::sym("inciden"), "%Y"))
        added_year <- TRUE
      }
    } else if (type == "SWcases") {
      updated_part <- pluck(res, type)
      if ("year" %in% expr_vars && "year" %nin% names(updated_part)) {
        updated_part <- updated_part |>
          mutate(year = format(!!rlang::sym("deathda"), "%Y"))
        added_year <- TRUE
      }
    }

    if (all(expr_vars %in% colnames(updated_part))) {
      res[[type]] <- updated_part |>
        filter(!!!conds) |>
        apply_cr_filter_drop(drop) |>
        remove_cr_filter_year(added_year)
      }
  }

  empty <- !all(purrr::map_lgl(res[c("FBcases", "SWcases")], has_rows))

  if (empty) {
    return(NULL)
  }

  structure(
    res,
    class = c("canreg", "list")
    )
}

#' @rdname cr_filter
#' @method cr_filter fbswicds
#' @export
#'
cr_filter.fbswicds <- function(.data,
                               ...,
                               drop = c("none"),
                               part = "all"
                               ) {
  conds <- rlang::enquos(...)
  res <- purrr::map(
    .data,
    cr_filter,
    !!!conds,
    drop = drop,
    part = part
  ) |> compact()
  structure(
    res,
    class = class(.data)
  )
}

#' @rdname cr_filter
#' @method cr_filter fbswicd
#' @export
#'
cr_filter.fbswicd <- function(.data,
                              ...,
                              drop = c("none"),
                              part = "all"
                              ) {
  valid_parts <- dplyr::setdiff(names(.data), "areacode")
  part <- normalize_cr_filter_part(part, valid_parts)
  conds <- rlang::enquos(...)
  expr_vars <- get_expr_vars(...)
  res <- .data

  for (type in part) {
    updated_part <- purrr::pluck(res, type)
    if (!is.data.frame(updated_part)) {
      next
    }
    if (all(expr_vars %in% colnames(updated_part))) {
      res[[type]] <- updated_part |>
        filter(!!!conds) |>
        apply_cr_filter_drop(drop)
    } else if (length(expr_vars) == 0) {
      res[[type]] <- updated_part |>
        apply_cr_filter_drop(drop)
    }
  }

  if (!has_rows(res[["fbswicd"]])) {
    return(NULL)
  }

  structure(
    res,
    class = c("fbswicd", "list")
  )
}

#' @rdname cr_filter
#' @method cr_filter asrs
#' @export
#'
cr_filter.asrs <- function(
    .data,
    ...,
    drop = c("none")
    ) {
  conds <- rlang::enquos(...)
  res <- purrr::map(
    .data,
    cr_filter,
    !!!conds,
    drop = drop
  )
  structure(
    res,
    class = class(.data)
  )
}

#' @rdname cr_filter
#' @method cr_filter age_rates
#' @export
#'
cr_filter.age_rates <- cr_filter.asrs

#' @rdname cr_filter
#' @method cr_filter qualities
#' @export
#'
cr_filter.qualities <- cr_filter.asrs

#' @rdname cr_filter
#' @method cr_filter default
#' @export
#'
cr_filter.default <- function(
    .data,
    ...,
    drop = c("none")
    ) {
  conds <- rlang::enquos(...)
  res <- filter(.data, !!!conds)
  if ("total" %in% drop & "cancer" %in% colnames(.data)) {
    res <- drop_total(res)
  }
  if ("others" %in% drop & "cancer" %in% colnames(.data)) {
    res <- drop_others(res)
  }
  structure(
    res,
    class = class(.data)
    )
}

#' Normalize `part` argument for cr_filter methods
#'
#' @noRd
normalize_cr_filter_part <- function(part, valid_parts) {
  if (length(part) == 0) {
    stop("`part` must not be empty.", call. = FALSE)
  }
  if ("all" %in% part) {
    if (length(part) > 1) {
      stop('`part = "all"` cannot be combined with other part names.', call. = FALSE)
    }
    return(valid_parts)
  }
  if (any(!part %in% valid_parts)) {
    stop("Unsupported part name(s): ",
         paste(part[!part %in% valid_parts], collapse = ", "),
         call. = FALSE)
  }
  part
}

#' Apply `drop` argument to a filtered component
#'
#' @noRd
apply_cr_filter_drop <- function(x, drop = c("none")) {
  if ("total" %in% drop & "cancer" %in% colnames(x)) {
    x <- drop_total(x)
  }
  if ("others" %in% drop & "cancer" %in% colnames(x)) {
    x <- drop_others(x)
  }
  x
}

#' Remove temporary year column when cr_filter added it
#'
#' @noRd
remove_cr_filter_year <- function(x, added_year = FALSE) {
  if (isTRUE(added_year)) {
    x <- dplyr::select(x, -dplyr::any_of("year"))
  }
  x
}
