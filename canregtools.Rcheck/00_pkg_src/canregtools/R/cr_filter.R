#' Filter cases from objects of class `canreg` or `canregs`
#'
#' @rdname cr_filter
#'
#' @param .data An object with class of `canreg`, or `canregs`.

#' @param ... Filtering conditions passed to [dplyr::filter()]. These
#' @param drop Drop specific cancer categories.
#' @param part A character vector specifying which components of `.data` to
#'   filter. Must be one or more of `"FBcases"`, `"SWcases"`, or `"POP"`.
#'   Defaults to `"FBcases"`.
#'   conditions are applied to the selected components.
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
                              part = "all"
                              ) {
  conds <- rlang::enquos(...)
  res <- purrr::map(
    .data,
    cr_filter,
    !!!conds,
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
                             part = "all"
                             ) {
  if (part == "all") part <- c("FBcases", "SWcases", "POP")
  conds <- rlang::enquos(...)
  valid_parts <- dplyr::setdiff(names(.data), "areacode")
  if (any(!part %in% valid_parts)) {
    stop("Unsupported part name(s): ",
         paste(part[!part %in% valid_parts], collapse = ", "))
  }
  res <- .data
  expr_vars <- get_expr_vars(...)
  for (type in part) {
    if (type == "POP") {
      updated_part <- pluck(res, type)
    } else if (type == "FBcases") {
      updated_part <- pluck(res, type) |>
        mutate(year = format(!!rlang::sym("inciden"), "%Y"))
    } else if (type == "SWcases") {
      updated_part <- pluck(res, type) |>
        mutate(year = format(!!rlang::sym("deathda"), "%Y"))
    }
    
    if (all(expr_vars %in% colnames(updated_part))) {
      res[[type]] <- updated_part |>
        filter(!!!conds)
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
