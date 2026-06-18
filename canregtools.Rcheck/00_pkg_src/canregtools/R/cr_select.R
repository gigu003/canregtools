#' Select elements from objects with class `"canregs"`, or `"fbswicds"`
#'
#' @description
#' This function allows you to select specific elements from objects of class
#' 'canregs', 'fbswicds', or 'asrs' based on provided indices, logical
#' conditions, or expressions. The selected elements are returned while
#' preserving the class of the input object.
#'
#' @rdname cr_select
#' @param data An object of class 'canregs', 'fbswicds', or 'asrs' from which
#'        elements will be selected.
#' @param ... Optional conditions or expressions used to filter elements within
#'        the list or data frame. Conditions are evaluated for each element of
#'        the input object.
#' @param index A vector of indices specifying the elements to
#'      select. This can be a character vector (matching element names), a
#'      numeric vector (specifying positions), or a logical vector
#'      (indicating inclusion).
#' @return An object of the same class as the input object, containing only the
#'         selected elements that meet the specified indices or conditions.
#'
#' @export
#'
cr_select <- function(data, ..., index = names(data)) {
  UseMethod("cr_select", data)
}

#' @rdname cr_select
#' @method cr_select canregs
#' @export
#' @examples
#' data("canregs")
#' # Select elements which mi greather than 0.5 from `canregs`
#' canregs_mi <- cr_select(canregs, mi > 0.5)
#'
cr_select.canregs <- function(data, ..., index = names(data)) {
  data_index <- structure(data[index], class = class(data))

  conds <- rlang::enquos(...)
  if (length(conds) == 0) {
    return(data_index)
  }
  vars <- get_expr_vars(!!!conds)
  if (all(vars %in% ls_vars("reframe")[["code"]])) {
    isnot <- check_reframe_expr(names(data), !!!conds)
    res <- data_index[isnot]
  } else if (length(conds) > 0) {
    summ <- summary(data_index, collapse = FALSE)
    summ1 <- cr_select.summaries(summ, !!!conds)
    res <- data_index[names(summ1)]
  }
  structure(compact(res), class = class(data))
}

#' @rdname cr_select
#' @method cr_select asrs
#' @export
cr_select.asrs <- function(data, ..., index = names(data)) {
  data_index <- structure(data[index], class = class(data))
  conds <- rlang::enquos(...)
  if (length(conds) == 0) {
    return(data_index)
  }
  
  vars <- get_expr_vars(!!!conds)
  if (all(vars %in% ls_vars("reframe")[["code"]])) {
    isnot <- check_reframe_expr(names(data), !!!conds)
    res <- data_index[isnot]
  } else if (length(conds) > 0) {
    res <- purrr::keep(data_index, check_conds, !!!conds)
  } 
  structure(compact(res), class = class(data))
}

#' @rdname cr_select
#' @method cr_select age_rates
#' @export
cr_select.age_rates <- cr_select.asrs

#' @rdname cr_select
#' @method cr_select fbswicds
#' @export
#' @examples
#' # Select elements from obejct with class of `fbswicds`
#' fbsws <- count_canreg(canregs)
#' # Select elements which `inci` greater than 250 per 100000 population
#' fbsws_inci <- cr_select(fbsws, inci > 250)
#'
cr_select.fbswicds <- function(data, ..., index = names(data)) {
  data_index <- structure(
    data[index],
    class = class(data)
  )
  
  conds <- rlang::enquos(...)
  if (length(conds) == 0) {
    return(data_index)
  }
  vars <- get_expr_vars(!!!conds)
  if (all(vars %in% ls_vars("reframe")[["code"]])) {
    isnot <- check_reframe_expr(names(data), !!!conds)
    res <- data_index[isnot]
  } else if (length(conds) > 0) {
      qua <- create_quality(data_index, collapse = FALSE)
      qua1 <- purrr::keep(qua, check_conds, !!!conds)
      res <- data[names(qua1)]
    }
  structure(compact(res), class = class(data))
}

#' @rdname cr_select
#' @method cr_select summaries
#' @export
#' @examples
#' # Select elements from object with class of `summaries`
#' summ <- summary(canregs, collapse = FALSE)
#' # Select elements for whcih `mi` greater than 0.5
#' summ_mi <- cr_select(summ, mi > 0.5)
#' names(summ_mi)
#'
cr_select.summaries <- function(data, ..., index = names(data)) {
  data_index <- structure(data[index], class = class(data))
  conds <- rlang::enquos(...)
  if (length(conds) == 0) {
    return(data_index)
  }
  res <- purrr::keep(data_index, check_conds, !!!conds)
  structure(
    res,
    class = class(data)
  )
}

#' Check if all conditions are met by any row in the data
#'
#' @param x A data frame-like object.
#' @param ... One or more conditions (expressions) to evaluate in `x`.
#'
#' @returns `TRUE` if every condition is satisfied by at least one row in `x`,
#'    `FALSE` otherwise.
#'
#' @noRd
check_conds <- function(x, ...) {
  conds <- rlang::enquos(...)
  all_conds <- purrr::map_lgl(conds, function(cond) {
    any(rlang::eval_tidy(cond, data = x))
  })
  all(all_conds)
}

#' Check if the data frame has rows after filtering
#'
#' Applies given conditions to filter a data frame and checks whether any
#' rows remain.
#'
#' @param x A data frame to check.
#' @param ... Conditions passed to dplyr::filter for subsetting the data.
#'
#' @return Logical. Returns TRUE if any rows remain after filtering;
#'    FALSE otherwise.
#' @noRd
has_rows <- function(x, ...) {
  sel_df <- dplyr::filter(x, !!!rlang::enquos(...))
  nrow(sel_df) > 0
}
