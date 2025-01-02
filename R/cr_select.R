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
#' @param index A vector of indices specifying the elements to select. This can 
#'        be a character vector (matching element names), a numeric vector 
#'        (specifying positions), or a logical vector (indicating inclusion).
#' @param ... Optional conditions or expressions used to filter elements within 
#'        the list or data frame. Conditions are evaluated for each element of 
#'        the input object.
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
cr_select.canregs <- function(data, ..., index = names(data)){
  data <- data[index]
  class(data) <- c("canregs", "list")
  summ <- summary(data)
  conds <- rlang::enquos(...)
  summ1 <- cr_select.summaries(summ, !!!conds)
  res <- data[names(summ1)]
  res <- compact(res)
  class(res) <- c("canregs", "list")
  return(res)
}

#' @rdname cr_select
#' @method cr_select asrs
#' @export
cr_select.asrs <- function(data, ..., index = names(data)) {
  data <- data[index]
  conds <- rlang::enquos(...)
  res <- purrr::keep(data, check_conds, !!!conds)
  class(res) <- c("asrs", "list")
  return(res)
}

#' @rdname cr_select
#' @method cr_select fbswicds 
#' @export
cr_select.fbswicds <- function(data, ..., index = names(data)){
  conds <- rlang::enquos(...)
  data <- data[index]
  class(data) <- c("fbswicds", "list")
  qua <- create_quality(data)
  qua1 <- purrr::keep(qua, check_conds, !!!conds)
  res <- data[names(qua1)]
  class(res) <- c("fbswicds", "list")
  return(res)
}

#' @rdname cr_select
#' @method cr_select summaries
#' @export
cr_select.summaries <- function(data, ..., index = names(data)) {
  conds <- rlang::enquos(...)
  data <- data[index]
  res <- purrr::keep(data, check_conds, !!!conds)
  class(res) <- c("summaries", "list")
  return(res)
}


check_conds <- function(x, ...) {
  conds <- rlang::enquos(...)
  all_conds <- purrr::map_lgl(conds, function(cond) {
    any(rlang::eval_tidy(cond, data = x))
  })
  all(all_conds)
}

#' Check if the data frame has rows after filtering
#' 
#' @param df A data frame to check for row count after applying conditions.
#' @param ... Conditions for filtering the data frame.
#' 
#' @return Logical: TRUE if the data frame has rows, FALSE otherwise.
has_rows <- function(df, ...) {
  sel_df <- dplyr::filter(df, !!!rlang::enquos(...))
  return(nrow(sel_df) > 0)
}
