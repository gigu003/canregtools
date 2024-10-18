#' Select elements from objects of class canregs, fbswicds, or asrs
#' 
#' @description
#' Select elements from object of class 'canregs', 'fbswicds', or 'asrs' based
#' on provided index or conditions. The class of the returned object will be the
#' same with the input object.
#' 
#' @rdname cr_select
#' @param data An object of class 'canregs', 'asrs', or 'fbswicds'.
#' @param index A vector of character strings, logical values, or numeric
#'        indices to select object elements.
#' @param ... Conditions or expressions to subset the data within list elements.
#'
#' @return An object with selected elements from the input object that meet the
#'          specified conditions or index. The returned object will have the
#'          same class as the input object.
#' @export
#'
cr_select <- function(data, ..., index = names(data)) {
  UseMethod("cr_select", data)
}

#' @rdname cr_select
#' @method cr_select canregs
#' @export
cr_select.canregs <- function(data, ..., index = names(data)){
  res <- data[index]
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
  res <- data[index]
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
