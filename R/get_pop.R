#' Subset or summarize population data
#'
#' Extracts population data from canreg-style objects and optionally summarizes it
#' by specified grouping variables.
#'
#' @param data An object of class `canreg`, `canregs`, `fbswicd`, or `fbswicds`.
#' @param sum_by Character vector of grouping variables to summarize population.
#' @param collapse Logical, if `TRUE`, collapses the list of results into a single data frame (only for list-type objects).
#'
#' @return A data frame or a list of data frames depending on `collapse`.
#' @export
get_pop <- function(
    data,
    sum_by = NULL,
    collapse = FALSE
    ) {
  UseMethod("get_pop", data)
}

#' @rdname get_pop
#' @method get_pop canreg
#' @export
get_pop.canreg <- function(
    data,
    sum_by = NULL,
    collapse = FALSE
    ) {
  getpop(data = data, sum_by = sum_by, pop = "POP")
}

#' @rdname get_pop
#' @method get_pop canregs
#' @export
get_pop.canregs <- function(
    data,
    sum_by = NULL,
    collapse = FALSE
    ) {
  res <- purrr::map(data, get_pop, sum_by)
  if (collapse) {
    res |> cmerge()
  } else {
    res
  }
}

#' @rdname get_pop
#' @method get_pop fbswicd
#' @export
get_pop.fbswicd <- function(
    data,
    sum_by = NULL,
    collapse = FALSE
    ) {
  getpop(data = data, sum_by = sum_by, pop = "pop")
}

#' @rdname get_pop
#' @method get_pop fbswicds
#' @export
get_pop.fbswicds <- function(
    data,
    sum_by = NULL,
    collapse = FALSE
    ) {
  res <- purrr::map(data, get_pop, sum_by)
  if (collapse) {
    res |> cmerge()
  } else {
    res
  }
}

#' @noRd
#' 
getpop <- function(
    data,
    sum_by = NULL,
    pop = "POP"
    ) {
  df <- purrr::pluck(data, pop)
  if (is.null(sum_by)) {
    return(df)
  }
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(sum_by))) |>
    dplyr::reframe(
      dplyr::across( dplyr::all_of(get_sumvar(data)), sum, na.rm = TRUE)
      )
}