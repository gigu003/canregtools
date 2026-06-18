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
      dplyr::across(
        dplyr::all_of(get_sumvar(data)),
        function(x) sum(x, na.rm = TRUE)
      )
      )
}

#' Get standardized population data
#'
#' Retrieves standardized population data for a specified standard from dict_maps.
#'
#' @param std Character string specifying the population standard. 
#'   Supported values are "cn64", "cn82", "cn2000", "wld85", "wld2000".
#'   Defaults to "wld85".
#' @param sep_zero Logical value indicating whether age 0 should be treated
#'      as a separate group.
#' @return A vector or data structure containing the standardized population data 
#'   for the specified standard, or NULL if the standard is not supported.
#' @examples
#' \dontrun{
#'   get_std("cn64")
#'   get_std("wld2000")
#' }
#' @importFrom purrr pluck
#' @export
get_stdpop <- function(std = "wld85", sep_zero = TRUE) {
  if (!std %in% c("cn64", "cn82", "cn2000", "wld85", "wld2000")) {
    cat(paste(std, "was not supported."))
  } else {
    rks <- purrr::pluck(dict_maps, "std_pop") |>
      purrr::pluck(std)
    if (!sep_zero) {
      c(sum(rks[1:2]), rks[3:19])
    } else {
      rks
    }
  }
}
