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
#'   Supported values are "cn64", "cn82", "cn2000", "wld85", "wld2000",
#'   "eur76", "eur2013", and "usa2000". Defaults to "wld85".
#' @details `eur2013` follows the 2013 European Standard Population. Its
#'   source separates ages 85--89, 90--94, and 95 years and over; these are
#'   combined into the final `85+` group to match the 19-group structure used
#'   by this package.
#' @param sep_zero Logical value indicating whether age 0 should be treated
#'   as a separate group. Retained for compatibility; it cannot be supplied
#'   together with `age_breaks`.
#' @param age_breaks Numeric vector of lower bounds for the requested age
#'   groups. It must begin at 0 and end no later than 85; the final group is
#'   open-ended. Defaults to the original 19 groups, or to 18 five-year groups
#'   when `sep_zero = FALSE`.
#' @param method Method for obtaining the requested grouping. With
#'   `"exact"` (the default), requested groups must only combine the original
#'   standard-population groups. `"interpolate"` expands the standard
#'   population to single-year ages before regrouping, and should be used only
#'   when a requested boundary splits an original group.
#' @param interpolation_method Interpolation method passed to
#'   [expand_age_pop()] when `method = "interpolate"`. With `"sprague"`,
#'   requested age groups cannot split 0--4 because the source 0 and 1--4
#'   groups are combined before interpolation.
#' @return A numeric vector containing the standardized population weights for
#'   the requested age groups, or `NULL` if the standard is not supported.
#' @examples
#' \dontrun{
#'   get_stdpop("cn64")
#'   get_stdpop("wld2000", age_breaks = c(0, 1, 15, 45, 65, 85))
#' }
#' @importFrom purrr pluck
#' @export
get_stdpop <- function(
  std = "wld85",
  sep_zero = NULL,
  age_breaks = NULL,
  method = c("exact", "interpolate"),
  interpolation_method = "linear"
) {
  supported_std <- names(purrr::pluck(dict_maps, "std_pop"))[-1]
  if (!all(std %in% supported_std)) {
    cat(paste(std, "was not supported."))
    return(invisible(NULL))
  }

  if (is.null(age_breaks)) {
    age_breaks <- if (is.null(sep_zero) || sep_zero) {
      std_age_breaks()
    } else {
      c(0, seq(5, 85, 5))
    }
  } else {
    if (!is.null(sep_zero)) {
      stop("`sep_zero` cannot be used together with `age_breaks`.")
    }
    age_breaks <- validate_std_age_breaks(age_breaks)
  }

  method <- match.arg(method)
  rks <- purrr::pluck(dict_maps, "std_pop") |>
    purrr::pluck(std)

  if (method == "exact") {
    return(reframe_stdpop_exact(rks, age_breaks))
  }

  reframe_stdpop_interpolate(rks, age_breaks, interpolation_method)
}

std_age_breaks <- function() {
  c(0, 1, seq(5, 85, 5))
}

validate_std_age_breaks <- function(age_breaks) {
  if (
    !is.numeric(age_breaks) ||
      length(age_breaks) < 2L ||
      any(!is.finite(age_breaks)) ||
      age_breaks[1] != 0 ||
      utils::tail(age_breaks, 1) > 85 ||
      any(diff(age_breaks) <= 0)
  ) {
    stop(
      "`age_breaks` must be strictly increasing, begin at 0, and end no later ",
      "than 85."
    )
  }
  as.numeric(age_breaks)
}

reframe_stdpop_exact <- function(rks, age_breaks) {
  source_breaks <- std_age_breaks()
  if (!all(age_breaks %in% source_breaks)) {
    stop(
      "`method = \"exact\"` requires age_breaks that only combine the ",
      "original standard-population age groups. Use `method = \"interpolate\"` ",
      "to split a group."
    )
  }

  group <- findInterval(source_breaks, age_breaks)
  vapply(seq_along(age_breaks), \(i) sum(rks[group == i]), numeric(1))
}

reframe_stdpop_interpolate <- function(rks, age_breaks, interpolation_method) {
  if (
    tolower(interpolation_method) == "sprague" &&
      any(age_breaks > 0 & age_breaks < 5)
  ) {
    stop(
      "`interpolation_method = \"sprague\"` combines the 0 and 1--4 ",
      "standard-population groups, so `age_breaks` cannot split 0--4."
    )
  }
  scale <- if (max(abs(rks), na.rm = TRUE) < 1e6) 1e6 else 1
  single_age <- expand_age_pop(rks * scale, method = interpolation_method)
  single_age$y <- single_age$y / scale
  agegrp <- cutage(
    single_age$x,
    method = "interval",
    breaks = age_breaks,
    labels = as.character(age_breaks)
  )
  unname(vapply(
    seq_along(age_breaks),
    \(i) sum(single_age$y[as.integer(agegrp) == i]),
    numeric(1)
  ))
}
