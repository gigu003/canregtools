#' Estimate object of fbswicd
#'
#' This function estimates `fbswicd` object based on existing `fbswicd` object
#' and using the total population data.
#'
#' @param obj An object with class of `fbswicd`.
#' @param pop An population dataset to override the population in `obj`.
#'
#' @return An estimated object with class of `fbswicd`.
#'
#' @details
#' The estimation proceeds in the following steps:
#' \enumerate{
#'   \item Calculate age-specific incidence and mortality rates for different
#'      sex and cancer sites.
#'   \item Derive estimated counts of `fbs` and `sws` using the
#'      rate × population formula.
#'   \item Recalculate proportions (e.g., `mv`, `dco`) back to
#'      estimated case counts.
#'   \item Join all information and return a new object of the same class as `obj`.
#' }
#'
#'
#' @seealso [create_age_rate()], [cr_filter()]
#'
#' @export
#'
esti_fbswicd <- function(obj, pop = NULL) {
  strat <- rlang::syms(c("year", "sex", "cancer"))
  rks <- rlang::sym("rks")
  fbs <- rlang::sym("fbs")
  sex <- rlang::sym("sex")
  agegrp <- rlang::sym("agegrp")
  inci <- create_age_rate(obj, event = "fbs", !!!strat) |>
    cr_filter(!!!sex == 0) |>
    select(-!!rlang::sym("cases")) |>
    rename(inci = !!rlang::sym("rate"))
  mort <- create_age_rate(obj, event = "sws", !!!strat) |>
    cr_filter(!!!sex == 0) |>
    select(-!!rlang::sym("cases")) |>
    rename(mort = !!rlang::sym("rate"))

  esti_var <- c("mv", "ub", "sub", "dco", "m8000")
  fbsw <- purrr::pluck(obj, "fbswicd") |>
    mutate(across(all_of(esti_var), ~ replace_na(.x / !!fbs)))
  
  pop_modi <- pop
  
  esti <- list(inci, mort, fbsw) |>
    purrr::reduce(left_join, by = join_by(!!!strat, !!agegrp)) |>
    left_join(pop_modi, by = join_by(!!!rlang::syms(c("year", "sex", "agegrp")))) |>
    mutate(fbs = round(inci * !!rks / 1e5),
           sws = round(mort * !!rks / 1e5),
           across(all_of(esti_var), ~ as.integer(replace_na(round(.x * fbs))))
           ) |>
    select(-!!rlang::sym("inci"), -!!rlang::sym("mort"), -!!rks)
  structure(
    list(
      areacode = obj$areacode,
      fbswicd = esti,
      sitemorp = pluck(obj, "sitemorp"),
      pop = pop_modi
    ),
    class = class(obj)
  )
}
