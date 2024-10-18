#' Summary object of class 'canreg'
#'
#' @rdname summary
#' @method summary canreg
#' @param object Object data with class of 'canreg', 'canregs'
#' @param ... Other filter expressions
#' @param collapse Collapse data or not.
#'
#' @return A data frame contains summary statistics of canreg data.
#' @export
#' 
summary.canreg <- function(object, ...) {
  r_vars_inci <- c("sex", "birthda", "inciden", "deathda", "basi", "icd10")
  fb <- purrr::pluck(object, "FBcases")
  sw <- purrr::pluck(object, "SWcases")
  pop <- purrr::pluck(object, "POP")
  
  fbs <- nrow(fb)
  sws <- nrow(sw)
  rks <- sum(purrr::pluck(pop, "rks"))
  rks_year <- pop |> pull(!!rlang::sym("year")) |> unique()
  inci <- round(fbs/rks*100000, 2)
  mort <- round(sws/rks*100000, 2)
  if (rks == 0){
    inci <- 0
    mort <- 0
  }
  
  inci_vars <- names(fb)
  mort_vars <- names(sw)
  
  miss_r_vars_inci <- setdiff(r_vars_inci, names(fb))
  miss_r_vars_mort <- setdiff(r_vars_inci, names(sw))

  res <- list(
    areacode = purrr::pluck(object, "areacode"),
    fbs = fbs,
    inci = inci,
    sws = sws,
    mort = mort,
    mi = sws / fbs,
    rks = rks,
    rks_year = rks_year,
    inci_vars = inci_vars,
    miss_r_vars_inci = miss_r_vars_inci,
    mort_vars = mort_vars,
    miss_r_vars_mort = miss_r_vars_mort
  )
  class(res) <- c("summary", class(res))
  return(res)
}

#' @rdname summary
#' @method summary canregs
#' @export
summary.canregs <- function(object, collapse = TRUE, ...) {
  res <- purrr::map(object, summary.canreg)
  class(res) <- c("summaries", class(res))
  return(res)
}

#' @rdname summary
#' @method summary check
#' @export
summary.check <- function(object,...) {
  return(any(unlist(object[["POP"]])))
}
