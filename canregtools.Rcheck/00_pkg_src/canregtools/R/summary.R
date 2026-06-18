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
#' @examples
#' data("canregs")
#' data <- canregs[[1]]
#' summary(data)
#'
summary.canreg <- function(object, collapse = FALSE, ...) {
  r_vars_inci <- c("sex", "birthda", "inciden", "deathda", "basi", "icd10")
  object_format <- cr_clean(object)
  fb <- purrr::pluck(object_format, "FBcases")
  sw <- purrr::pluck(object_format, "SWcases")
  pop <- purrr::pluck(object_format, "POP")

  basi <- purrr::pluck(fb, "basi")
  basi2 <- basi[basi %in% 5:7]
  basi0 <- basi[basi == 0]

  fbs <- nrow(fb)
  sws <- nrow(sw)
  mv <- round(length(basi2) / fbs * 100, 2)
  dco <- round(length(basi0) / fbs * 100, 2)
  rks <- sum(purrr::pluck(pop, "rks"))
  rks <- ifelse(is.na(rks), 0, rks)
  rks_year <- pop |>
    pull(!!rlang::sym("year")) |>
    unique()
  if (length(rks_year) == 0) {
    rks_year <- 0
  }

  if ("death" %in% names(pop)) {
    death <- sum(purrr::pluck(pop, "death"))
  } else {
    death <- 0
  }

  if (is.na(rks) || rks == 0) {
    inci <- 0
    mort <- 0
  } else {
    inci <- round(fbs / rks * 100000, 2)
    mort <- round(sws / rks * 100000, 2)
  }

  inci_vars <- names(fb)
  mort_vars <- names(sw)

  miss_r_vars_inci <- dplyr::setdiff(r_vars_inci, names(fb))
  miss_r_vars_mort <- dplyr::setdiff(r_vars_inci, names(sw))

  res <- list(
    areacode = purrr::pluck(object, "areacode"),
    rks = rks,
    fbs = fbs,
    inci = inci,
    sws = sws,
    mort = mort,
    mi = round(sws / fbs, 2),
    mv = mv,
    dco = dco,
    death = death,
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
#'
#' @examples
#'
#' summary(canregs)
#'
summary.canregs <- function(object, collapse = TRUE, ...) {
  res <- purrr::map(object, summary.canreg)
  class(res) <- c("summaries", class(res))
  if (collapse) {
    res <- cr_merge(res)
    class(res) <- c("summary", "tibble", "data.frame")
  }
  return(res)
}
