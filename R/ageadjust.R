#' Calculate the age standardized rate using the direct method.
#'
#' @param count The number of cases of a specific disease or condition.
#' @param pop The total population of the same group or region
#'        where the disease cases (count) were observed.
#' @param rate Disease rate, which is the number of cases (count) per unit
#'        of population (pop).
#' @param stdpop Standardized population for age standardization.
#' @param method Method used for calculating the age-standardized rate, options
#'        are 'gamma', 'normal', or 'lognormal', default is 'gamma'.
#' @param conf_level Confidence level for calculating confidence
#'        intervals, value between 0 and 1, default is 0.95.
#' @param mp A multiplier used to scale the calculated rates. Default is 100000.
#'
#' @importFrom stats qgamma qnorm
#'
#' @return Age standardized rate and its confidence interval.
#' @export
#'
#' @examples
#' cases <- c(50, 60, 45, 70)
#' pop <- c(1000, 1200, 1100, 900)
#' spop <- c(800, 1000, 1100, 900)
#' ageadjust(cases, pop, stdpop = spop, mp = 100000)
ageadjust <- function(count,
                      pop,
                      rate = NULL,
                      stdpop = NULL,
                      method = "gamma",
                      conf_level = 0.95,
                      mp = 100000) {
  if (missing(count) == TRUE && !missing(pop) == TRUE && is.null(rate) ==
    TRUE) {
    count <- rate * pop
  }
  if (missing(pop) == TRUE && !missing(count) == TRUE && is.null(rate) ==
    TRUE) {
    pop <- count / rate
  }
  if (is.null(rate) == TRUE && !missing(count) == TRUE && !missing(pop) ==
    TRUE) {
    rate <- count / pop
  }
  alpha <- 1 - conf_level
  cases <- round(sum(count), 1)
  rks <- sum(pop)
  cr <- cases / rks
  crvar <- cases / rks^2
  
  if (!is.null(stdpop)){
    stdwt <- stdpop / sum(stdpop)
    asr <- sum(stdwt * rate)
    asrvar <- sum((stdwt^2) * (count / pop^2))
    wm <- max(stdwt / pop)  
  }
  
  if (method == "gamma") {
    cr_lci <- mp * (qgamma(alpha / 2, shape = cr^2 / crvar) / (cr / crvar))
    cr_uci <- mp * (qgamma(1 - alpha / 2,
      shape = 1 + cr^2 / crvar
    ) / (cr / crvar))
    if (!is.null(stdpop)){
      asr_lci <- mp * (qgamma(alpha / 2, shape = asr^2 / asrvar) / (asr / asrvar))
      asr_uci <- mp * (qgamma(1 - alpha / 2,
                              shape = ((asr + wm)^2) /
                                (asrvar + wm^2),
                              scale = (asrvar + wm^2) / (asr + wm)
      ))
    }

  } else if (method == "normal") {
    cr_lci <- mp * (cr + qnorm(alpha / 2) * sqrt(crvar))
    cr_uci <- mp * (cr - qnorm(alpha / 2) * sqrt(crvar))
    if (!is.null(stdpop)){
      asr_lci <- mp * (asr + qnorm(alpha / 2) * sqrt(asrvar))
      asr_uci <- mp * (asr - qnorm(alpha / 2) * sqrt(asrvar))
    }

  } else if (method == "lognormal") {
    cr_lci <- mp * exp(log(cr) + qnorm(alpha / 2) * sqrt(crvar) / cr)
    cr_uci <- mp * exp(log(cr) - qnorm(alpha / 2) * sqrt(crvar) / cr)
    if (!is.null(stdpop)){
      asr_lci <- mp * exp(log(asr) + qnorm(alpha / 2) * sqrt(asrvar) / asr)
      asr_uci <- mp * exp(log(asr) - qnorm(alpha / 2) * sqrt(asrvar) / asr)
    }
  } else {
    stop("Method specified was not supported.
       The supported methods: gamma, normal and lognormal")
  }

  cr <- mp * cr

  if (!is.null(stdpop)){
    asr <- mp * asr
    res <- list(
      cases = cases,
      cr = cr, cr_var = crvar, cr_lci = cr_lci, cr_uci = cr_uci,
      asr = asr, asr_var = asrvar, asr_lci = asr_lci, asr_uci = asr_uci
    )
  } else {
    res <- list(
      cases = cases, cr = cr, cr_var = crvar, cr_lci = cr_lci, cr_uci = cr_uci
    )
  }
 return(res)
}
