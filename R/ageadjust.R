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

#' Calculate truncated age standardized rate.
#'
#' @param cases Number of cases.
#' @param pop  Number of population at risk.
#' @param stdpop The standard population.
#' @param trunc_age The truncated age range.
#' @param agewidth Age groups width, default is 5.
#' @param sep_zero Logical value, if the 0 age group was a separate group.
#' @param mp A multiplier used to scale the calculated rates. Default is 100. 
#' @param decimal Decimals of the calculated rates, default is 2.
#'
#' @return Truncated age standardized rate.
#' @export
#'
#' @examples
#' px <- c(20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
#'         163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909)
#' dx <- c(156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
#'         891, 831, 926, 731, 269)
#' stdpop <- c(2.4,9.6,10,9,9,8,8,6,6,6,6,5,4,4,3,2,1,0.5,0.5)
#' truncrate(dx, px, stdpop, trunc_age=c(35,64))
truncrate <- function(cases,
                      pop,
                      stdpop = NULL,
                      trunc_age = c(35, 64),
                      agewidth = 5,
                      sep_zero = TRUE,
                      mp = 100,
                      decimal = 2){
  if (length(cases) != length(pop)) {
    stop("Length of 'cases' and 'pop' must be the same.")
  }
  
  nn <- length(cases)
  
  if (sep_zero){
    age <- c(0, 1, seq(agewidth, (nn-2)*agewidth, agewidth))
    agewidth <- c(1, agewidth-1, rep(agewidth, nn-2))
  } else {
    age <- c(0, seq(agewidth, (nn-1)*agewidth, agewidth))
    agewidth <- rep(agewidth, nn)
  }
  
  pos_start <- which(age == trunc_age[1])
  pos_end <- which(age == (trunc_age[2] + 1))
  
  cases <- cases[pos_start : pos_end]
  pop <- pop[pos_start : pos_end]
  stdpop <- stdpop[pos_start : pos_end]
  rate <- cases / pop
  if (!is.null(stdpop)){
    rate <- sum(rate*stdpop)/sum(stdpop)
  }
  rate <- round(rate * mp, decimal)
  return(rate)
}


