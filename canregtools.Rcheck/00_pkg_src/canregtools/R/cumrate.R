#' Calculate the cumulative incidence or mortality rate
#'
#' Computes the cumulative rate up to a specified age limit, typically used in 
#' cancer epidemiology to estimate the probability of developing or dying from 
#' a disease over a lifetime or up to a target age.
#'
#' @param count Numeric vector, number of incident cases or deaths in each age
#'    group.
#' @param pop Numeric vector, corresponding population at risk for each age
#'    group.
#' @param rate Numeric vector, age-specific incidence or mortality rates. If not 
#'   supplied, it will be calculated as \code{count / pop}.
#' @param eage Integer, the upper age limit (e.g., 70) up to which the cumulative 
#'   rate is calculated.
#' @param agewidth Integer, width of the age intervals (e.g., 5 for 5-year
#'    bands).
#' @param sep_zero Logical, whether the 0–1 age group is separated (i.e., 
#'   age groups are 0, 1–4, 5–9, ...). Default is \code{TRUE}.
#' @param mp Numeric. A multiplier used to scale the final cumulative rate 
#'   (e.g., 100,000 or 1). Default is \code{1}.
#' @param decimal Integer, number of decimal places to round the result.
#'    Default is \code{6}.
#'
#' @return A named numeric value representing the cumulative rate, scaled
#'    by \code{mp}.
#' @export
#'
#' @examples
#' px <- c(
#'   20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
#'   163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
#' )
#' dx <- c(
#'   156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
#'   891, 831, 926, 731, 269
#' )
#' mx <- dx / px
#' cumrate(mx, eage = 70)
#'
cumrate <- function(count,
                    pop,
                    rate = NULL,
                    eage = 70,
                    agewidth = 5,
                    sep_zero = TRUE,
                    mp = 1,
                    decimal = 6) {
  if (is.null(rate)) {
    if (!missing(count) && !missing(pop)) {
      rate <- count / pop
    } else if (!missing(count)) {
      rate <- count
    } else {
      stop("At least one of 'rate' or 'count' must be provided.")
    }
  }
  nn <- length(rate)
  if (sep_zero) {
    age <- c(0, 1, seq(agewidth, (nn - 2) * agewidth, agewidth))
    agewidth <- c(1, agewidth - 1, rep(agewidth, nn - 2))
  } else {
    age <- c(0, seq(agewidth, (nn - 1) * agewidth, agewidth))
    agewidth <- rep(agewidth, nn)
  }
  pos <- which(age == eage)
  cr <- sum(agewidth[1:pos] * rate[1:pos])
  cr <- round(cr * mp, decimal)
  names(cr) <- paste0("Cumulative Rate(1/", mp, ")")
  return(cr)
}

#' Calculate the cumulative risk
#'
#' Converts a cumulative rate to a cumulative risk using the standard 
#' exponential formula. This is commonly used in cancer epidemiology to 
#' estimate the probability of developing or dying from cancer up to a 
#' certain age, under the assumption of constant rates.
#' 
#' @param cumrate Numeric. The cumulative incidence or mortality rate, 
#'   typically calculated using \code{\link{cumrate}}.
#' @param mp Numeric. The rate multiplier used in \code{cumrate}. 
#'   This is used for labeling purposes only. Default is \code{100}.
#' @param decimal Integer. Number of decimal places to round the result. 
#'   Default is \code{2}.
#'
#' @return A named numeric value representing the cumulative risk
#'    (as a percentage).
#' @details
#' The cumulative risk is calculated as:
#' \deqn{1 - \exp(-\text{cumrate})}
#' This converts the cumulative rate to a probability, assuming the event 
#' rate is constant over each age interval and the competing risks are ignored.
#' @export
#'
#' @examples
#' px <- c(
#'   20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
#'   163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
#' )
#' dx <- c(
#'   156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
#'   891, 831, 926, 731, 269
#' )
#' mx <- dx / px
#' cumrate(mx, eage = 70)
#' cumrisk(cumrate(mx, eage = 70))
#'
cumrisk <- function(cumrate, mp = 100, decimal = 2) {
  risk <- round((1 - exp(-cumrate)) * 100, decimal)
  names(risk) <- rep(paste0("Cumulative Risk (1/", mp, ")"), length(cumrate))
  return(risk)
}
