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
                      stdpop = spop,
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
