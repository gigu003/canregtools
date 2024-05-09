#' Estimating population structure using interpolation method.
#'
#' @param pop1 Population or population proportion in each age group for the
#'             start year.
#' @param pop2 Population or population proportion in each age group for the
#'             end year.
#' @param period  Vector contain the start year and end year value.
#'
#' @return A data frame contain the estimated population proportion in each
#'         year during the period with each year in one column and each age
#'         group in one row.
#' @export
#'
#' @examples
#' pop1 <- c(59546, 294129, 472511, 552549, 821119, 996436, 805635, 1004506,
#'           989357, 1056612, 986559, 792270, 544544, 452297, 473579, 350802,
#'           212614, 109598, 61990)
#' pop2 <- c(75641, 377276, 327116, 380338, 539034, 1158852, 1152329, 881443,
#'           903484, 1011164, 1238871, 1137832, 1022787, 645441, 464777,
#'           482941, 406144, 227977, 144526)
#' estimate_pop(pop1, pop2, c(2000, 2010))
estimate_pop <- function(pop1, pop2, period){
  #calculate the proportion of the population in each age group.
  pop1 <- pop1/sum(pop1)
  pop2 <- pop2/sum(pop2)
  #transform the population data.
  pop <- lapply(seq_along(pop1), function(i) c(pop1[i], pop2[i]))
  res <- lapply(seq_along(pop), function(i) {
    res <- approx(period, pop[[i]], xout = seq(period[1], period[2], 1))
    res <- res$y
    return(res)})
  res <- as.data.frame(matrix(unlist(res),
                              ncol= (period[2] - period[1]+1), byrow = T))
  colnames(res) <- seq(period[1], period[2], 1)
  return(res)
}
