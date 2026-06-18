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
#'
#' pop1 <- c(
#'   59546, 294129, 472511, 552549, 821119, 996436, 805635, 1004506,
#'   989357, 1056612, 986559, 792270, 544544, 452297, 473579, 350802,
#'   212614, 109598, 61990
#' )
#' pop2 <- c(
#'   75641, 377276, 327116, 380338, 539034, 1158852, 1152329, 881443,
#'   903484, 1011164, 1238871, 1137832, 1022787, 645441, 464777,
#'   482941, 406144, 227977, 144526
#' )
#' esti_pop(pop1, pop2, c(2000, 2010))
#'
esti_pop <- function(pop1, pop2, period) {
  # Calculate proportions
  pop1_prop <- pop1 / sum(pop1)
  pop2_prop <- pop2 / sum(pop2)
  
  # Years sequence
  years <- seq(period[1], period[2], 1)
  n_years <- length(years)
  
  # Per-year increment (slope)
  diff_prop <- pop2_prop - pop1_prop
  increment <- diff_prop / (period[2] - period[1])
  
  # Vectorized interpolation: matrix of proportions (rows: age groups, columns: years)
  props <- outer(increment, 0:(n_years - 1), "*") + pop1_prop
  
  # Convert to data frame and set column names
  res <- as.data.frame(props)
  colnames(res) <- years
  res
}
