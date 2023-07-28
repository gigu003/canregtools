#' Cut age into groups.
#'
#' @param x Vector contains the ages.
#' @param method The method to use for age grouping.
#'        Options are "interval", "distance", or "quantile".
#' @param length The length of intervals for age grouping when method is
#'        set to "distance".
#' @param maxage The maximum age for age grouping.
#' @param sep_zero A logical value indicating whether to include a separate
#'        group for age 0.
#' @param breaks Custom breakpoints for the "interval" method.
#' @param labels labels for the levels of the resulting category.
#'        By default, labels are constructed using "(a,b]" interval notation.
#'        If labels = FALSE, simple integer codes are returned instead of
#'        a factor.
#' @param label_tail A string to be appended at the end of each label.
#' @param right A logical value indicating whether the intervals are
#'        right-closed or right-open.
#'
#' @return Factor of age groups.
#' @export
#'
#' @examples
#' library(canregtools)
#' age <- sample(0:101, 200, replace = TRUE)
#' agegrp <- cutage(age, method = "distance", length = 10, maxage = 60, sep_zero = T)
cutage <- function(x,
                   method = "interval",
                   length = 5,
                   maxage = 85,
                   sep_zero = TRUE,
                   breaks = c(seq(0, 85, 5)),
                   labels = NULL,
                   label_tail = "yrs",
                   right = FALSE){
  x <- as.numeric(x)
  if (method == "interval") {
    if (is.null(breaks)) {
      stop("For interval grouping, 'breaks' argument must be provided.")
    }
    if (right) {breaks <- unique(c(breaks, -Inf, Inf))}
    else {breaks <- unique(c(breaks, Inf))}
    age_groups <- cut(x, breaks = breaks, labels= labels, right = right)
  } else if (method == "distance") {
    if (sep_zero) {
      breaks <- c(0, 1, seq(length, maxage, by = length), Inf)
    } else {
      breaks <- c(seq(0, maxage, by = length), Inf)
    }
    age_groups <- cut(x, breaks = breaks, labels = labels, right =right)
  } else if (method == "quantile") {
    breaks <- quantile(x, probs = seq(0, 1, 0.25))
    age_groups <- cut(x, breaks, right = right)
  } else {
    stop("Invalid method. Supported methods are
         'interval', 'distance', and 'quantile'.")
  }
  
  if (is.null(labels) & !is.null(age_groups)) {
    labels <- sprintf(paste("%s-%s",label_tail),
                      as.character(breaks[-length(breaks)]),
                      as.character(breaks[-1] - 1))
    labels <- gsub("-Inf", "+", labels)
    labels <- gsub("0-0", "0", labels)
    levels(age_groups) <- labels
  }
  
  return(age_groups)

}