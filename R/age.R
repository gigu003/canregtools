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
#' @param lang Language used for output. Options are 'cn' or 'en'.
#'        Default is 'cn.'.
#'
#' @return Factor of age groups.
#' @export
#'
#' @examples
#' age <- sample(0:101, 200, replace = TRUE)
#' agegrp <- cutage(age,
#'   method = "distance", length = 5,
#'   maxage = 60, sep_zero = TRUE)
#' agegrp
cutage <- function(x,
                   method = "distance",
                   length = 5,
                   maxage = 85,
                   sep_zero = TRUE,
                   breaks = c(seq(0, 85, 5)),
                   labels = NULL,
                   lang = "cn",
                   label_tail = NULL,
                   right = FALSE) {
  x <- as.numeric(x)
  if (method == "interval") {
    if (is.null(breaks)) {
      stop("For interval grouping, 'breaks' argument must be provided.")
    }
    if (right) {
      breaks <- unique(c(breaks, -Inf, Inf))
    } else {
      breaks <- unique(c(breaks, Inf))
    }
    age_groups <- cut(x, breaks = breaks, labels = labels, right = right)
  } else if (method == "distance") {
    if (sep_zero) {
      breaks <- c(0, 1, seq(length, maxage, by = length), Inf)
    } else {
      breaks <- c(seq(0, maxage, by = length), Inf)
    }
    age_groups <- cut(x, breaks = breaks, labels = labels, right = right)
  } else if (method == "quantile") {
    breaks <- quantile(x, probs = seq(0, 1, 0.25))
    age_groups <- cut(x, breaks, right = right)
  } else {
    stop("Invalid method. Supported methods are
         'interval', 'distance', and 'quantile'.")
  }

  if (is.null(labels) && !is.null(age_groups)) {
    if (is.null(label_tail)) {
      if (tolower(lang) %in% c("en", "eng", "english")) {
        label_tail <- "yrs"
      } else if (tolower(lang) %in% c("cn", "zh-cn", "zh")) {
        label_tail <- "\u5c81"
      } else {
        warning("Unsupported language. Using default labels.")
        label_tail <- "yrs"
      }
    }

    labels <- sprintf(
      paste("%s-%s", label_tail),
      as.character(breaks[-length(breaks)]),
      as.character(breaks[-1] - 1)
    )
    labels <- gsub("-Inf", "+", labels)
    labels <- gsub("0-0", "0", labels)
    levels(age_groups) <- labels
  }

  return(age_groups)
}

#' Calculate the actual age between two dates
#'
#' @param birth_date Birth date.
#' @param onset_date Event date.
#'
#' @return Vector of ages.
#' @export
#'
#' @examples
#' sdate <- as.Date("1960-01-01")
#' edate <- as.Date("1980-12-31")
#' bdate <- sample(seq(sdate, edate, by = "1 day"), 100, replace = TRUE)
#' event <- sample(seq(as.Date("2020-01-01"),
#'                     as.Date("2023-07-08"),
#'                     by = "1 day"),
#'                 100, replace = TRUE)
#' ages <- calc_age(bdate, event)
#' ages
calc_age <- function(birth_date, onset_date) {
  # Ensure both inputs are Date objects
  birth_date <- as.Date(birth_date)
  onset_date <- as.Date(onset_date)
  
  # Initialize age as NA for missing values
  age <- rep(NA, length(birth_date))
  
  # Identify non-NA values
  valid_idx <- !(is.na(birth_date) | is.na(onset_date))
  
  # Calculate age for non-missing values
  birth_year <- as.numeric(format(birth_date[valid_idx], "%Y"))
  onset_year <- as.numeric(format(onset_date[valid_idx], "%Y"))
  birth_month <- as.numeric(format(birth_date[valid_idx], "%m"))
  onset_month <- as.numeric(format(onset_date[valid_idx], "%m"))
  birth_day <- as.numeric(format(birth_date[valid_idx], "%d"))
  onset_day <- as.numeric(format(onset_date[valid_idx], "%d"))
  
  age[valid_idx] <- onset_year - birth_year
  age[valid_idx][onset_month < birth_month | 
                   (onset_month == birth_month & onset_day < birth_day)] <- 
    age[valid_idx][onset_month < birth_month | 
                     (onset_month == birth_month & onset_day < birth_day)] - 1
  
  return(age)
}

#' Expand age groups population.
#'
#' @param x Vector, population for each age group.
#' @param method Method for expanding, options are 'linear', 'constant',
#'        'periodic', or 'natural'. Default is 'linear'.
#'
#' @importFrom stats aggregate approx quantile spline
#'
#' @return Vector contain the expanded age population.
#' @export
#'
#' @examples
#' ages <- c(
#'   5053, 17743, 25541, 32509, 30530, 34806, 36846, 38691, 40056,
#'   39252, 37349, 30507, 26363, 21684, 15362, 11725, 7461, 3260, 915)
#' expand_age_pop(ages)
expand_age_pop <- function(x, method = "linear") {
  x1 <- x
  max <- 92
  x[2] <- x[2] / 4
  x[3:18] <- x[3:18] / 5
  x[19] <- x[19] / 15
  a <- c(0, 3, seq(7, 82, 5), max)

  if (tolower(method) %in% c("linear", "constant")) {
    ds <- approx(a, x, xout = seq(0, max, 1), method = method)
  } else if (tolower(method) %in% c("fmm", "periodic", "natural", "monoh.fc")) {
    ds <- spline(a, x, xout = seq(0, max, 1), method = method)
  }
  cond <- FALSE
  while (!cond) {
    ds$x <- cutage(ds$x, labels = paste(c(0, 1, seq(5, 85, 5))))
    ds$y <- round(ds$y)
    x2 <- aggregate(y ~ x, data = ds, sum)[, 2]
    diff <- round(x2 - x1)
    if (all(diff < 5)) {
      y3 <- ds$y
      y3[c(1, 2, seq(6, 86, 5))] <- y3[c(1, 2, seq(6, 86, 5))] - diff[1:19]
      cond <- TRUE
    } else {
      diff[2] <- round(diff[2] / 4)
      diff[3:18] <- round(diff[3:18] / 5)
      diff[19] <- round(diff[19] / 8)
      diff <- c(
        diff[1], rep(diff[2], 4), rep(diff[3:18], each = 5),
        rep(diff[19], 8)
      )
      y3 <- round(ds$y - diff)
      y3[y3 < 0] <- 0
    }
    x3 <- seq(0, max, 1)
    ds <- data.frame(x = x3, y = y3)
  }
  return(ds)
}
