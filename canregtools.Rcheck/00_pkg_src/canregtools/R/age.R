#' Group ages into categories
#'
#' Groups numeric age values into categorized age bands using one of three
#' methods: fixed interval (`"interval"`), equal distance (`"distance"`),
#' or quantile-based grouping (`"quantile"`). Supports flexible labeling and
#' language-specific suffixes.
#' 
#' @param x Numeric vector of ages.
#' @param method Character. Grouping method: `"interval"` (custom breaks), 
#'   `"distance"` (uniform width), or `"quantile"` (equal-sized groups).
#' @param length Integer. Width of age bands (used only
#'    if `method = "distance"`). Default is 5.
#' @param maxage Numeric. Upper limit of the age range (used in `"distance"`
#'    method).
#' @param sep_zero Logical. Whether to separate age 0 into its own group
#'    (only used if `method = "distance"`). Default is TRUE.
#' @param breaks Numeric vector of breakpoints (required if
#'    `method = "interval"`).
#' @param labels Character vector of labels for resulting age groups.
#'    If `NULL`, default interval-style labels are generated.
#' @param label_tail Character string appended to labels, e.g., `"yrs"`.
#'    Default depends on `lang`.
#' @param right Logical. Whether intervals are right-closed. Passed to `cut()`.
#'    Default is FALSE.
#' @param lang Output language for default labels, `"cn"` (Chinese)
#'    or `"en"` (English). Default is `"cn"`.
#'
#' @return A factor variable of age groups with labeled levels.
#' @export
#'
#' @examples
#' ages <- sample(0:101, 200, replace = TRUE)
#' cutage(ages, method = "distance", length = 5, maxage = 60, sep_zero = TRUE)
#' # Custom breaks
#' cutage(ages, method = "interval", breaks = c(0, 15, 30, 45, 60, 75, Inf))
#' # Quantile-based grouping
#' cutage(ages, method = "quantile")
#' 
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

#' Calculate the actual age in completed years between two dates
#'
#' Computes the exact age in full years at the time of an event
#' (e.g., diagnosis, death, or survey) by comparing a person's birth date
#' to the date of the event.
#'
#' @param birth_date A vector of birth dates in `Date` format.
#' @param onset_date A vector of corresponding event dates in `Date` format
#'    (e.g., date of diagnosis).
#'
#' @details
#' This function calculates age in *completed years*, taking into account
#' whether the birthday has occurred before the event date in the given year.
#' If `birth_date` or `onset_date` contains `NA`,
#' the corresponding result will be `NA`.
#'
#' @return A numeric vector of ages in years, with the same length
#'    as `birth_date` and `onset_date`.
#' 
#' @export
#'
#' @examples
#' # Generate random birth dates
#' set.seed(123)
#' sdate <- as.Date("1960-01-01")
#' edate <- as.Date("1980-12-31")
#' bdate <- sample(seq(sdate, edate, by = "1 day"), 100, replace = TRUE)
#'
#' # Generate random event dates
#' sdate <- as.Date("2020-01-01")
#' edate <- as.Date("2023-07-08")
#' event <- sample(seq(sdate, edate, by = "1 day"), 100, replace = TRUE)
#'
#' # Calculate ages
#' ages <- calc_age(bdate, event)
#' head(ages)
#' # Handle missing values
#' bdate[1] <- NA
#' event[2] <- NA
#' calc_age(bdate, event)[1:5]
#' 
calc_age <- function(birth_date, onset_date) {
  # Ensure both inputs are Date objects
  birth_date <- as.Date(birth_date)
  onset_date <- as.Date(onset_date)
  # Check for length mismatch
  if (length(birth_date) != length(onset_date)) {
    stop("Length of 'birth_date' and 'onset_date' must be equal.")
  }
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
  age
}

#' Expand population data from 5-year age groups to single-year ages
#' 
#' `expand_age_pop` transforms population data aggregated in age groups into
#' estimates for single-year ages. It utilizes interpolation methods to
#' distribute the grouped data across individual ages, ensuring consistency
#' with the original totals.
#'
#' @param x A numeric vector representing the population counts for each age
#'    group. The vector should have 19 elements corresponding to the following
#'    age groups: 0, 1–4, 5–9, ..., 85+.
#' @param method A character string specifying the interpolation method to use.
#'    Options include:
#' \itemize{
#'   \item \code{"linear"}: Linear interpolation.
#'   \item \code{"constant"}: Constant interpolation.
#'   \item \code{"periodic"}: Periodic spline interpolation.
#'   \item \code{"natural"}: Natural spline interpolation.
#' }
#' The default is \code{"linear"}.
#' 
#' @importFrom stats aggregate approx quantile spline
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{\code{x}}{Integer ages from 0 to 92.}
#'   \item{\code{y}}{Estimated population counts for each single-year age.}
#' }
#' 
#' @export
#'
#' @examples
#' # Example population data for 19 age groups: 0, 1–4, 5–9, ..., 85+
#' ages <- c(
#'   5053, 17743, 25541, 32509, 30530, 34806, 36846, 38691, 40056,
#'   39252, 37349, 30507, 26363, 21684, 15362, 11725, 7461, 3260, 915
#' )
#' eages <- expand_age_pop(ages)
#' head(eages)
expand_age_pop <- function(x, method = "linear") {
  x <- replace(x, is.na(x), 0)
  if (!length(x) == 19) {
    x <- rep(0, 19)
  }
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

