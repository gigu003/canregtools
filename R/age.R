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
cutage <- function(
  x,
  method = "distance",
  length = 5,
  maxage = 85,
  sep_zero = TRUE,
  breaks = c(seq(0, 85, 5)),
  labels = NULL,
  lang = "cn",
  label_tail = NULL,
  right = FALSE
) {
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
    breaks <- stats::quantile(x, probs = seq(0, 1, 0.25))
    age_groups <- cut(x, breaks, right = right)
  } else {
    stop(
      "Invalid method. Supported methods are
         'interval', 'distance', and 'quantile'."
    )
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
  age <- rep(NA_real_, length(birth_date))

  # Identify non-NA values
  valid_idx <- !(is.na(birth_date) | is.na(onset_date))

  # Convert each date vector once instead of formatting six components.
  birth <- as.POSIXlt(birth_date[valid_idx], tz = "UTC")
  onset <- as.POSIXlt(onset_date[valid_idx], tz = "UTC")
  before_birthday <- onset$mon < birth$mon |
    (onset$mon == birth$mon & onset$mday < birth$mday)
  age[valid_idx] <- as.numeric(onset$year - birth$year - before_birthday)
  age
}

#' Expand grouped population to single-year ages while preserving an open group
#'
#' `expand_age_pop()` converts any set of closed age groups to single-year
#' estimates and keeps the final source group as an actual open age group. It
#' never represents an open group as a fabricated sequence of high ages. Every
#' effective source-group total is constrained to its input total exactly, and
#' estimates are non-negative.
#'
#' @param x Numeric population totals. Its length must equal
#'   `length(source_breaks)`; the last value is the open age group.
#' @param source_breaks Integer lower bounds of the input age groups. The last
#'   bound starts the open group. The default is 0, 1--4, 5--9, ..., 85+.
#' @param method Splitting method: `"linear"`, `"constant"`, `"fmm"`,
#'   `"natural"`, `"monoh.fc"`, `"reference"`, `"sprague"`, or `"pclm"`.
#'   The spline options are retained for compatibility and apply only to closed
#'   groups. `"reference"` uses an external single-age schedule. `"sprague"`
#'   requires regular closed five-year groups; the default 0 and 1--4 groups
#'   are automatically combined to 0--4. `"pclm"` requires `ungroup`.
#' @param reference An external single-age schedule for `method = "reference"`.
#'   Supply a named numeric vector (names are ages), or a data frame with
#'   `age`/`x` and `population`/`pop`/`y` columns. It must cover every closed
#'   age to be split. It should be independent of data evaluated in a blind
#'   validation.
#' @param high_age A character method or list supplied to [expand_open_age_pop()].
#'   The default, `"keep"`, retains the source open group. Use a list with
#'   `method = "reference"`, `reference`, and `max_age` only when an external
#'   high-age schedule is available.
#' @param pclm_control A list passed to `ungroup::pclm(control = ...)`.
#'
#' @return A data frame with `x` (age lower bound), `y` (population), `age_end`,
#'   `open`, `source_group`, `lower`, and `upper`. Closed rows have
#'   `age_end == x`; the final open row has `age_end = Inf`. PCLM uncertainty
#'   intervals are returned when supplied by `ungroup`; other methods report
#'   missing intervals. Attributes record the effective `source_breaks`, method,
#'   and uncertainty method.
#'
#' @references
#' Sprague, T. B. (1880). Explanation of a new formula for interpolation.
#' *Journal of the Institute of Actuaries*, 22, 270--285.
#'
#' Rizzi, S., Gampe, J., and Eilers, P. H. C. (2015). Efficient ungrouping of
#'   coarse histograms and age heaping in demographic data. *Statistical
#'   Methods in Medical Research*, 24(3), 291--313.
#'
#' @export
#'
#' @examples
#' population <- c(
#'   5053, 17743, 25541, 32509, 30530, 34806, 36846, 38691, 40056,
#'   39252, 37349, 30507, 26363, 21684, 15362, 11725, 7461, 3260, 915
#' )
#' expand_age_pop(population, method = "sprague")
expand_age_pop <- function(
  x,
  source_breaks = c(0, 1, seq(5, 85, 5)),
  method = "linear",
  reference = NULL,
  high_age = "keep",
  pclm_control = list()
) {
  if (is.character(source_breaks) && length(source_breaks) == 1L) {
    if (!missing(method)) {
      stop("Supply `method` once, either by name or as the second argument.")
    }
    method <- source_breaks
    source_breaks <- c(0, 1, seq(5, 85, 5))
  }
  source_breaks <- validate_age_breaks(source_breaks)
  x <- validate_age_pop_input(x, source_breaks)
  method <- match.arg(
    tolower(method),
    c(
      "linear",
      "constant",
      "fmm",
      "natural",
      "monoh.fc",
      "reference",
      "sprague",
      "pclm"
    )
  )

  if (method == "sprague") {
    sprague_input <- normalize_sprague_input(x, source_breaks)
    x <- sprague_input$x
    source_breaks <- sprague_input$source_breaks
  }

  open_lower <- utils::tail(source_breaks, 1)
  closed_ages <- seq.int(source_breaks[1], open_lower - 1L)
  closed_groups <- findInterval(closed_ages, source_breaks)
  closed <- split_closed_age_pop(
    x = x[-length(x)],
    source_breaks = source_breaks[-length(source_breaks)],
    closed_ages = closed_ages,
    closed_groups = closed_groups,
    method = method,
    reference = reference,
    pclm_control = pclm_control
  )
  high_age <- normalize_high_age(high_age)
  open <- do.call(
    expand_open_age_pop,
    c(
      list(x = utils::tail(x, 1), lower = open_lower),
      high_age
    )
  )

  result <- data.frame(
    x = c(closed_ages, open$x),
    y = c(closed$y, open$y),
    age_end = c(closed_ages, open$age_end),
    open = c(rep(FALSE, length(closed_ages)), open$open),
    source_group = c(closed_groups, rep(length(x), nrow(open))),
    lower = c(closed$lower, open$lower),
    upper = c(closed$upper, open$upper)
  )
  attr(result, "source_breaks") <- source_breaks
  attr(result, "method") <- method
  attr(result, "uncertainty_method") <- closed$uncertainty_method
  result
}

#' Expand one open-ended population age group
#'
#' This high-age module is deliberately separate from ordinary interpolation.
#' By default it keeps the open group intact. Reference-based splitting needs a
#' high-age schedule and always retains a final open group at `max_age`.
#'
#' @param x Non-negative total of the open group.
#' @param lower Lower bound of the open group.
#' @param method Either `"keep"` or `"reference"`.
#' @param reference Reference single-age schedule required for `"reference"`.
#' @param max_age Final lower bound for an open group after reference splitting.
#'
#' @return A data frame with `x`, `y`, `age_end`, `open`, `lower`, and `upper`.
#' @export
expand_open_age_pop <- function(
  x,
  lower,
  method = c("keep", "reference"),
  reference = NULL,
  max_age = NULL
) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    stop("`x` must be one finite, non-negative open-group population total.")
  }
  if (
    !is.numeric(lower) ||
      length(lower) != 1L ||
      lower < 0 ||
      lower != round(lower)
  ) {
    stop("`lower` must be one non-negative integer age.")
  }
  method <- match.arg(method)
  if (method == "keep") {
    return(data.frame(
      x = lower,
      y = x,
      age_end = Inf,
      open = TRUE,
      lower = NA_real_,
      upper = NA_real_
    ))
  }
  if (
    is.null(max_age) ||
      !is.numeric(max_age) ||
      length(max_age) != 1L ||
      max_age <= lower ||
      max_age != round(max_age)
  ) {
    stop(
      "`max_age` must be an integer greater than `lower` for reference splitting."
    )
  }

  schedule <- as_age_schedule(reference)
  closed_ages <- seq.int(lower, max_age - 1L)
  if (!all(closed_ages %in% schedule$age) || !any(schedule$age >= max_age)) {
    stop(
      "`reference` must cover all closed high ages and the final open group."
    )
  }
  weights <- c(
    schedule$population[match(closed_ages, schedule$age)],
    sum(schedule$population[schedule$age >= max_age])
  )
  values <- reconcile_age_pop(weights, x, rep(1L, length(weights)))
  data.frame(
    x = c(closed_ages, max_age),
    y = values,
    age_end = c(closed_ages, Inf),
    open = c(rep(FALSE, length(closed_ages)), TRUE),
    lower = NA_real_,
    upper = NA_real_
  )
}

split_closed_age_pop <- function(
  x,
  source_breaks,
  closed_ages,
  closed_groups,
  method,
  reference,
  pclm_control
) {
  widths <- diff(c(source_breaks, max(closed_ages) + 1L))
  midpoints <- source_breaks + (widths - 1) / 2
  uncertainty <- list(
    lower = rep(NA_real_, length(closed_ages)),
    upper = rep(NA_real_, length(closed_ages)),
    method = "not available"
  )

  if (method == "reference") {
    raw <- reference_age_values(reference, closed_ages)
  } else if (method == "sprague") {
    raw <- as.vector(sprague_coefficients_closed(length(x)) %*% x)
  } else if (method == "pclm") {
    pclm <- split_closed_age_pop_pclm(source_breaks, x, widths, pclm_control)
    raw <- pclm$y
    uncertainty <- pclm$uncertainty
  } else {
    rates <- x / widths
    if (length(x) == 1L) {
      raw <- rep(rates, widths)
    } else if (method %in% c("linear", "constant")) {
      raw <- stats::approx(
        midpoints,
        rates,
        xout = closed_ages,
        method = method,
        rule = 2
      )$y
    } else {
      raw <- stats::spline(
        midpoints,
        rates,
        xout = closed_ages,
        method = if (method == "monoh.fc") "hyman" else method
      )$y
    }
  }

  if (any(raw < 0, na.rm = TRUE)) {
    # Cubic interpolation can overshoot for irregular age profiles. Fall back
    # to linear interpolation, which is non-negative for non-negative inputs,
    # before source-group reconciliation.
    rates <- x / widths
    raw <- stats::approx(
      midpoints,
      rates,
      xout = closed_ages,
      method = "linear",
      rule = 2
    )$y
  }
  y <- reconcile_age_pop(
    pmax(replace(raw, !is.finite(raw), 0), 0),
    x,
    closed_groups
  )
  list(
    y = y,
    lower = uncertainty$lower,
    upper = uncertainty$upper,
    uncertainty_method = uncertainty$method
  )
}

split_closed_age_pop_pclm <- function(source_breaks, x, widths, control) {
  if (!requireNamespace("ungroup", quietly = TRUE)) {
    stop(
      "`method = \"pclm\"` requires the optional package `ungroup`. ",
      "Install it with install.packages(\"ungroup\")."
    )
  }
  fit <- ungroup::pclm(
    x = source_breaks,
    y = x,
    nlast = utils::tail(widths, 1),
    control = control
  )
  if (length(fit$fitted) != sum(widths)) {
    stop("PCLM returned an unexpected number of single-age estimates.")
  }
  ci <- extract_pclm_ci(fit$ci, length(fit$fitted))
  list(
    y = fit$fitted,
    uncertainty = list(
      lower = ci$lower,
      upper = ci$upper,
      method = if (all(is.na(ci$lower))) {
        "not available"
      } else {
        "PCLM confidence interval"
      }
    )
  )
}

extract_pclm_ci <- function(ci, n) {
  empty <- list(lower = rep(NA_real_, n), upper = rep(NA_real_, n))
  if (is.null(ci)) {
    return(empty)
  }
  ci <- as.data.frame(ci)
  lower_name <- intersect(c("lower", "lwr", "lo"), tolower(names(ci)))
  upper_name <- intersect(c("upper", "upr", "hi"), tolower(names(ci)))
  if (!length(lower_name) || !length(upper_name) || nrow(ci) != n) {
    return(empty)
  }
  names(ci) <- tolower(names(ci))
  list(lower = ci[[lower_name[1]]], upper = ci[[upper_name[1]]])
}

normalize_high_age <- function(high_age) {
  if (is.character(high_age) && length(high_age) == 1L) {
    return(list(method = high_age))
  }
  if (!is.list(high_age) || is.null(high_age$method)) {
    stop("`high_age` must be a method string or a list containing `method`.")
  }
  high_age
}

validate_age_breaks <- function(source_breaks) {
  if (
    !is.numeric(source_breaks) ||
      length(source_breaks) < 2L ||
      any(!is.finite(source_breaks)) ||
      source_breaks[1] != 0 ||
      any(source_breaks != round(source_breaks)) ||
      any(diff(source_breaks) <= 0)
  ) {
    stop(
      "`source_breaks` must be strictly increasing integer lower bounds that ",
      "start at 0 and include a final open-group lower bound."
    )
  }
  as.integer(source_breaks)
}

validate_age_pop_input <- function(x, source_breaks) {
  if (!is.numeric(x) || length(x) != length(source_breaks)) {
    stop(
      "`x` must be numeric and have one value for every `source_breaks` value."
    )
  }
  x <- as.numeric(replace(x, is.na(x), 0))
  if (any(!is.finite(x)) || any(x < 0)) {
    stop("`x` must contain finite, non-negative population counts.")
  }
  x
}

normalize_sprague_input <- function(x, source_breaks) {
  default_young <- length(source_breaks) >= 3L &&
    identical(source_breaks[1:3], c(0L, 1L, 5L)) &&
    all(diff(source_breaks[-2]) == 5L)
  if (default_young) {
    x <- c(sum(x[1:2]), x[-c(1, 2)])
    source_breaks <- c(0L, source_breaks[-c(1, 2)])
  }
  if (!all(diff(source_breaks) == 5L) || length(x) < 6L) {
    stop(
      "`method = \"sprague\"` requires at least five closed, regular five-year ",
      "groups and a final open group."
    )
  }
  list(x = x, source_breaks = source_breaks)
}

as_age_schedule <- function(reference) {
  if (is.numeric(reference) && !is.null(names(reference))) {
    out <- data.frame(
      age = suppressWarnings(as.integer(names(reference))),
      population = as.numeric(reference)
    )
  } else if (is.data.frame(reference)) {
    age_name <- intersect(c("age", "x"), names(reference))
    value_name <- intersect(
      c("population", "pop", "y", "rks"),
      names(reference)
    )
    if (!length(age_name) || !length(value_name)) {
      stop("`reference` needs age/x and population/pop/y/rks columns.")
    }
    out <- data.frame(
      age = as.integer(reference[[age_name[1]]]),
      population = as.numeric(reference[[value_name[1]]])
    )
  } else {
    stop("`reference` must be a named numeric vector or a data frame.")
  }
  if (
    anyNA(out$age) ||
      any(out$age < 0) ||
      anyDuplicated(out$age) ||
      any(!is.finite(out$population)) ||
      any(out$population < 0)
  ) {
    stop(
      "`reference` must have unique non-negative ages and finite non-negative values."
    )
  }
  out[order(out$age), , drop = FALSE]
}

reference_age_values <- function(reference, ages) {
  schedule <- as_age_schedule(reference)
  if (!all(ages %in% schedule$age)) {
    stop("`reference` must cover every closed age in `source_breaks`.")
  }
  schedule$population[match(ages, schedule$age)]
}

sprague_coefficients_closed <- function(n_group) {
  if (n_group < 5L) {
    stop("Sprague splitting needs at least five closed five-year groups.")
  }
  first_two <- matrix(
    c(
      0.3616,
      -0.2768,
      0.1488,
      -0.0336,
      0,
      0.2640,
      -0.0960,
      0.0400,
      -0.0080,
      0,
      0.1840,
      0.0400,
      -0.0320,
      0.0080,
      0,
      0.1200,
      0.1360,
      -0.0720,
      0.0160,
      0,
      0.0704,
      0.1968,
      -0.0848,
      0.0176,
      0,
      0.0336,
      0.2272,
      -0.0752,
      0.0144,
      0,
      0.0080,
      0.2320,
      -0.0480,
      0.0080,
      0,
      -0.0080,
      0.2160,
      -0.0080,
      0,
      0,
      -0.0160,
      0.1840,
      0.0400,
      -0.0080,
      0,
      -0.0176,
      0.1408,
      0.0912,
      -0.0144,
      0
    ),
    nrow = 10,
    byrow = TRUE
  )
  middle <- matrix(
    c(
      -0.0128,
      0.0848,
      0.1504,
      -0.0240,
      0.0016,
      -0.0016,
      0.0144,
      0.2224,
      -0.0416,
      0.0064,
      0.0064,
      -0.0336,
      0.2544,
      -0.0336,
      0.0064,
      0.0064,
      -0.0416,
      0.2224,
      0.0144,
      -0.0016,
      0.0016,
      -0.0240,
      0.1504,
      0.0848,
      -0.0128
    ),
    nrow = 5,
    byrow = TRUE
  )
  last_two <- matrix(
    c(
      0,
      -0.0144,
      0.0912,
      0.1408,
      -0.0176,
      0,
      -0.0080,
      0.0400,
      0.1840,
      -0.0160,
      0,
      0,
      -0.0080,
      0.2160,
      -0.0080,
      0,
      0.0080,
      -0.0480,
      0.2320,
      0.0080,
      0,
      0.0144,
      -0.0752,
      0.2272,
      0.0336,
      0,
      0.0176,
      -0.0848,
      0.1968,
      0.0704,
      0,
      0.0160,
      -0.0720,
      0.1360,
      0.1200,
      0,
      0.0080,
      -0.0320,
      0.0400,
      0.1840,
      0,
      -0.0080,
      0.0400,
      -0.0960,
      0.2640,
      0,
      -0.0336,
      0.1488,
      -0.2768,
      0.3616
    ),
    nrow = 10,
    byrow = TRUE
  )

  middle_panels <- n_group - 4L
  coefficient <- matrix(0, nrow = n_group * 5L, ncol = n_group)
  coefficient[1:10, 1:5] <- first_two
  for (i in seq_len(middle_panels)) {
    rows <- 10 + (i - 1) * 5 + seq_len(5)
    columns <- i + seq_len(5) - 1
    coefficient[rows, columns] <- middle
  }
  coefficient[
    (nrow(coefficient) - 9L):nrow(coefficient),
    (n_group - 4L):n_group
  ] <- last_two
  coefficient
}

reconcile_age_pop <- function(interpolated, grouped, age_group) {
  expanded <- numeric(length(interpolated))
  for (i in seq_along(grouped)) {
    index <- which(age_group == i)
    target <- grouped[i]
    if (target == 0) {
      next
    }

    weights <- interpolated[index]
    if (sum(weights) == 0) {
      weights <- rep(1, length(index))
    }
    values <- weights / sum(weights) * target
    if (abs(target - round(target)) < sqrt(.Machine$double.eps)) {
      whole <- floor(values)
      remainder <- as.integer(round(target - sum(whole)))
      if (remainder > 0L) {
        order_index <- order(values - whole, decreasing = TRUE)
        whole[order_index[seq_len(remainder)]] <-
          whole[order_index[seq_len(remainder)]] + 1
      }
      values <- whole
    }
    expanded[index] <- values
  }
  expanded
}

#' Blindly benchmark age-splitting methods against a single-age schedule
#'
#' Aggregate an independent single-age population schedule to `source_breaks`,
#' restore it with each method, and compare the restored schedule with the
#' held-out source. This provides a reproducible validation workflow for an
#' official single-age population series without allowing the target series to
#' be used as a `"reference"` schedule.
#'
#' @param population Held-out single-age population schedule. Supply a named
#'   numeric vector or a data frame accepted by [expand_age_pop()]. Ages at or
#'   above the final `source_breaks` value are treated as the open group.
#' @param source_breaks Lower bounds used to blind the held-out schedule.
#' @param methods Character vector of methods passed to [expand_age_pop()].
#' @param reference Independent reference schedule used only by the
#'   `"reference"` method.
#' @param cases Optional held-out single-age case schedule. If supplied,
#'   age-standardized-rate bias is calculated using the restored population.
#' @param stdpop Optional single-age standard-population weights for ASR.
#'   Equal weights are used when `cases` is supplied without `stdpop`.
#' @param mp Multiplier for the ASR, defaulting to 100,000.
#' @param high_age High-age configuration passed to [expand_age_pop()].
#' @param pclm_control PCLM control settings passed to [expand_age_pop()].
#'
#' @return A data frame with MAE, RMSE, total variation distance, ASR bias,
#'   maximum source-group error, number of negative values, and available
#'   uncertainty-interval diagnostics. The restored schedules are stored in
#'   the `estimates` attribute; the error for every effective source group is
#'   stored in the `group_errors` attribute.
#' @export
benchmark_age_split <- function(
  population,
  source_breaks = c(0, 1, seq(5, 85, 5)),
  methods = c("linear", "sprague"),
  reference = NULL,
  cases = NULL,
  stdpop = NULL,
  mp = 100000,
  high_age = "keep",
  pclm_control = list()
) {
  source_breaks <- validate_age_breaks(source_breaks)
  truth <- schedule_to_open_groups(population, source_breaks)
  grouped <- aggregate_schedule_to_breaks(truth, source_breaks)
  case_truth <- if (is.null(cases)) {
    NULL
  } else {
    schedule_to_open_groups(cases, source_breaks)
  }
  std_truth <- if (is.null(stdpop)) {
    data.frame(x = truth$x, y = rep(1, nrow(truth)))
  } else {
    schedule_to_open_groups(stdpop, source_breaks)
  }

  estimates <- vector("list", length(methods))
  names(estimates) <- methods
  group_errors <- vector("list", length(methods))
  names(group_errors) <- methods
  metrics <- lapply(methods, function(method) {
    estimate <- expand_age_pop(
      x = grouped,
      source_breaks = source_breaks,
      method = method,
      reference = reference,
      high_age = high_age,
      pclm_control = pclm_control
    )
    estimates[[method]] <<- estimate
    comparable <- expansion_to_open_groups(estimate, source_breaks)
    difference <- comparable$y - truth$y
    effective_breaks <- attr(estimate, "source_breaks")
    expected_groups <- aggregate_schedule_to_breaks(truth, effective_breaks)
    observed_groups <- aggregate_schedule_to_breaks(
      comparable,
      effective_breaks
    )
    group_error_values <- observed_groups - expected_groups
    tolerance <- sqrt(.Machine$double.eps) * max(1, max(expected_groups))
    group_error_values[abs(group_error_values) <= tolerance] <- 0
    group_error <- max(abs(group_error_values))
    group_errors[[method]] <<- data.frame(
      method = method,
      age_lower = effective_breaks,
      error = group_error_values
    )
    interval <- uncertainty_diagnostics(estimate, truth, source_breaks)
    asr <- age_split_asr(case_truth, truth, comparable, std_truth, mp)
    data.frame(
      method = method,
      mae = mean(abs(difference)),
      rmse = sqrt(mean(difference^2)),
      total_variation = total_variation_distance(comparable$y, truth$y),
      asr_true = asr$truth,
      asr_estimate = asr$estimate,
      asr_bias = asr$estimate - asr$truth,
      max_group_error = group_error,
      n_negative = sum(estimate$y < 0),
      uncertainty_method = attr(estimate, "uncertainty_method"),
      interval_coverage = interval$coverage,
      mean_interval_width = interval$width
    )
  })
  result <- do.call(rbind, metrics)
  attr(result, "estimates") <- estimates
  attr(result, "group_errors") <- do.call(rbind, group_errors)
  result
}

schedule_to_open_groups <- function(schedule, source_breaks) {
  schedule <- as_age_schedule(schedule)
  closed_ages <- seq.int(source_breaks[1], utils::tail(source_breaks, 1) - 1L)
  if (
    !all(closed_ages %in% schedule$age) ||
      !any(schedule$age >= utils::tail(source_breaks, 1))
  ) {
    stop(
      "The held-out schedule must cover every closed age and the open group."
    )
  }
  data.frame(
    x = c(closed_ages, utils::tail(source_breaks, 1)),
    y = c(
      schedule$population[match(closed_ages, schedule$age)],
      sum(schedule$population[schedule$age >= utils::tail(source_breaks, 1)])
    )
  )
}

expansion_to_open_groups <- function(expansion, source_breaks) {
  closed_ages <- seq.int(source_breaks[1], utils::tail(source_breaks, 1) - 1L)
  if (!all(closed_ages %in% expansion$x)) {
    stop("The expansion does not contain every closed single age.")
  }
  data.frame(
    x = c(closed_ages, utils::tail(source_breaks, 1)),
    y = c(
      expansion$y[match(closed_ages, expansion$x)],
      sum(expansion$y[expansion$x >= utils::tail(source_breaks, 1)])
    )
  )
}

aggregate_schedule_to_breaks <- function(schedule, source_breaks) {
  vapply(
    seq_along(source_breaks),
    function(i) {
      in_group <- if (i == length(source_breaks)) {
        schedule$x >= source_breaks[i]
      } else {
        schedule$x >= source_breaks[i] & schedule$x < source_breaks[i + 1L]
      }
      sum(schedule$y[in_group])
    },
    numeric(1)
  )
}

total_variation_distance <- function(x, y) {
  if (sum(x) == 0 && sum(y) == 0) {
    return(0)
  }
  if (sum(x) == 0 || sum(y) == 0) {
    return(1)
  }
  sum(abs(x / sum(x) - y / sum(y))) / 2
}

uncertainty_diagnostics <- function(expansion, truth, source_breaks) {
  closed <- expansion$x < utils::tail(source_breaks, 1) &
    is.finite(expansion$lower) &
    is.finite(expansion$upper)
  if (!any(closed)) {
    return(list(coverage = NA_real_, width = NA_real_))
  }
  actual <- truth$y[match(expansion$x[closed], truth$x)]
  list(
    coverage = mean(
      actual >= expansion$lower[closed] & actual <= expansion$upper[closed]
    ),
    width = mean(expansion$upper[closed] - expansion$lower[closed])
  )
}

age_split_asr <- function(
  cases,
  truth_population,
  estimate_population,
  stdpop,
  mp
) {
  if (is.null(cases)) {
    return(list(truth = NA_real_, estimate = NA_real_))
  }
  if (
    !identical(cases$x, truth_population$x) ||
      !identical(cases$x, estimate_population$x) ||
      !identical(cases$x, stdpop$x)
  ) {
    stop(
      "Cases, population, and standard weights must share the same age groups."
    )
  }
  rate <- function(population) {
    if (any(population$y == 0 & cases$y > 0)) {
      return(NA_real_)
    }
    age_rate <- ifelse(population$y == 0, 0, cases$y / population$y)
    sum(age_rate * stdpop$y) / sum(stdpop$y) * mp
  }
  list(truth = rate(truth_population), estimate = rate(estimate_population))
}
