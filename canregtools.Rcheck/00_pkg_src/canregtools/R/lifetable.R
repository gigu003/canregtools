#' Construct a life table from age-specific mortality rates
#'
#' `lt()` constructs a life table based on a vector of age-specific mortality
#' rates (mx), starting ages of age groups, and specified sex. It calculates
#' standard life table columns including the probability of dying (qx),
#' number of survivors (lx), number of deaths (dx), person-years lived (Lx),
#' total person-years remaining (Tx), and life expectancy (ex).
#' It can also compute a cause-deleted life table if cancer_death is provided.
#'
#' @param death Number of deaths from vital statistics.
#' @param cancer_death Cancer related death.
#' @param pop Average population size.
#' @param mx Numeric vector of age-specific mortality rates.
#' @param sage Numeric vector of starting ages for the age groups,
#'        default is `c(0, 1, seq(5, 85, 5))`.
#' @param sex Character string specifying the sex: "male", "female", or "total"
#' (default is "total").
#' @param cohort The size of the initial cohort (default is 100000).
#' @param qx_method Character string specifying the method used to estiamte the
#'      probability of dying between age x and x + n (qx).
#'
#' @return A data frame with the following columns:
#' \describe{
#' \item{age}{Starting age of each age group.}
#' \item{mx}{Age-specific mortality rate.}
#' \item{qx}{Probability of dying between age x and x+n.}
#' \item{lx}{Number of survivors at exact age x, starting from a radix of cohort.}
#' \item{dx}{Number of deaths between ages x and x+n.}
#' \item{Lx}{Person-years lived between ages x and x+n.}
#' \item{Tx}{Total person-years remaining after age x.}
#' \item{ex}{Life expectancy at exact age x.}
#' }
#' Optionally includes pop, death, and cancer_death if provided.
#'
#' @details
#' The function uses standard demographic formulas to compute life table values.
#' The average number of person-years lived in the interval by those dying in
#' the interval (ax) is estimated based on standard formulas for infant ages,
#' and n/2 for other ages. The calculations assume a starting population (radix) of cohort.
#'
#' If cancer_death is provided, a cause-deleted life table is computed by adjusting mx to exclude cancer deaths.
#'
#' @export
#'
#' @examples
#' pop <- c(3605201, 14795034, 41758253, 44202275, 44834666, 42184137, 40868806,
#' 43408408, 33111965, 16344101, 6336435)
#' death <- c(19538, 2027, 9730, 37218, 71511, 104616, 193514, 450972, 686178,
#' 816715, 963781)
#' cancer_death <- c(80, 328, 924, 1893, 2841, 10917, 33965, 87945, 152843,
#' 169826, 144958)
#' lt(death = death, pop = pop, sage = c(0,1, seq(5, 85, 10)))
#'
lt <- function(
    death = NULL,
    cancer_death = NULL,
    pop = NULL,
    mx = NULL,
    sage = c(0, 1, seq(5, 85, 5)),
    sex = "total",
    cohort = 100000,
    qx_method = "constant"
) {
  
  # Handle input for mx
  if (!is.null(cancer_death)) {
    if (is.null(death) || is.null(pop)) {
      stop("death and pop are required when cancer_death is provided")
    }
    if (length(death) != length(cancer_death) || length(death) != length(pop)) {
      stop("death, cancer_death, and pop must have the same length")
    }
    mx <- (death - cancer_death) / pop
    if (any(mx < 0, na.rm = TRUE)) {
      stop("Adjusted mx cannot be negative")
    }
  } else if (!is.null(death) && !is.null(pop)) {
    if (length(death) != length(pop)) {
      stop("death and pop must have the same length")
    }
    mx <- death / pop
  } else if (is.null(mx)) {
    stop("Provide either mx, or death and pop (and optionally cancer_death)")
  }
  
  # Calculate the number of age groups 'nn'
  nn <- length(mx)
  
  if (length(sage) != nn) {
    stop("`sage` must have the same length as `mx`.")
  }
  
  sage <- as.numeric(sage)
  
  if (any(diff(sage) <= 0)) {
    stop("`sage` must be strictly increasing.")
  }
  
  nx <- c(diff(sage), Inf)
  # Calculate ax
  ax <- calc_ax(mx, sage, sex)
  
  # Calculate age specific probability of death 'qx'
  if (qx_method == "constant") {
    qx <- 1 - exp(-nx * mx)
  } else if (qx_method == "empirical") {

    qx <- nx * mx / (1 + (nx - ax) * mx)
  } else if (qx_method == "uniform") {
    qx <- nx * mx / (1 + nx / 2 * mx)
  }
  
  qx[nn] <- 1
  qx[is.na(qx)] <- 0
  
  # Calculate lx and dx
  if (nn > 1) {
    lx <- cohort * c(1, cumprod(1 - qx[1:(nn - 1)]))
    dx <- -diff(c(lx, 0))
  } else {
    lx <- dx <- cohort
  }
  
  # Calculate Lx
  Lx <- rep(NA, nn)
  for (i in 1:(nn - 1)) {
    Lx[i] <- nx[i] * lx[i + 1] + ax[i] * dx[i]
  }
  Lx[nn] <- lx[nn] / mx[nn]
  
  # Calculate Tx and ex
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  
  num_trunc <- ifelse(cohort == 1, 5, 2)
  
  # Create result data frame
  result <- data.frame(
    age = sage,
    mx = round(mx, 5),
    qx = round(qx, 5),
    lx = round(lx, num_trunc),
    dx = round(dx, num_trunc),
    Lx = round(Lx, num_trunc),
    Tx = round(Tx, num_trunc),
    ex = round(ex, 2)
  )
  
  # Add optional columns
  if (!is.null(pop)) result$pop <- pop
  if (!is.null(death)) result$death <- death
  if (!is.null(cancer_death)) result$cancer_death <- cancer_death
  
  return(result)
}

#' Expand a five-year abridged life table to a complete life table
#'
#' `expand_lx()` transforms a five-year abridged life table into a
#' one-year complete life table using the Elandt–Johnson method.
#'
#' @param lx A numeric vector representing the number of survivors (\eqn{l_x})
#' at the beginning of each age interval in the abridged life table. The
#' vector should correspond to age intervals provided in `sage`.
#' @param sage Numeric vector of starting ages for the age groups,
#' default is `c(0, 1, seq(5, 85, 5))`.
#' @param max_age Numeric value specifying the max age of the output estimated
#' lx(fitlx) and mx (fitmx).
#'
#' @return A list containing:
#' \describe{
#' \item{fitlx}{A numeric vector representing the estimated
#' number of survivors at each single year of age from 0 to 100.}
#' \item{fitmx}{A numeric vector representing the estimated
#' central death rates (\eqn{m_x}) for each single year of
#' age from 0 to 100.}
#' }
#'
#' @export
#'
#' @references
#' Baili, P., Micheli, A., Montanari, A., & Capocaccia, R. (2005).
#' Comparison of Four Methods for Estimating Complete Life Tables
#' from Abridged Life Tables Using Mortality Data Supplied to EUROCARE-3.
#' *Mathematical Population Studies*, 12(4), 183–198.
#' https://doi.org/10.1080/08898480500301751
#'
#' @examples
#' # Example abridged life table data (normalized to a radix of 1)
#' lx <- c(
#' 100000, 99498.39, 99294.62, 99173.88, 99047.59, 98840.46,
#' 98521.16, 98161.25, 97636.99, 96900.13, 95718.96, 93930.91,
#' 91463.21, 87131.41, 80525.02, 70907.59, 58090.75, 41630.48,
#' 24019.33
#' )
#' lx <- lx / 100000
#' expand_lx(lx)
expand_lx <- function(
    lx,
    sage = c(0, 1, seq(5, 85, 5)),
    max_age = 100
) {
  n <- length(sage)
  if (any(diff(sage) <= 0)) {
    stop("sage must be strictly increasing.")
  }
  if (sage[1] != 0 || sage[2] != 1 || diff(sage)[2] != 4 || any(diff(sage)[3:(n-1)] != 5)) {
    stop("sage must be of the form c(0, 1, seq(5, last, 5)) with 5-year intervals after age 1.")
  }
  if (!is.numeric(lx)) {
    stop("lx must be a numeric vector.")
  }
  if (length(lx) != n) {
    stop("lx and sage must have the same length.")
  }
  if (lx[1] < 0.2) {
    warning("First value is small (< 0.2), assuming input is mx instead of lx.")
    mx <- lx
    lx_comp <- numeric(n)
    lx_comp[1] <- 1
    for (i in 1:(n - 1)) {
      nn <- sage[i + 1] - sage[i]
      if (i == 1) {
        a <- 0.07 + 1.7 * mx[i]
      } else if (i == 2) {
        a <- 1.5
      } else {
        a <- nn / 2
      }
      q <- nn * mx[i] / (1 + (nn - a) * mx[i])
      if (q > 1) q <- 1
      lx_comp[i + 1] <- lx_comp[i] * (1 - q)
    }
    lx <- lx_comp
  }
  lx <- lx / lx[1]
  if (any(lx < 0)) {
    stop("lx cannot be negative.")
  }
  if (any(diff(lx) > 0)) {
    stop("lx must be non-increasing.")
  }
  ages <- sage
  coef1 <- matrix(c(
    1, 0, 0, 0, 0, 0,
    .56203, .7176, -.4784, .283886, -.100716, .0156,
    .273392, 1.047199, -.531911, .2992, -.103747, .015867,
    .096491, 1.1088, -.328533, .1728, -.058358, .0088,
    0, 1, 0, 0, 0, 0,
    -.041667, .798, .354667, -.152, .048, -.007,
    -.048872, .5616, .6656, -.240686, .072758, -.0104,
    -.037281, .3332, .888533, -.2448, .070147, -.0098,
    -.018379, .1408, 1.001244, -.160914, .043116, -.005867,
    0, 0, 1, 0, 0, 0
  ), ncol = 6, byrow = TRUE)
  coef2 <- matrix(c(
    .008064, -.07392, .88704, .22176, -.04928, .006336,
    .011648, -.09984, .69888, .46592, -.08736, .010752,
    .010752, -.08736, .46592, .69888, -.09984, .011648,
    .006336, -.04928, .22176, .88704, -.07392, .008064,
    0, 0, 0, 1, 0, 0
  ), ncol = 6, byrow = TRUE)
  clx <- numeric(max_age + 1)
  clx[1:10] <- coef1 %*% lx[2:7]
  for (m in 2:(n - 5)) {
    x1 <- which(ages == 5 * m - 10)
    x2 <- which(ages == 5 * m - 5)
    x3 <- which(ages == 5 * m)
    x4 <- which(ages == 5 * m + 5)
    x5 <- which(ages == 5 * m + 10)
    x6 <- which(ages == 5 * m + 15)
    clx[5 * m + 1:5] <- coef2 %*% lx[c(x1, x2, x3, x4, x5, x6)]
  }
  y <- log10(lx[c(1, 3:n)] / c(lx[3:n], NA))
  c_val <- (y / c(y[2:(n - 1)], NA)) ^ (-1 / 5)
  logb <- y / (c_val ^ ages[c(1, 3:n)] * (c_val ^ 5 - 1))
  b <- 10 ^ logb
  y <- c(y[1], NA, y[2:(n - 1)])
  c_val <- c(c_val[1], NA, c_val[2:(n - 1)])
  b <- c(b[1], NA, b[2:(n - 1)])
  parametric_start <- 5 * (n - 4)
  open_start <- parametric_start + 5
  for (i in seq(parametric_start, parametric_start, 5)) {
    x <- which(ages == i)
    S <- rep(NA, 5)
    for (k in 1:5) {
      S[k] <- b[x] ^ (1 - c_val[x] ^ (i + k - 1))
    }
    for (k in 1:5) {
      clx[i + k - 1] <- lx[x] * S[k] / S[1]
      if (i + k == open_start) {
        clx[i + k] <- lx[x + 1]
      }
    }
  }
  SS <- S[1]
  extension_number <- max_age - open_start + 1
  S <- rep(NA, extension_number + 1)
  for (i in 1:(extension_number + 1)) {
    S[i] <- b[x] ^ (1 - c_val[x] ^ (i + open_start - 1))
  }
  for (i in 1:extension_number) {
    clx[open_start + i] <- lx[x] * S[i + 1] / SS
  }
  for (i in 1:max_age) {
    if (is.na(clx[i]) || is.na(clx[i + 1])) next
    if (clx[i] < clx[i + 1]) {
      clx[i] <- clx[i + 1]
    }
  }
  clx <- c(1, clx[1:max_age])
  fitmx <- -log(c(clx[2:(max_age + 1)], NA) / clx)
  fitmx[max_age + 1] <- -log(clx[max_age + 1] / clx[max_age])
  list(fitlx = clx, fitmx = fitmx)
}

#' Calculate ax for life table
#'
#' `calc_ax()` calculates the average number of person-years lived in the interval
#' by those dying in the interval (ax) based on age-specific mortality rates (mx),
#' starting ages (sage), and sex.
#'
#' @param mx Numeric vector of age-specific mortality rates.
#' @param sage Numeric vector of starting ages for the age groups.
#' @param sex Character string specifying the sex: "male", "female", or "total"
#' (default is "male").
#'
#' @return A numeric vector of ax values.
#'
#' @details
#' For most age groups, \eqn{a_x = n_x / 2}, where \eqn{n_x} is the width
#' of the age interval.
#' For the infant group (age 0), ax is adjusted based on sex and m0:
#' - Male: if m0 >= 0.1, ax=0.33; else 0.045 + 2.684 * m0
#' - Female: if m0 >= 0.1, ax=0.35; else 0.053 + 2.8 * m0
#' - Total: if m0 >= 0.1, ax=0.34; else 0.049 + 2.742 * m0
#'
#' @references 
#' - Coale, A. J., Demeny, P., & Vaughan, B. (1983). Regional Model Life Tables
#'   and Stable Populations (2nd ed.). New York: Academic Press.
#' - Preston, S. H., Heuveline, P., & Guillot, M. (2001). Demography: Measuring
#'   and Modeling Population Processes. Malden, MA: Blackwell Publishers.
#'
#' @export
#'
#' @examples
#' mx <- c(0.05, 0.01, 0.005) 
#' sage <- c(0, 1, 5)
#' calc_ax(mx, sage, "male")
calc_ax <- function(mx, sage, sex = "male") {
  nn <- length(mx)
  if (length(sage) != nn) {
    stop("`sage` must have the same length as `mx`.")
  }
  sage <- as.numeric(sage)
  if (any(diff(sage) <= 0)) {
    stop("`sage` must be strictly increasing.")
  }
  nx <- c(diff(sage), Inf)
  ax <- nx / 2
  if (sage[1] == 0) {
    m0 <- mx[1]
    if (sex == "male") {
      ax[1] <- ifelse(m0 >= 0.1, 0.33, 0.045 + 2.684 * m0)
    } else if (sex == "female") {
      ax[1] <- ifelse(m0 >= 0.1, 0.35, 0.053 + 2.8 * m0)
    } else {
      ax[1] <- ifelse(m0 >= 0.1, 0.34, 0.049 + 2.742 * m0)
    }
  }
  ax
}
