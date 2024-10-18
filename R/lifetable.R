#' Calculate life table from mortality rates.
#'
#' @param mx Death rate at age x.
#' @param sage Start age.
#' @param age_width Age groups width.
#' @param sex Gender for the life table, it could be "male","female" or "total".
#' @param sep_zero Specify whether the zero age group was separate.
#'
#' @return A data frame containing the following values.
#' \item{age}{Start age in each age group.}
#' \item{mx}{Death rate at age x.}
#' \item{qx}{The probability that an individual of exact age x will die before exact age x+1.}
#'  \item{lx}{Number of survivors to exact age x. The radix is 1.}
#'  \item{dx}{The number of deaths between exact age x and x+1.}
#'  \item{Lx}{Number of years lived between age x and x+1.}
#'  \item{Tx}{Number of years lived after exact age x.}
#'  \item{ex}{Remaining life expectancy at exact age x.}
#' @export
#'
#' @examples
#' px <- c(20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
#'         163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909)
#' dx <- c(156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
#'         891, 831, 926, 731, 269)
#' mx <- dx / px
#' lt(mx, sage = 0, age_width = 5, sex = "male")
#' mx <- c(0.01685, 0.00085, 0.00044, 0.00045, 0.00064, 0.00086, 0.00103,
#'         0.00136, 0.00195, 0.00291, 0.00429, 0.00672, 0.00985, 0.01596,
#'         0.02605, 0.04536, 0.07247, 0.12078, 0.17957, 0.25938, 0.25989)
#' lt(mx, sage = 0, age_width = 5, sex = "total")
#' 
lt <- function(mx, sage = 0, sep_zero = TRUE ,age_width = 5, sex = "male"){
  nn <- length(mx)
  ax <- calc_ax(mx, sage, age_width, sex)
  # calculate width of each age group
  switch(as.character(age_width),
         "1" = {
           nx <- c(rep(1, nn - 1), Inf)
           age <- c(sage,seq(1, nn - 1, 1))
         },
         "5" = {
           if (sage == 0){
             nx <- c(1, 4, rep(5, nn - 3), Inf)
             age <- c(sage, 1, seq(5, (nn - 2) * 5, 5))
           } else{
             nx <- c(ceiling(sage/5)*5-sage, rep(5, nn-2), Inf)
             age <- c(sage, seq(ceiling(sage/5)*5, (nn - 1) * 5, 5))
           }
         }
  )
  
  qx <- nx * mx / (1 + (nx - ax) * mx)
  
  qx[nn] <- 1
  if (nn > 1) {
    lx <- c(1, cumprod(1 - qx[1:(nn - 1)]))
    dx <- -diff(c(lx, 0))
  } else {
    lx <- dx <- 1
  }
  Lx <- nx * lx - dx * (nx - ax)
  Lx[nn] <- lx[nn] / mx[nn]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  if (nn > 2) {
    rx <- c(Lx[1] / lx[1], Lx[2:(nn - 1)] / Lx[1:(nn - 2)], Tx[nn] / Tx[nn - 1])
  } else if (nn == 2) {
    rx <- c(Lx[1] / lx[1], Tx[nn] / Tx[nn - 1])
  } else {
    rx <- c(Lx[1] / lx[1])
  }
  if (age_width == 5) {
    rx <- c(
      0, (Lx[1] + Lx[2]) / 5 * lx[1], Lx[3] / (Lx[1] + Lx[2]),
      Lx[4:(nn - 1)] / Lx[3:(nn - 2)], Tx[nn] / Tx[nn - 1]
    )
  }
  result <- data.frame(
    age = age, mx = mx, qx = qx, lx = lx,
    dx = dx, Lx = Lx, Tx = Tx, ex = ex )
  return(result)
}

calc_ax <- function(mx, sage, age_width, sex){
  nn <- length(mx)
  # calculate a0
  if (sage == 0){
    switch(sex,
           female = {a0 <- ifelse(mx[1] < 0.107, 0.053 + 2.8 * mx[1], 0.35)},
           male = {a0 <- ifelse(mx[1] < 0.107, 0.045 + 2.684 * mx[1], 0.33)},
           total = {a0 <- ifelse(mx[1] < 0.107, 0.049 + 2.742 * mx[1], 0.34)},
           default = {stop(paste("unsupported sex value:", sex))})
  } else if (sage > 0){
    a0 <- 0.5
  } else {
    stop("sage must be non-negative")
  }
  
  # calculate a1
  if (age_width == 5 && sage == 0){
    switch(sex,
           female = {a1 <-ifelse(mx[1] < 0.107, 1.522 - 1.518 * mx[1], 1.361)},
           male = {a1 <-ifelse(mx[1] < 0.107, 1.651 - 2.816 * mx[1], 1.352)},
           total = {a1 <- ifelse(mx[1] < 0.107, 1.5865 - 2.167 * mx[1], 1.3565)},
           default = {stop(paste("unsupported sex value:", sex))})
  }

  # calculate ax
  if(sage == 0){
    switch(as.character(age_width),
           "1" = {ax <- c(a0, rep(0.5, nn - 2), Inf)},
           "5" = {ax <- c(a0, a1, rep(2.5, nn - 3), Inf)})
  } else {
    switch(as.character(age_width),
           "1" = {ax <- c(rep(0.5, nn - 1), Inf)},
           "5" = {ax <- c(rep(2.5, nn - 1), Inf)})
  }
  return(ax)
}



#' Expand five-year(abridged) life table to one-year(complete) life table.
#'
#' @param lx A vector contains the total number of survivors lx at age x
#'        which was obtained from the abridged life table.
#'
#' @return List contains two vectors fitlx and fitmx.
#' @export
#'
#' @examples
#' lx <- c(
#'   100000, 99498.39, 99294.62, 99173.88, 99047.59, 98840.46,
#'   98521.16, 98161.25, 97636.99, 96900.13, 95718.96, 93930.91,
#'   91463.21, 87131.41, 80525.02, 70907.59, 58090.75, 41630.48,
#'   24019.33
#' )
#' lx <- lx / 100000
#' expand_lifetable(lx)
expand_lifetable <- function(lx) {
  ages <- c(0, 1, seq(5, 85, 5))
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
  ), ncol = 6, byrow = T)

  coef2 <- matrix(c(
    .008064, -.07392, .88704, .22176, -.04928, .006336,
    .011648, -.09984, .69888, .46592, -.08736, .010752,
    .010752, -.08736, .46592, .69888, -.09984, .011648,
    .006336, -.04928, .22176, .88704, -.07392, .008064,
    0, 0, 0, 1, 0, 0
  ), ncol = 6, byrow = T)
  clx <- 0
  for (i in 1:10) {
    clx[i] <- coef1[i, 1] * lx[2] + coef1[i, 2] * lx[3] + coef1[i, 3] * lx[4] +
      coef1[i, 4] * lx[5] + coef1[i, 5] * lx[6] + coef1[i, 6] * lx[7]
  }
  for (m in 2:14) {
    x1 <- which(ages == 5 * m - 10)
    x2 <- which(ages == 5 * m - 5)
    x3 <- which(ages == 5 * m)
    x4 <- which(ages == 5 * m + 5)
    x5 <- which(ages == 5 * m + 10)
    x6 <- which(ages == 5 * m + 15)
    for (i in 1:5) {
      clx[5 * m + i] <- coef2[i, 1] * lx[x1] + coef2[i, 2] * lx[x2] + coef2[i, 3] * lx[x3] +
        coef2[i, 4] * lx[x4] + coef2[i, 5] * lx[x5] + coef2[i, 6] * lx[x6]
    }
  }

  y <- log10(lx[c(1, 3:19)] / c(lx[c(3:19)], NA))
  c <- (y / c(y[2:18], NA))^(-1 / 5)
  logb <- y / (c^ages[c(1, 3:19)] * ((c^5) - 1))
  b <- 10^logb
  y <- c(y[1], NA, y[2:18])
  c <- c(c[1], NA, c[2:18])
  b <- c(b[1], NA, b[2:18])

  for (i in seq(75, 75, 5)) {
    S <- NA
    for (k in 1:5) {
      x <- which(ages == i)
      S[k] <- b[x]^(1 - c[x]^(i + k - 1))
    }
    for (k in 1:5) {
      clx[i + k - 1] <- lx[x] * S[k] / S[1]
      if (i + k == 80) {
        clx[i + k] <- lx[x + 1]
      }
    }
  }

  SS <- S[1]

  for (i in seq(1, 21, 1)) {
    S[i] <- b[17]^(1 - c[17]^(i + 80 - 1))
  }
  for (i in seq(1, 21, 1)) {
    clx[i + 80] <- lx[17] * S[i + 1] / SS
  }

  for (i in 1:99) {
    if (clx[i] < clx[i + 1]) {
      clx[i] <- clx[i + 1]
    }
  }
  clx <- c(1, clx[1:99])
  fitmx <- -log(c(clx[2:100], NA) / clx)
  fitmx[100] <- -log(clx[100] / clx[99])
  return(list(fitlx = clx, fitmx = fitmx))
}
