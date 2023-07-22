#' Plot population pyramid
#'
#' @param data A data.frame including left pyramid numbers in the 1st column
#'  and and right pyramid numbers in the 2nd column, where the numbers
#'  of males in each age-class are usually given to left numbers and
#'  those of females are to right numbers. If the data.frame includes 3rd column,
#'  it is used as age-class labels, otherwise the row.names(data) is used as
#'  age-class labels.
#' @param Show.Values Logical value to draw the population numbers. Default is
#'  TRUE.
#' @param Show.Proportion Logical value to draw the pyramid using proportion.
#'  Default is TRUE.
#' @param Laxis A vector of axis for left pyramid. If missing, automatically
#'  given using pretty().
#' @param Raxis A vector of axis for right pyramid. If missing, Laxis is used.
#' @param AxisFM A format code of formatC for plotting axis.
#'  If missing, "g" is used.
#' @param AxisBM A big.mark of formatC for plotting axis.
#'  If missing, none.
#' @param AxisBI A big.interval number of formatC for plotting axis.
#'  Default is 3.
#' @param Cgap The width of center gap (as ratio to each panel) to draw
#'  age-class. Default is 0.3.
#' @param Cstep The interval to write the labels of age classes.
#'  Default is 1.
#' @param Csize The font size factor to write the labels of age classes.
#'  Default is 1.
#' @param Llab The label of the left pyramid. Default is "Males".
#' @param Rlab The label of the right pyramid. Default is "Females".
#' @param Clab The label of the center age-class. Default is "Ages".
#' @param GL Logical value to draw the vertical dotted lines. Default is TRUE.
#' @param Cadj The vertical adjustment factor for the labels of age classes.
#'  Default is -0.03.
#' @param Lcol The color of the left pyramid. Default is "Cyan".
#' @param Rcol The color of the right pyramid. Default is "Pink".
#' @param Ldens The density of hatching lines (/inch) for left pyramid.
#'  Default is -1, when the pyramid will be filled.
#' @param Rdens The density of hatching lines (/inch) for right pyramid.
#'  Default is -1, when the pyramid will be filled.
#' @param main The main title of the pyramid.
#' @param ... Other options.
#'
#' @importFrom graphics lines rect segments text
#' @return A population pyramid plot.
#' @export
#'
#' @examples
#' library(canregtools)
#' library(readxl)
pyramid <- function (data, Show.Values = TRUE, Show.Proportion = TRUE,
                     Laxis = NULL, Raxis = NULL, AxisFM = "g",
                     AxisBM = "", AxisBI = 3, Cgap = 0.3, Cstep = 1, Csize = 1,
                     Llab = "Males", Rlab = "Females", Clab = "Ages", GL = TRUE,
                     Cadj = 0, Lcol = "lightblue", Rcol = "pink", Ldens = -1,
                     Rdens = -1, main = "",  ...) {
  data <- as.data.frame(data)
  if (Show.Proportion == TRUE){
    ll <- data[, 1]
    rr <- data[, 2]
    Left <- ll/(sum(ll)+sum(rr))*100
    Right <- rr/(sum(ll)+sum(rr))*100
  } else {
    Left <- data[, 1]
    ll <- Left
    Right <- data[, 2]
    rr <- Right}
  
  if (length(data) == 2) {
    Center <- row.names(data)
  } else {
    Center <- data[, 3]
  }
  if (is.null(Laxis)) {
    Laxis <- pretty(c(0, max(c(Left, Right))))
  }
  if (is.null(Raxis)) {
    Raxis <- Laxis
  }
  side <- ifelse(Show.Values == TRUE, 1.2, 1)
  BX <- c(-side - Cgap/2, side + Cgap/2)
  BY <- c(-0.1, 1.1)
  plot(BX, BY, type = "n", axes = FALSE, xlab = "", ylab = "", main = main)
  LL <- max(Laxis)
  LR <- min(Laxis)
  LS <- LL - LR
  LI <- length(Laxis)
  RL <- min(Raxis)
  RR <- max(Raxis)
  RS <- RR - RL
  RI <- length(Raxis)
  segments(-(Laxis - LR)/LS - Cgap/2, -0.01, -(Laxis - LR)/LS - Cgap/2, 0.01)
  segments((Raxis - RL)/RS + Cgap/2, -0.01, (Raxis - RL)/RS + Cgap/2, 0.01)

  if (GL) {
    segments(-(Laxis - LR)/LS - Cgap/2, 0, -(Laxis - LR)/LS - 
               Cgap/2, 1, lty = 3, col = "blue")
    segments((Raxis - RL)/RS + Cgap/2, 0, (Raxis - RL)/RS + 
               Cgap/2, 1, lty = 3, col = "blue")
  }
  lines(c(-1 - Cgap/2, -Cgap/2), c(0, 0), lty = 1)
  lines(c(-Cgap/2, -Cgap/2), c(0, 1.3), lty = 1)
  lines(c(1 + Cgap/2, Cgap/2), c(0, 0), lty = 1)
  lines(c(Cgap/2, Cgap/2), c(0, 1.3), lty = 1)
  sign <- ifelse(Show.Proportion == TRUE, "(%)", "")
  text(-0.5 - Cgap/2, -0.14, paste(Llab, sign), pos = 3)
  text(0.5 + Cgap/2, -0.14, paste(Rlab, sign), pos = 3)
  text(0, 1.05, Clab, pos = 3)
  Ci <- length(Center)
  #draw center age labels
  for (i in 0:(Ci - 1)) {
    if ((i%%Cstep) == 0) {
      text(0, i/Ci + Cadj, paste(Center[i + 1]), pos = 3, 
           cex = Csize) }}
  #draw the population number on two sides of pyramid
  if (Show.Values){
    for(i in 0:(Ci - 1)){
      text(-1.15 - Cgap/2, i/Ci + Cadj, paste(ll[i + 1]), pos = 3, cex = Csize)
      text(1.15 + Cgap/2, i/Ci + Cadj, paste(rr[i + 1]), pos = 3, cex = Csize)
      text(-0.5 - Cgap/2, 1.05, paste(round(sum(Left), 2)), pos =3, cex = Csize)
      text(0.5 + Cgap/2, 1.05, paste(round(sum(Right), 2)), pos =3, cex = Csize)
    }
  }
  text(-(Laxis - LR)/LS - Cgap/2, rep(0, LI),
       paste(formatC(Laxis, format = AxisFM,
                     big.mark = AxisBM, big.interval = AxisBI)), pos = 1)
  text((Raxis - RL)/RS + Cgap/2, rep(0, RI),
       paste(formatC(Raxis,format = AxisFM,
                     big.mark = AxisBM, big.interval = AxisBI)), pos = 1)
  VB <- 0:(Ci - 1)/Ci
  VT <- 1:Ci/Ci
  LeftP <- -(Left - LR)/LS - Cgap/2
  rect(LeftP, VB, rep(-Cgap/2, Ci), VT, col = Lcol, density = Ldens)
  RightP <- (Right - RL)/RS + Cgap/2
  rect(rep(Cgap/2, Ci), VB, RightP, VT, col = Rcol, density = Rdens)    
}