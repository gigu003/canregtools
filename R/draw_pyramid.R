#' Plot population pyramid
#'
#' @param data A data.frame including left pyramid numbers in the 1st column
#'  and and right pyramid numbers in the 2nd column, where the numbers
#'  of males in each age-class are usually given to left numbers and
#'  those of females are to right numbers. If the data.frame includes 3rd
#'  column, it is used as age-class labels, otherwise the row.names(data)
#'  is used as age-class labels.
#' @param show_value Logical value to draw the population numbers. Default is
#'  TRUE.
#' @param show_prop Logical value to draw the pyramid using proportion.
#'  Default is TRUE.
#' @param left_axis A vector of axis for left pyramid. If missing, automatically
#'  given using pretty().
#' @param right_axis A vector of axis for right pyramid.
#'        If missing, left_axis is used.
#' @param axis_fm A format code of formatC for plotting axis.
#'  If missing, "g" is used.
#' @param axis_bm A big.mark of formatC for plotting axis.
#'  If missing, none.
#' @param axis_bi A big.interval number of formatC for plotting axis.
#'  Default is 3.
#' @param cgap The width of center gap (as ratio to each panel) to draw
#'  age-class. Default is 0.3.
#' @param cstep The interval to write the labels of age classes.
#'  Default is 1.
#' @param csize The font size factor to write the labels of age classes.
#'  Default is 1.
#' @param labs The label of the left, center, and right pyramid.
#'        Default is c("Males", "Ages", "Females").
#' @param gl Logical value to draw the vertical dotted lines. Default is TRUE.
#' @param cadj The vertical adjustment factor for the labels of age classes.
#'  Default is -0.03.
#' @param cols The color of the left and right pyramid.
#'        Default is c("lightblue", "pink").
#' @param dens The density of hatching lines (/inch) for left and right pyramid.
#'        Default is c(-1, -1), when the pyramid will be filled.
#' @param main The main title of the pyramid.
#' @param ... Other options.
#'
#' @importFrom graphics lines rect segments text
#' @return A population pyramid plot.
#' @export
#' @examples
#' left <- c(
#'   5053, 17743, 25541, 32509, 30530, 34806, 36846, 38691, 40056,
#'   39252, 37349, 30507, 26363, 21684, 15362, 11725, 7461, 3260, 915
#' )
#' right <- c(
#'   4728, 15330, 22633, 27784, 28082, 32605, 32964, 35732, 36234,
#'   37123, 34242, 29152, 24667, 18940, 15406, 12355, 10206, 5634,
#'   2547
#' )
#' agegrp <- c(
#'   "0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
#'   "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
#'   "70-74", "75-79", "80-84", "85+"
#' )
#' pop <- data.frame(left = left, right = right, row.names = agegrp)
#' pyramid(pop, main = "Population pyramid in China.", csize = 0.8)
pyramid <- function(data, show_value = TRUE, show_prop = TRUE,
                    left_axis = NULL, right_axis = NULL, axis_fm = "g",
                    axis_bm = "", axis_bi = 3, cgap = 0.3, cstep = 1, csize = 1,
                    labs = c("Males", "Ages", "Females"), gl = TRUE,
                    cadj = 0, cols = c("#006400", "#b32134"), dens = c(-1, -1),
                    main = "", ...) {
 # opar <- par(no.readonly = TRUE)
par(mar = c(0, 0, 2, 0))

  data <- as.data.frame(data)
  if (show_prop == TRUE) {
    ll <- data[, 1]
    rr <- data[, 2]
    Left <- ll / (sum(ll) + sum(rr)) * 100
    Right <- rr / (sum(ll) + sum(rr)) * 100
  } else {
    Left <- data[, 1]
    ll <- Left
    Right <- data[, 2]
    rr <- Right
  }

  if (length(data) == 2) {
    Center <- row.names(data)
  } else {
    Center <- data[, 3]
  }
  if (is.null(left_axis)) {
    left_axis <- pretty(c(0, max(c(Left, Right))))
  }
  if (is.null(right_axis)) {
    right_axis <- left_axis
  }
  side <- ifelse(show_value == TRUE, 1.2, 1)
  BX <- c(-side - cgap / 2, side + cgap / 2)
  BY <- c(-0.2, 1.1)
  plot(BX, BY, type = "n", axes = FALSE, xlab = "", ylab = "", main = main)
  LL <- max(left_axis)
  LR <- min(left_axis)
  LS <- LL - LR
  LI <- length(left_axis)
  RL <- min(right_axis)
  RR <- max(right_axis)
  RS <- RR - RL
  RI <- length(right_axis)
  
  #draw axis ticks for left axis
  segments(
    -(left_axis - LR) / LS - cgap / 2,
    -0.1,
    -(left_axis - LR) / LS - cgap / 2,
    -0.08, lwd = csize)
  
  #draw axis ticks for right axis
  segments(
    (right_axis - RL) / RS + cgap / 2,
    -0.1,
    (right_axis - RL) / RS + cgap / 2,
    -0.08, lwd = csize)

  #draw background grids
  if (gl) {
    segments(-(left_axis - LR) / LS - cgap / 2, 0, -(left_axis - LR) / LS -
      cgap / 2, 1, lty = 3, col = "blue")
    segments((right_axis - RL) / RS + cgap / 2, 0, (right_axis - RL) / RS +
      cgap / 2, 1, lty = 3, col = "blue")
  }
  #draw left axis line
  lines(c(-1 - cgap / 2, -cgap / 2), c(-0.09, -0.09), lty = 1, lwd = 0.9)
  #draw right axis line
  lines(c(1 + cgap / 2, cgap / 2), c(-0.09, -0.09), lty = 1, lwd = 0.9)
  # draw gap left line
  lines(c(-cgap / 2, -cgap / 2), c(0, 1.3), lty = 1, lwd = 0.5)
  # draw gap left line
  lines(c(cgap / 2, cgap / 2), c(0, 1.3), lty = 1, lwd = 0.5)
  
  sign <- ifelse(show_prop == TRUE, "(%)", "")
  text(-0.5 - cgap / 2, -0.18, paste(labs[1], sign), pos = 3,
       cex = csize*1.2, font = 2)
  text(0.5 + cgap / 2, -0.18, paste(labs[3], sign), pos = 3,
       cex = csize*1.2, font = 2)
  text(0, 1.05, labs[2], pos = 3, cex = csize*1.2, font = 2)
  Ci <- length(Center)
  # draw center age labels
  for (i in 0:(Ci - 1)) {
    if ((i %% cstep) == 0) {
      text(0, i / Ci + cadj, paste(Center[i + 1]), pos = 3, cex = csize)
    }
  }
  # draw the population number on two sides of pyramid
  if (show_value) {
    for (i in 0:(Ci - 1)) {
      if ((i %% cstep) == 0) {
        text(-1.15 - cgap / 2, i / Ci + cadj, paste(ll[i + 1]),
          pos = 3, cex = csize)
        text(1.15 + cgap / 2, i / Ci + cadj, paste(rr[i + 1]),
          pos = 3, cex = csize)
      }
    }
    #draw the total population number.
    text(-cgap-1, 1.06, paste("POP:",round(sum(ll,rr)/10000, 2)),
         pos = 4, cex = csize, font = 2)
    #draw the population proportion on left.
    text(-0.5 - cgap / 2, 1.05, paste(round(sum(Left), 2), sign),
      pos = 3, cex = csize)
    #draw the population proportion on right.
    text(0.5 + cgap / 2, 1.05, paste(round(sum(Right), 2), sign),
      pos = 3, cex = csize)
  }
  # draw label for left axis
  text(-(left_axis - LR) / LS - cgap / 2, rep(-0.022, LI),
    paste(formatC(left_axis,
      format = axis_fm,
      big.mark = axis_bm, big.interval = axis_bi
    )), pos = 1, cex = csize)
  # draw label for right axis
  text((right_axis - RL) / RS + cgap / 2, rep(-0.022, RI),
    paste(formatC(right_axis,
                  format = axis_fm,
                  big.mark = axis_bm, big.interval = axis_bi)),
    pos = 1, cex = csize)
  VB <- 0:(Ci - 1) / Ci
  VT <- 1:Ci / Ci
  LeftP <- -(Left - LR) / LS - cgap / 2
  #draw left bar
  rect(LeftP, VB, rep(-cgap / 2, Ci), VT, col = cols[1], density = dens[1],
       lwd = 0.5)
  RightP <- (Right - RL) / RS + cgap / 2
  #draw right bar
  rect(rep(cgap / 2, Ci), VB, RightP, VT, col = cols[2], density = dens[2],
       lwd = 0.5)
  #par(opar)
}

pyramids <- function(left, right, center = NULL, ...) {
  if (is.null(center)) {
    dx <- data.frame(left, right, row.names = names(Left))
  } else {
    dx <- data.frame(left, right, center)
  }
  pyramid(dx, ...)
}


#' Draw population pyramids.
#'
#' @param x Class of 'canreg' or 'fbswicd'.
#' @param show_value Logical value to draw the population numbers. Default is
#'  TRUE.
#' @param show_prop Logical value to draw the pyramid using proportion.
#'  Default is TRUE.
#' @param left_axis A vector of axis for left pyramid. If missing, automatically
#'  given using pretty().
#' @param right_axis A vector of axis for right pyramid.
#'        If missing, left_axis is used.
#' @param axis_fm A format code of formatC for plotting axis.
#'  If missing, "g" is used.
#' @param axis_bm A big.mark of formatC for plotting axis.
#'  If missing, none.
#' @param axis_bi A big.interval number of formatC for plotting axis.
#'  Default is 3.
#' @param cgap The width of center gap (as ratio to each panel) to draw
#'  age-class. Default is 0.3.
#' @param cstep The interval to write the labels of age classes.
#'  Default is 1.
#' @param csize The font size factor to write the labels of age classes.
#'  Default is 1.
#' @param labs The label of the left, center, and right pyramid.
#'        Default is c("Males", "Ages", "Females").
#' @param gl Logical value to draw the vertical dotted lines. Default is TRUE.
#' @param cadj The vertical adjustment factor for the labels of age classes.
#'  Default is -0.03.
#' @param cols The color of the left and right pyramid.
#'        Default is c("lightblue", "pink").
#' @param dens The density of hatching lines (/inch) for left and right pyramid.
#'        Default is c(-1, -1), when the pyramid will be filled.
#' @param grid Grids.
#' @param main The main title of the pyramid.
#'
#' @return Pyramids plot.
#' @export
#'
draw_pyramid <- 
  function(x,
           show_value = TRUE, show_prop = TRUE,
           left_axis = NULL, right_axis = NULL, axis_fm = "g",
           axis_bm = "", axis_bi = 3, cgap = 0.3, cstep = 1, csize = 1,
           labs = c("Males", "Ages", "Females"), gl = TRUE,
           cadj = 0, cols = c("#006400", "#b32134"), dens = c(-1, -1),
           main = "",grid=c(2,3)) {
  UseMethod("draw_pyramid", x)
}

#' Draw pyramids from object of fbswicd.
#'
#' @param x Object of class 'fbswicd'.
#' @param ... Parameters.
#' @param grid grids.
#'
#' @return Pyramid plots.
#' @export
#'
draw_pyramid.fbswicd <- function(x, ..., grid = NULL){
  pop <- x$pop
  pop <- split(pop, pop$year)
  if (is.null(grid)){
    grid <- c(1, length(pop))
  }
  to_wide <- function(x){
    res <- x %>%
      pivot_wider(id_cols = c("agegrp"),
                  values_from = c("rks"),
                  names_from = c("sex"),
                  values_fill = 0) %>%
      select(where(is.numeric), agegrp)
    return(res)
  }
  pop <- lapply(pop, to_wide)
  par(mfrow = grid)
  res <- lapply(pop, pyramid, ...)
}

