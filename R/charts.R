#' Plot dual bar chart.
#'
#' @param data A data frame.
#' @param topn Top n values to show.
#' @param gap The width of center gap. Default is 0.1.
#' @param space Space between bars.
#' @param adj The vertical adjustment factor for the labels of age classes.
#'  Default is -0.01.
#' @param csize The font size to write the labels on sides. Default is 1.
#' @param gl Logical value to draw the vertical dotted lines. Default is TRUE.
#' @param label Labels for the two sides.
#' @param legend Legends for the bars.
#' @param cols The colors of the bars.
#' @param dens The density of hatching lines (/inch).
#' @param main The main title for the dulbar.
#' @param ... Other options.
#'
#' @importFrom grDevices palette
#' @importFrom graphics legend par points
#' @return A dulbar chart.
#' @export
#' 
#' @examples
#' library(canregtools)
#' file <- system.file("extdata", "411721.xls", package = "canregtools")
#' data <- read_canreg(file)
#' rate <- asr_rate(data, icd_cat, event = sws)
#' dulbar_chart(rate[,c(1, 3, 6, 9, 10)])
dulbar_chart <- function(data,
                         topn = 10,
                         gap = 0.1,
                         space = 0.9,
                         adj = -0.01,
                         csize = 1,
                         gl = TRUE,
                         label = c("Male", "Female"),
                         legend = c("Crude rate", "ASR rate"),
                         cols = c("#006400", "#b32134"),
                         dens = c(-1, -1),
                         main = "", ...) {
  data <- as.data.frame(data)
  cate <- as.character(data[, 1])
  left <- data[, 2]
  ll <- tail(cate[order(left)], topn)
  right <- data[, 3]
  rr <- tail(cate[order(right)], topn)

  if (length(data) > 3) {
    left2 <- tail(data[, 4][order(left)], topn)
    right2 <- tail(data[, 5][order(right)], topn)
  }

  left <- tail(left[order(left)], topn)
  right <- tail(right[order(right)], topn)

  laxis <- pretty(c(0, max(c(left, right))))
  raxis <- laxis

  side <- 1.3
  bx <- c(-side - gap / 2, side + gap / 2)
  by <- c(0, 1.10)
  plot(bx, by, type = "n", axes = FALSE, xlab = "", ylab = "", main = main, ...)
  LL <- max(laxis)
  LR <- min(laxis)
  LS <- LL - LR
  LI <- length(laxis)
  RL <- min(raxis)
  RR <- max(raxis)
  RS <- RR - RL
  RI <- length(raxis)
  segments(-(laxis - LR) / LS - gap / 2, 1.02, -(laxis - LR) / LS - gap / 2, 1.03)
  segments((raxis - RL) / RS + gap / 2, 1.02, (raxis - RL) / RS + gap / 2, 1.03)

  if (gl) {
    segments(-(laxis - LR) / LS - gap / 2, 0, -(laxis - LR) / LS -
      gap / 2, 1, lty = 3, col = "blue")
    segments((raxis - RL) / RS + gap / 2, 0, (raxis - RL) / RS +
      gap / 2, 1, lty = 3, col = "blue")
  }

  lines(c(-1 - gap / 2, -gap / 2), c(1.02, 1.02), lty = 1)
  lines(c(1 + gap / 2, gap / 2), c(1.02, 1.02), lty = 1)

  text(-0.5 - gap / 2, 1.08, paste(label[1]), pos = 3, cex = csize)
  text(0.5 + gap / 2, 1.08, paste(label[2]), pos = 3, cex = csize)
  text(-(laxis - LR) / LS - gap / 2, rep(1.02, LI),
    paste(formatC(laxis, format = "g")),
    pos = 3
  )
  text((raxis - RL) / RS + gap / 2, rep(1.02, RI),
    paste(formatC(raxis, format = "g")),
    pos = 3
  )

  Ci <- length(left)

  VB <- 0:(Ci - 1) / Ci
  VT <- 1:Ci / Ci
  # draw side labels
  text(-1.0 - gap / 2, (VB + VT) / 2 + adj, ll, pos = 2, cex = csize)
  text(1.0 + gap / 2, (VB + VT) / 2 + adj, rr, pos = 4, cex = csize)
  VT <- (VT - (VT - VB) * (1 - space))
  if (length(data) > 3) {
    h <- (VT - VB) / 2
  } else {
    h <- 0
  }

  # draw the rect bars
  leftP <- -(left - LR) / LS - gap / 2
  rect(leftP,
    VB + h,
    rep(-gap / 2, Ci),
    VT,
    col = cols[1], border = cols[1], density = dens[1]
  )
  rightP <- (right - RL) / RS + gap / 2
  rect(rep(gap / 2, Ci),
    VB + h,
    rightP,
    VT,
    col = ifelse(length(data) > 3, cols[1], cols[2]),
    border = ifelse(length(data) > 3, cols[1], cols[2]), density = dens[2]
  )

  if (length(data) > 3) {
    leftP2 <- -(left2 - LR) / LS - gap / 2
    rightP2 <- (right2 - RL) / RS + gap / 2
    rect(leftP2, VB, rep(-gap / 2, Ci), VB + h,
      col = cols[2], border = cols[2],
      density = dens[1]
    )
    rect(rep(gap / 2, Ci), VB, rightP2, VB + h,
      col = cols[2], border = cols[2],
      density = dens[2]
    )
    legend(0.5, 0.2, legend = legend, bty = "n", fill = cols)
  }
}

#' Plot line chart
#'
#' @param data The input data frame.
#' @param x_var The variable for the x-axis.
#' @param y_var The variable for the y-axis.
#' @param group_var The variable used to group the data for multiple lines.
#' @param y_axis Optional. The custom Y-axis tick values. If not provided, the
#'        function generates them.
#' @param x_axis Optional. The custom X-axis tick values. If not provided, the
#'        function generates them.
#' @param col Optional. The colors for the lines. If not provided, the function
#'        uses default colors.
#' @param main Optional. The title of the plot.
#' @param ... Other parameters.
#'
#' @return Line plot.
#' @export
#' @examples
#' library(canregtools)
#' file <- system.file("extdata", "411721.xls", package = "canregtools")
#' data <- read_canreg(file)
#' fbsw <- count_canreg(data)
#' age <- create_age_rate(fbsw, sex)
#' line_chart(age, agegrp, rate, sex)
line_chart <- function(data,
                       x_var,
                       y_var,
                       group_var,
                       y_axis = NULL,
                       x_axis = NULL,
                       col = NULL,
                       main = NULL,
                       ...) {
  # Check if the input data is a data frame.
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  # Convert variable names to strings.
  x_var <- as.character(substitute(x_var))
  y_var <- as.character(substitute(y_var))
  group_var <- as.character(substitute(group_var))


  # Check if x_var, y_var, and group_var are present in the data frame.
  if (!(x_var %in% names(data)) || !(y_var %in% names(data)) ||
    !(group_var %in% names(data))) {
    stop("The specified variables do not exist in the data frame.")
  }

  opar <- par(no.readonly = TRUE)
  par(mar = c(0, 0, 2, 0))
  if (is.null(y_axis)) {
    y_axis <- pretty(c(0, max(data[[y_var]])))
  }
  if (is.null(x_axis)) {
    x_axis <- pretty(c(0, max(as.numeric(data[[x_var]]))))
  }

  offset <- 0.02
  BX <- c(-0.1, 1)
  BY <- c(-0.1, 1)
  plot(BX, BY, type = "n", axes = FALSE, xlab = "", ylab = "", main = main, ...)
  YU <- max(y_axis)
  YL <- min(y_axis)
  YS <- YU - YL
  XU <- max(x_axis)
  XL <- min(x_axis)
  XS <- XU - XL
  segments(-offset, (y_axis - YL) / YS, -(offset + 0.01), (y_axis - YL) / YS)
  segments((x_axis - XL) / XS, -offset, (x_axis - XL) / XS, -(offset + 0.01))
  lines(c(-offset, -offset), c(-offset, 1), lty = 1)
  lines(c(-offset, 1), c(-offset, -offset), lty = 1)
  text(-offset, (y_axis - YL) / YS, paste(formatC(y_axis, format = "g")), pos = 2)
  text((x_axis - XL) / XS, -offset, paste(formatC(x_axis, format = "g")), pos = 1)
  groups <- unique(data[[group_var]])

  # Check if the group_var is factor or character vector.
  if (!is.factor(data[[group_var]]) && !is.character(data[[group_var]])) {
    stop("group_var must be factor or character.")
  }


  if (is.null(group_var)) {
    lines(as.numeric(group_data[[x_var]]) / XS,
      group_data[[y_var]] / YS,
      type = "l",
      col = palette()[1], lwd = 3)
  } else {
    # plot line for each group.
    for (i in 1:length(groups)) {
      group <- groups[i]
      group_data <- data[data[[group_var]] == group, ]
      if (nrow(group_data) > 1) {
        lines(as.numeric(group_data[[x_var]]) / XS, group_data[[y_var]] / YS,
          type = "l", col = palette()[i], lwd = 3
        )
      }
    }
  }
  text((1 - offset) / 2, -(offset + 0.05), x_var, pos = 1, font = 2)
  text(-(offset + 0.07), (1 - offset) / 2, y_var, pos = 2, srt = 90)
  if (length(groups) > 1) {
    legend(0.01, 0.95, legend = groups, col = palette(), lwd = 3, bty = "n")
  }
  par(opar)
}

#' Plot dumbbell chart.
#'
#' @param data A data frame contains data to be plotted.
#' @param x A category variable in data.
#' @param y1 Variable indicate start point.
#' @param y2 Variable indicate end point.
#' @param topn Top n values to be plotted.
#' @param sort Sort options.
#' @param legend Legends.
#' @param cols Colors of the start and end points.
#' @param main Main title of the plot.
#'
#' @importFrom utils tail
#'
#' @return A dumbbell plot.
#' @export
#'
dumbbell_chart <- function(data,
                           x = NULL,
                           y1 = NULL,
                           y2 = NULL,
                           topn = 20,
                           sort = "insc",
                           legend = NULL,
                           cols = c("#006400", "#b32134"),
                           main = "") {
  x <- enquo(x)
  y1 <- enquo(y1)
  y2 <- enquo(y2)

  cate <- data %>% pull(!!x)
  left <- data %>% pull(!!y1)
  right <- data %>% pull(!!y2)

  order <- order(left)
  cate <- tail(cate[order], topn)
  left <- tail(left[order], topn)
  right <- tail(right[order], topn)

  if (sort == "desc") {
    order <- order(left, decreasing = TRUE)
    cate <- cate[order]
    left <- left[order]
    right <- right[order]
  }

  axis <- pretty(c(0, max(c(left, right))))
  BX <- c(-0.2, 1.0)
  BY <- c(-0.1, 1.0)
  plot(BX, BY, type = "n", axes = FALSE, xlab = "", ylab = "", main = main)
  max_axis <- max(axis)
  min_axis <- min(axis)
  r_axis <- max_axis - min_axis
  long <- length(axis)

  ll <- (left - min_axis) / r_axis
  rr <- (right - min_axis) / r_axis
  Ci <- length(left)

  segments((axis - min_axis) / r_axis, -0.02, (axis - min_axis) / r_axis, -0.03)
  lines(c(0, 1), c(-0.02, -0.02))
  text((axis - min_axis) / r_axis, rep(-0.03, long),
    paste(formatC(axis, format = "g")),
    pos = 1
  )
  gl <- TRUE
  if (gl) {
    segments((axis - min_axis) / r_axis, 0, (axis - min_axis) / r_axis, 1,
      lty = 3, col = "blue"
    )
  }

  text(-.03, 0:(Ci - 1) / Ci, cate, pos = 2, cex = 1)

  segments(ll, 0:(Ci - 1) / Ci, rr, 0:(Ci - 1) / Ci, lwd = 1.7, col = cols[1])
  points(ll, 0:(Ci - 1) / Ci, pch = 16, col = cols[1])
  points(rr, 0:(Ci - 1) / Ci, pch = 16, col = cols[2])
  if (!is.null(legend)) {
    legend(0.7, 0.15, legend = legend, bty = "n", fill = cols)
  }
}
