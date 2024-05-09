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
#' fbsw <- count_canreg(data, lang = "en")
#' age <- create_age_rate(fbsw, sex)
#' draw_line(age, agegrp, rate, sex)
draw_line <- function(data,
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