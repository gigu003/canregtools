#' Draw line chart
#'
#' @rdname draw_linechart
#' @param data The input data frame.
#' @param x_var The variable for the x-axis.
#' @param y_var The variable for the y-axis.
#' @param group_var The variable used to group the data for multiple lines.
#' @param x_axis Optional, the custom X-axis tick values. If not provided, the
#'        function generates them.
#' @param y_axis Optional, the custom Y-axis tick values. If not provided, the
#'        function generates them.
#' @param x_label Optional, label for X-axis ticks.
#' @param y_label Optional, label for Y-axis ticks.
#' @param axis_label Labels for the axis title, it should be character vector
#'        length 2.
#' @param cols Optional. The colors for the lines. If not provided, the function
#'        uses default colors.
#' @param line_type 1-character string giving the type of plot desired. The
#'        following values are possible, for details, see plot: "p" for points,
#'        "l" for lines, "b" for both points and lines, "c" for empty points
#'        joined by lines, "o" for overplotted points and lines, "s" and "S"
#'        for stair steps and "h" for histogram-like vertical lines. Finally,
#'        "n" does not produce any points or lines.
#' @param main Optional. The title of the plot.
#' @param add Plots to be added.
#' @param ... Other parameters in plot.

#'
#' @return Line chart.
#' @export
#' 
draw_line <- function(data,
                      x_var,
                      y_var,
                      group_var,
                      x_axis = NULL,
                      y_axis = NULL,
                      x_label = NULL,
                      y_label = NULL,
                      axis_label = c("Age (years)", "Age specific rate"),
                      cols = c("darkgreen", "darkred", "gray"),
                      line_type = "l",
                      main = NULL,
                      add = FALSE,
                       ...) {
  if (is.null(cols)) cols <- palette()
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
  # Set up the plotting area
  opar <- par(no.readonly = TRUE)
  par(mar = c(1, 2, 2, 1))
  # Set up axis values
  if (is.null(y_axis)) {
    y_axis <- pretty(c(0, max(data[[y_var]], na.rm = TRUE)))
    }
  if (is.null(x_axis)) {
    x_axis <- pretty(c(0, max(as.numeric(data[[x_var]], na.rm = TRUE))))
    }
  # Calculate axis scaling factors
  offset <- 0.01
  BX <- c(-0.1, 1)
  BY <- c(-0.1, 1)

  plot(BX, BY, type = "n", axes = FALSE, xlab = NULL, ylab = NULL,
       main = main, ...)
  
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
  # draw axis ticks label
  text(-offset, (y_axis - YL) / YS,
       paste(formatC(y_axis, format = "fg")),
       pos = 2, srt = 90)
  text((x_axis - XL) / XS, -offset,
       paste(formatC(x_axis, format = "fg")),
       pos = 1)
  # draw the axis title labels.
  text((1 - offset) / 2, -(offset + 0.06), axis_label[1], pos = 1, font = 2)
  text(-(offset + 0.09), (1 - offset) / 2, axis_label[2], pos = 3,
       srt = 90, font = 2)
  
  groups <- unique(data[[group_var]])
  
  # Check if the group_var is factor or character vector.
  if (!is.factor(data[[group_var]]) && !is.character(data[[group_var]])) {
    stop("group_var must be factor or character.")
  }
  
  if (is.null(group_var)) {
    lines(as.numeric(group_data[[x_var]]) / XS,
          group_data[[y_var]] / YS,
          type = line_type,
          col = cols[1], lwd = 2)
  } else {
   # plot line for each group.
    for (i in 1:length(groups)) {
      group <- groups[i]
      group_data <- data[data[[group_var]] == group, ]
      if (nrow(group_data) > 1) {
        lines(as.numeric(group_data[[x_var]]) / XS, group_data[[y_var]] / YS,
              type = line_type, col = cols[i], lwd = 2
        )
      }
    }
  }
  
  legend(0.01, 0.95, legend = groups, col = cols,
         lwd = 2, seg.len = 0.5, bty = "n")
  
  if (add){
    par(opar) 
  }
}


#' @rdname draw_linechart
#' @param x The input data frame.
#' @param facet_var Facet var
#' @param grid Grid
#' @param ... Parameters
#'
#' @export
#'
draw_linechart <- function(x, facet_var, grid, ...) {
  # Split the data frame into a list of data frames by the facet_var
  res <- x %>%
    group_by({{facet_var}}) %>%
    group_split()
  
  # Set up the plotting area with the specified grid layout
  par(mfrow = grid)
  
  sapply(res, function(data) {
    facet <- data |> 
      pull({{facet_var}}) |> 
      unique()
    draw_line(data, main = facet, ...)
    })
  
  # Reset graphical parameters to default
  par(mfrow = c(1, 1))
}
