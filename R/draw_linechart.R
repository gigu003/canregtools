#' Draw line chart
#'
#' @rdname draw_linechart
#'
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
#' @param offset Offset of the axis.
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
                      offset = 0.02,
                       ...) {
  
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var)
  group_var <- rlang::enquo(group_var)
  
  if (is.null(cols)) cols <- palette()
  # Check if the input data is a data frame.
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }
  # Names of the variables in the dataset
  d_vars <- names(data)
  # Check if x_var, y_var, and group_var are present in the data frame.
  if (!(as_name(x_var) %in% d_vars) || !(as_name(y_var) %in% d_vars)) {
    stop("The specified variables do not exist in the data frame.")
  }
  
  xvar <- data |> pull(!!x_var)
  if (is.factor(xvar))
  
  if (!quo_is_null(group_var)) {
    if (!(as_name(group_var) %in% d_vars)){
      stop(paste("The specified variable", as_name(group_var),
                 "not exist in the data frame"))  
    }
  }
  
  # Set up the plotting area
  opar <- par(no.readonly = TRUE)
  par(mar = c(1, 1, 1, 1))

  max_yvar <- data |> pull(!!y_var) |> max()
  max_xvar <- data |> pull(!!x_var) |> as.numeric(na.rm = TRUE) |>  max()
    
  # Set up axis values
  if (is.null(y_axis)) { y_axis <- pretty(c(0, max_yvar)) }
  if (is.null(x_axis)) { x_axis <- pretty(c(0, max_xvar)) }
  
  # Calculate axis scaling factors
  BX <- c(-0.1, 1)
  BY <- c(-0.1, 1)
  plot(BX, BY, type = "n", axes = FALSE, xlab = NULL, ylab = NULL, main = main)
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
  if (is.null(x_label)) { x_label <- x_axis }
  if (is.null(y_label)) { y_label <- y_axis }
  
  # Draw Y axis tick label
  ylabel <- paste(formatC(y_label, format = "fg"))
  text(-offset, (y_axis - YL) / YS, ylabel, pos = 2, srt = 90)
  # Draw X axis tick label
  xlabel <- paste(formatC(x_label, format = "fg"))
  text((x_axis - XL) / XS, -offset, xlabel, pos = 1)
  
  # draw the axis title labels.
  text((1 - offset) / 2, -(offset + 0.06), axis_label[1], pos = 1, font = 2)
  text(-(offset + 0.09), (1 - offset) / 2, axis_label[2], pos = 3, srt = 90,
       font = 2)
  
  # Check if the group_var is factor or character vector.
  if (!quo_is_null(group_var)) {
    gvar_data <- data |> pull(!!group_var) 
    if (!is.factor(gvar_data) && !is.character(gvar_data)) {
      stop("group_var must be factor or character.")
    }
  }
  
  if (quo_is_null(group_var)) {
    xvar_data <- data |> pull(!!x_var) |> as.numeric()
    yvar_data <- data |> pull(!!y_var) |> as.numeric()
    lines(xvar_data / XS, yvar_data / YS, type = line_type, 
          col = cols[1], lwd = 3)
    } else {
      groups <- data |> pull(!!group_var) |> unique()
      group_data <- data |> group_by(!!group_var) |> group_split()
      # plot line for each group.
      for (i in 1:length(groups)) {
        group <- groups[i]
        gdata <- group_data[[i]]
        xvar_data <- gdata |> pull(!!x_var) |> as.numeric()
        yvar_data <- gdata |> pull(!!y_var) |> as.numeric()
        #group_data <- data[data[[group_var]] == group, ]
        if (nrow(gdata) > 1) {
          lines(xvar_data / XS, yvar_data / YS,
                type = line_type, col = cols[i], lwd = 3) } }
    }
  if (!quo_is_null(group_var)){
    legend(0.01, 0.95, legend = groups, col = cols,
           lwd = 2, seg.len = 0.5, bty = "n")  
  }
  
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
  par(mar = c(1, 2, 2, 1))
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
