% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_linechart.R
\name{draw_line}
\alias{draw_line}
\alias{draw_linechart}
\title{Draw line chart}
\usage{
draw_line(
  data,
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
  lwd = 2,
  main = NULL,
  add = FALSE,
  offset = 0.01,
  ...
)

draw_linechart(x, facet_var, grid, ...)
}
\arguments{
\item{data}{The input data frame.}

\item{x_var}{The variable for the x-axis.}

\item{y_var}{The variable for the y-axis.}

\item{group_var}{The variable used to group the data for multiple lines.}

\item{x_axis}{Optional, the custom X-axis tick values. If not provided, the
function generates them.}

\item{y_axis}{Optional, the custom Y-axis tick values. If not provided, the
function generates them.}

\item{x_label}{Optional, label for X-axis ticks.}

\item{y_label}{Optional, label for Y-axis ticks.}

\item{axis_label}{Labels for the axis title, it should be character vector
length 2.}

\item{cols}{Optional. The colors for the lines. If not provided, the function
uses default colors.}

\item{line_type}{1-character string giving the type of plot desired. The
following values are possible, for details, see plot: "p" for points,
"l" for lines, "b" for both points and lines, "c" for empty points
joined by lines, "o" for overplotted points and lines, "s" and "S"
for stair steps and "h" for histogram-like vertical lines. Finally,
"n" does not produce any points or lines.}

\item{lwd}{Line width.}

\item{main}{Optional. The title of the plot.}

\item{add}{Plots to be added.}

\item{offset}{Offset of the axis.}

\item{...}{Parameters}

\item{x}{The input data frame.}

\item{facet_var}{Facet var}

\item{grid}{Grid}
}
\value{
Line chart.
}
\description{
Draw line chart
}
