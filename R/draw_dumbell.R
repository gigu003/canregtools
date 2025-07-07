#' Plot dumbbell chart
#'
#' @param data A data frame contains data to be plotted.
#' @param x A category variable in data.
#' @param y1 Variable indicate start point.
#' @param y2 Variable indicate end point.
#' @param topn Top n values to be plotted.
#' @param sort Sort options.
#' @param legend Legends.
#' @param cols Colors of the start and end points.
#' @param gl Integer. Indicating the line type of the grids.
#' @param gl_col Color of the background grid.
#' @param main Main title of the plot.
#'
#' @importFrom utils tail
#'
#' @return A dumbbell plot.
#' @export
#'
#' @examples
#' asr <- create_asr(canregs[[1]], year, cancer, show_ci = TRUE) |>
#'   drop_others() |>
#'   drop_total() |>
#'   add_labels(lang = "en", label_type = "abbr")
#' draw_dumbbell(asr, site, asr_lower_cn2000, asr_upper_cn2000, topn = 15)
#'
draw_dumbbell <- function(data,
                          x = NULL,
                          y1 = NULL,
                          y2 = NULL,
                          topn = 20,
                          sort = "insc",
                          legend = NULL,
                          cols = c("#006400", "gray", "#b32134"),
                          gl = NULL,
                          gl_col = c("gray"),
                          main = "") {
  x <- enquo(x)
  y1 <- enquo(y1)
  y2 <- enquo(y2)
  
  cate <- pull(data, !!x)
  left <- pull(data, !!y1)
  right <- pull(data, !!y2)
  
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
  if (!is.null(gl)) {
    segments((axis - min_axis) / r_axis, 0, (axis - min_axis) / r_axis, 1,
             lty = gl, col = gl_col
    )
  }
  
  text(-.03, 0:(Ci - 1) / Ci, cate, pos = 2, cex = 1)
  
  segments(ll, 0:(Ci - 1) / Ci, rr, 0:(Ci - 1) / Ci, lwd = 1.7, col = cols[2])
  points(ll, 0:(Ci - 1) / Ci, pch = 16, col = cols[1])
  points(rr, 0:(Ci - 1) / Ci, pch = 16, col = cols[3])
  if (!is.null(legend)) {
    legend(0.7, 0.15, legend = legend, bty = "n", fill = cols)
  }
}
