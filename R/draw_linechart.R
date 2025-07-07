#' Draw a Custom Line Chart Using Base R
#'
#' This function draws a line chart from a data frame, optionally grouped by a
#' categorical variable. It uses base R graphics and supports custom axis
#' ticks, labels, and styles.
#'
#' @param data A data frame containing the variables to plot.
#' @param x,y Bare column names for the x and y axis variables.
#' @param group Optional bare column name used to group and color lines.
#' @param facet Optional bare column name used for faceting. If provided,
#'   the data will be split by this variable and plotted in a multi-panel
#'   layout.
#' @param grid A vector of length 2 specifying number of rows and columns for
#'    facets. Default is c(1, 1).
#' @param x_axis Optional numeric vector specifying x-axis tick locations.
#' @param y_axis Optional numeric vector specifying y-axis tick locations.
#' @param x_label,y_label Optional labels for x and y axis ticks. If `NULL`,
#'   defaults are used.
#' @param axis_title Character vector of length 2 giving the axis titles:
#'    c("x axis label", "y axis label").
#' @param cols Character vector of line colors. Defaults to c("darkgreen",
#'    "darkred", "gray").
#' @param line_type 1-character string giving the type of plot desired. The
#'        following values are possible, for details, see plot: "p" for points,
#'        "l" for lines, "b" for both points and lines, "c" for empty points
#'        joined by lines, "o" for overplotted points and lines, "s" and "S"
#'        for stair steps and "h" for histogram-like vertical lines. Finally,
#'        "n" does not produce any points or lines.
#' @param palette Character, palette name indicate group of colors.
#' @param lwd Line width. Default is 2.
#' @param main,sub Main title and subtitle of the plot.
#' @param adj Adjustment for axis text placement. Default is 0.02.
#' @param srt String rotation angle for x-axis labels. Default is 90 degrees.
#' @param add Logical. If `TRUE`, restores original graphics parameters after
#'    plotting.
#' @param mar Margin of the sub plot.
#' @param offset Axis offset used for spacing ticks. Default is 0.01.
#' @param ... Additional arguments (currently unused).
#' @export
#' @examples
#' data("canregs")
#' fbsw <- count_canreg(canregs[[1]], label_tail="yrs")
#' agerate <- create_age_rate(fbsw, year, sex)
#' agerate <- add_labels(agerate, lang = "en")
#' draw_linechart(agerate, agegrp, rate, sex)
#' agerate <- create_age_rate(fbsw, year, sex, cancer)
#' agerate <- add_labels(agerate, lang = "en")
#' agerate <- dplyr::filter(agerate, cancer %in% as.character(c(103:106)))
#' draw_linechart(agerate, agegrp, rate, sex, cancer, grid = c(2, 2))
draw_linechart <- function(data,
                           x,
                           y,
                           group = NULL,
                           facet = NULL,
                           grid = c(1, 1),
                           x_axis = NULL,
                           y_axis = NULL,
                           x_label = NULL,
                           y_label = NULL,
                           axis_title = c("Age (years)", "Age specific rate"),
                           cols = NULL,
                           palette = "Peach",
                           line_type = "l",
                           lwd = 2,
                           adj = 0.02,
                           srt = 60,
                           main = NULL,
                           sub = NULL,
                           mar = c(1, 0, 1, 0),
                           add = TRUE,
                           offset = 0.01,
                           ...) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  facet_var <- rlang::enquo(facet)
  
  if (rlang::quo_is_null(facet_var)) {
    draw_line(data, !!x_var, !!y_var, !!group_var,
              x_axis = x_axis, y_axis = y_axis,
              x_label = x_label, y_label = y_label, axis_title = axis_title,
              cols = cols, palette = palette, line_type = line_type, lwd = lwd,
              main = main, sub = sub,
              add = add, offset = offset, adj = adj, srt = srt, mar = mar)
  } else {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfrow = grid, mar = c(2, 0, 2, 0))
    res <- dplyr::group_by(data, !!facet_var) |> dplyr::group_split()
    
    for (df in res) {
      facet_value <- unique(dplyr::pull(df, !!facet_var))
      draw_line(df,
                !!x_var, !!y_var, !!group_var,
                x_axis = x_axis, y_axis = y_axis,
                x_label = x_label, y_label = y_label, axis_title = axis_title,
                cols = cols, palette = palette, line_type = line_type,
                lwd = lwd, main = facet_value, sub = sub,
                add = add, offset = offset, adj = adj, srt = srt, mar = mar)
    }
  }
}

#' @noRd
#' 
draw_line <- function(data,
                      x,
                      y,
                      group = NULL,
                      x_axis = NULL,
                      y_axis = NULL,
                      x_label = NULL,
                      y_label = NULL,
                      axis_title = c("Age (years)", "Age specific rate"),
                      cols = NULL,
                      palette = "Peach",
                      line_type = "l",
                      lwd = 2,
                      main = NULL,
                      sub = NULL,
                      adj = 0.02,
                      srt = 60,
                      add = TRUE,
                      legend_pos = c(0.01, 0.95),
                      mar = c(2, 0, 2, 0),
                      offset = 0.01,
                      ...) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  oldpar <- par(no.readonly = TRUE)
  par(mar = mar)
  
  line_type <- match.arg(line_type,
                         choices = c("p", "l", "b", "c", "o", "s",
                                     "S", "h", "n"))
  
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  d_vars <- names(data)
  for (var in c(rlang::as_name(x_var), rlang::as_name(y_var))) {
    if (!var %in% d_vars) stop(sprintf("Variable `%s` not in data.", var))
  }
  
  if (!rlang::quo_is_null(group_var) && !(rlang::as_name(group_var) %in% d_vars)) {
    stop(sprintf("Grouping variable `%s` not in data.", rlang::as_name(group_var)))
  }
  
  
  xvalue <- dplyr::pull(data, !!x_var)
  yvalue <- dplyr::pull(data, !!y_var)
  # Type coercion
  if (is.character(xvalue)) xvalue <- factor(xvalue)
  if (is.factor(xvalue)) {
    if (is.null(x_axis)) {
      x_axis <- seq(0, length(levels(xvalue)) - 1)
    }
    if (is.null(x_label)) {
      x_label <- levels(xvalue)
    }
    xvalue <- as.integer(xvalue) - 1
  }
  
  if (!is.numeric(yvalue)) yvalue <- as.numeric(yvalue)
  
  if (is.null(y_axis)) {
    y_axis <- pretty(range(yvalue, na.rm = TRUE))
  } else {
    y_min <- min(yvalue, na.rm = TRUE)
    y_max <- max(yvalue, na.rm = TRUE)
    if (min(y_axis) > y_min || max(y_axis) < y_max) {
      warning("Specified y_axis does not fully cover the data range. Expanding it.")
      y_axis <- pretty(range(c(y_axis, yvalue), na.rm = TRUE))
    }
  }
  if (is.null(x_axis)) {
    x_axis <- pretty(range(xvalue, na.rm = TRUE))
  } else {
    x_min <- min(xvalue, na.rm = TRUE)
    x_max <- max(xvalue, na.rm = TRUE)
    if (min(x_axis) > x_min || max(x_axis) < x_max) {
      warning("Specified x_axis does not fully cover the data range. Expanding it.")
      x_axis <- pretty(range(c(x_axis, xvalue), na.rm = TRUE))
    }
  }
  if (is.null(x_label)) x_label <- x_axis
  if (is.null(y_label)) y_label <- y_axis

  max_y <- max(y_axis)
  min_y <- min(y_axis)
  range_y <- max_y - min_y
  max_x <- max(x_axis)
  min_x <- min(x_axis)
  range_x <- max_x - min_x
  plot(c(-0.12, 1), c(-0.17, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
  segments(-offset, (y_axis - min_y) / range_y, -(offset + 0.01), (y_axis - min_y) / range_y)
  segments((x_axis - min_x) / range_x, -offset, (x_axis - min_x) / range_x, -(offset + 0.01))
  lines(c(-offset, -offset), c(-offset, 1.02))
  lines(c(-offset, 1.02), c(-offset, -offset))
  
  text((x_axis - min_x) / range_x, -offset - adj, labels = x_label, srt = srt, pos = 1)
  text(-offset, (y_axis - min_y) / range_y, labels = y_label, pos = 2, srt = 90)
  
  text((1 - offset) / 2, -offset - 0.08 - adj, axis_title[1], pos = 1, font = 2)
  text(-offset - 0.09, (1 - offset) / 2, axis_title[2], pos = 3, srt = 90, font = 2)

  if (!rlang::quo_is_null(group_var)) {
    data <- dplyr::mutate(data, !!group_var := as.factor(!!group_var))
    groups <- levels(dplyr::pull(data, !!group_var))
    if (is.null(cols)) {
      cols <- hcl.colors(length(groups), palette)
    }
    if (length(cols) < length(groups)) {
      warning("Not enough colors provided. Colors will be recycled.")
      cols <- rep(cols, length.out = length(groups))
    }
    group_data <- dplyr::group_by(data, !!group_var) |> dplyr::group_split()
    for (i in seq_along(groups)) {
      df <- group_data[[i]]
      xv <- dplyr::pull(df, !!x_var)
      if (is.factor(xv)) xv <- as.integer(xv) - 1
      yv <- dplyr::pull(df, !!y_var)
      if (nrow(df) > 1) {
        lines((xv - min_x) / range_x, (yv - min_y) / range_y,
              type = line_type, col = cols[i], lwd = lwd)
      }
    }
    legend(legend_pos[1], legend_pos[2],
           legend = groups, col = cols, lwd = lwd, bty = "n", seg.len = 0.5)
  } else {
    lines((xvalue - min_x) / range_x, (yvalue - min_y) / range_y,
          type = line_type, col = cols[1], lwd = lwd)
  }
 
  title(main = main, font = 2)
  if (!is.null(sub)) {
    mtext(sub, side = 1, line = 1, adj = 1)
  }
  if (!add) graphics::par(oldpar)
}
