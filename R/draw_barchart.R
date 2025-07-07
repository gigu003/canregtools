#' Draw Grouped Bar Charts with Facets
#'
#' This function creates grouped bar charts with optional faceting. It is useful
#' for comparing category-specific values across different groups and panels.
#'
#' @param data A `data.frame` or tibble containing the input data.
#' @param x The categorical variable (unquoted) for the x-axis.
#' @param y The numerical variable (unquoted) for the y-axis (bar height).
#' @param group Optional grouping variable (unquoted) used for color/fill aesthetics.
#' @param facet Optional faceting variable (unquoted) for splitting the data into panels.
#' @param facet_label Optional character vector of labels corresponding to each facet.
#' @param y_label Optional y-axis label or vector of labels for facets.
#' @param rev_group Reverse the bar position of group variables.
#' @param grid A numeric vector of length 2 indicating the layout (rows, columns) of the plot grid.
#' @param topn Number of top `x` categories to display in each group/panel.
#' @param axis Optional vector of breaks for the y-axis. If `NULL`, computed automatically.
#' @param bar_side Integer indicating the side of bars: 1 = left, 2 = right.
#' @param bar_way Integer for bar layout: 1 = one-sided, 2 = mirrored.
#' @param gap Horizontal spacing between bars and y-axis (default depends on `x_label_side`).
#' @param csize Character size scaling factor (default = 0.8).
#' @param space Vertical space between bars (default = 0.9).
#' @param adj Vertical adjustment of x-label text (default = -0.01).
#' @param gl Integer. Indicating the line type of the grids.
#' @param cols Optional vector of colors for bars.
#' @param palette Character. Name of palette colors.
#' @param x_label_side Side to place x-labels: 1 = inside, 2 = outside (default = 1).
#' @param legend Logical. Whether to show a legend (default = FALSE).
#' @param legend_label Optional character vector for legend labels.
#' @param legend_pos Optional, numeric vector of length 2 for legend position.
#' @param dens A numeric vector of two density values for bar shading (default = c(-1, -1)).
#' @param overlay Logical. Whether to overlay bars from two groups in the same panel.
#'
#' @return A base R plot with grouped bar charts, optionally faceted.
#' @export
#'
#' @examples
#' data("canregs")
#' asr <- create_asr(canregs[[1]], year, sex, cancer, event = "fbs")
#' asr <- cr_filter(asr, drop = c("total", "others"))
#' draw_barchart(asr, x = cancer, y = cr, group =  year, facet = sex)
draw_barchart <- function(
    data,
    x,
    y,
    group = NULL,
    facet = NULL,
    facet_label = NULL,
    y_label = NULL,
    rev_group = FALSE,
    grid = NULL,
    topn = NULL,
    axis = NULL,
    bar_side = NULL,
    bar_way = NULL,
    gap = NULL,
    csize = 0.8, space = 0.9, adj = -0.01, gl = NULL,
    cols = NULL,
    palette = "Peach",
    x_label_side = 1,
    legend = FALSE,
    legend_label = NULL,
    legend_pos = c(0.4, 0.2),
    dens = c(-1, -1), overlay = FALSE
    ) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  facet_var <- rlang::enquo(facet)
  
  if (!quo_is_null(facet_var)) {
    data_splited <- data |>
      arrange(!!facet_var) |>
      group_split(!!facet_var)
    if (is.null(facet_label)) {
      facet_value <- dplyr::pull(data, !!facet_var) |> unique()
      facet_label <- facet_value
    }
  } else {
    data_splited <- list(data)
    facet_label <- names(data_splited)
  }
  
  facet_length <- length(data_splited)
  if (facet_length == 2 & is.null(bar_side)) x_label_side <- 2
  
  if (is.null(gap)) gap <- ifelse(x_label_side == 1, 0.6, 0.1)
  max_y <- max(dplyr::pull(data, {{ y }}))
  axis <- pretty(c(0, max_y))
  way <- ifelse(facet_length == 2 & is.null(bar_side), 2, 1)
  
  if (length(facet_label) == 2) {
    if (is.null(bar_side)) {
      sides <- c(1, 2)
      over_lay <- c(FALSE, TRUE)
    } else {
      sides <- rep(bar_side, facet_length)
      over_lay <- c(FALSE, FALSE)
    }
    } else {
    sides <- rep(bar_side, facet_length)
    over_lay <- rep(FALSE, facet_length)
  }
  
  plotbar <- function(i) {
    # arrange data by group_var and plot_var.
    facet_data <- data_splited[[i]]
    if (is.null(topn)) topn <- nrow(facet_data)
    draw_bar(
      facet_data,
      x = !!x_var,
      y = !!y_var,
      group = !!group_var,
      rev_group = rev_group,
      topn = topn,
      axis = axis,
      bar_side = sides[i],
      x_label_side = x_label_side,
      bar_way = way,
      overlay = over_lay[i],
      gap = gap,
      csize = csize, space = space, adj = adj, gl = gl,
      cols = cols,
      palette = palette,
      legend = ifelse(i == 1, TRUE, FALSE),
      legend_label = legend_label,
      legend_pos = legend_pos,
      dens = dens,
      y_label = facet_label[i]
    )
  }
  
  if (is.null(grid) & !quo_is_null(facet_var) & !facet_length == 2) {
    grid <- c(1, facet_length)
  }
  par(mfrow = grid)
  
  for (i in 1:facet_length) {
    plotbar(i)
  }
  
  par(mfrow = c(1, 1))
}

#' Draw bar chart
#' @noRd
draw_bar <- function(
    data,
    x,
    y,
    group = NULL,
    rev_group = FALSE,
    axis = NULL,
    topn = 10,
    y_label = NULL,
    bar_side = NULL,
    x_label_side = 1,
    bar_way = 1,
    gap = ifelse(x_label_side == 1, 0.6, 0.1),
    csize = 0.8, space = 0.9, adj = -0.004, gl = NULL,
    cols = NULL,
    palette = "Peach",
    legend = FALSE,
    legend_label = NULL,
    legend_pos = c(0.4, 0.15),
    dens = c(-1, -1), overlay = FALSE, ...
    ) {
  par(mar = c(0, 0, 1, 0))
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  if (is.null(y_label)) y_label <- rlang::as_name(y_var)
  group_var <- rlang::enquo(group)
  if (quo_is_null(group_var)) {
    data <- dplyr::arrange(data, !!y_var) |>
      slice_tail(n = topn)
    if (is.null(bar_side)) bar_side <- 1
  } else {
    if (is.null(bar_side)) bar_side <- 1
    group_value <- unique(dplyr::pull(data, !!group_var))
    x_order <- dplyr::filter(data, !!group_var == group_value[1]) |>
      arrange(!!y_var) |>
      slice_tail(n = topn) |>
      pull(!!x_var)
    data <- dplyr::mutate(data, !!x_var := factor(!!x_var, levels = x_order)) |>
      filter(!!x_var %in% x_order) |>
      arrange(!!group_var, !!x_var)
  }
  
  if (quo_is_null(group_var)) {
    y_value <- list(
      dplyr::pull(data, !!y_var)
      )
    if (quo_is_null(x_var)) {
      x_value <- seq(1, length(y_value[[1]]))
    } else {
      x_value <- dplyr::pull(data, !!x_var)
    }
  } else {
    y_value <- lapply(
      group_split(data, !!group_var), function(x) {
        dplyr::pull(x, !!y_var)
      })
    if (rev_group) y_value <- rev(y_value)
    if (quo_is_null(x_var)) {
      x_value <- seq(1, length(y_value[[1]]))
    } else {
      x_value <- lapply(
        group_split(data, !!group_var), function(x) {
          dplyr::pull(x, !!x_var)
        })[[1]]
    }
  }
  
  if (is.null(axis)) {
    axis <- pretty(c(0, 1.005 * max(unlist(y_value))))
  }

  if (!overlay == TRUE) {
    side <- ifelse(x_label_side == 1, 1.1, 1.3)
    if (bar_way == 2) {
      bx <- c(-side - gap / 2, side + gap / 2)
    } else {
      if (bar_side == 1) {
        bx <- c(-side - gap / 2, 0)
      } else {
        bx <- c(0, side + gap / 2)
      }
    }
    by <- c(0, 1.10)
    plot(bx, by, type = "n", axes = FALSE, xlab = "", ylab = "", ...)
  }

  if (is.null(cols)) {
    cols <- hcl.colors(length(y_value), palette)
  } else if (length(y_value) > length(cols)) {
    cols <- rep(cols, length.out = length(y_value))
  } else {
    cols <- cols[1:length(y_value)]
  }
  
  max_axis <- max(axis)
  min_axis <- min(axis)
  range_axis <- max_axis - min_axis
  # draw axis ticks
  ticks_loc <- (axis - min_axis) / range_axis + gap / 2
  axis_loc <- c(1 + gap / 2, gap / 2)
  axis_label_loc <- 0.5 + gap / 2
  if (bar_side == 1) {
    ticks_loc <- -ticks_loc
    axis_loc <- -axis_loc
    axis_label_loc <- -axis_label_loc
  }

  # draw axis ticks
  segments(ticks_loc, 1.02, ticks_loc, 1.03)

  # draw axis line
  lines(axis_loc, c(1.02, 1.02), lty = 1)
  # draw axis tick label
  text(ticks_loc, rep(1.02, length(ticks_loc)),
    paste(formatC(axis, format = "g")),
    pos = 3, cex = csize * 0.9
  )
  # draw axis label
  text(axis_label_loc, 1.08,
    #paste(ifelse(bar_side == 1, y_label[1], y_label[2])),
    y_label,
    pos = 3, cex = csize, font = 2
  )

  # draw grid lines
  if (!is.null(gl)) {
    segments(ticks_loc, 0, ticks_loc, 1, lty = gl, col = "gray")
  }

  Ci <- length(y_value[[1]]) # calculate groups number
  VB <- 0:(Ci - 1) / Ci # calculate vertical start position for each group
  VT <- 1:Ci / Ci # calculate vertical end position fro each group
  # draw the side label
  if (x_label_side == 1) {
    text(ifelse(bar_side == 1, -gap / 2, gap / 2), (VB + VT) / 2 + adj,
      as.character(x_value),
      pos = ifelse(bar_side == 1, 4, 2), cex = csize * 0.9
    )
  } else {
    text(axis_loc[1], (VB + VT) / 2 + adj,
      as.character(x_value),
      pos = ifelse(bar_side == 1, 2, 4), cex = csize * 0.9
    )
  }

  VT <- (VT - (VT - VB) * (1 - space))
  h <- (VT - VB) / length(y_value)
  
  # draw the left bar
  left_ <- lapply(y_value, function(x) -(x - min_axis) / range_axis - gap / 2)
  bottom_ <- lapply(1:length(y_value), function(i) VB + (i - 1) * h)
  top_ <- lapply(1:length(y_value), function(i) VB + i * h)

  if (bar_side == 1) {
    for (i in 1:length(y_value)) {
      rect(left_[[i]], bottom_[[i]], rep(-gap / 2, Ci), top_[[i]],
        col = rev(cols)[i], border = rev(cols)[i], density = dens[1]
      )
    }
  } else {
    for (i in 1:length(y_value)) {
      rect(-rep(-gap / 2, Ci), bottom_[[i]], -left_[[i]], top_[[i]],
        col = rev(cols)[i], border = rev(cols)[i], density = dens[1]
      )
    }
  }
  if (legend & length(y_value) > 1) {
    if (is.null(legend_label) && !rlang::quo_is_null(group_var)) {
      legend_label <- rev(levels(factor(dplyr::pull(data, !!group_var))))
    }
    if (rev_group) legend_label <- rev(legend_label)
    legend(
      ifelse(bar_side == 1,
             -legend_pos[1] - gap / 2,
             legend_pos[1] + gap / 2
             ), legend_pos[2],
      legend = legend_label, border = "transparent",
      bty = "n", fill = cols, cex = csize * 0.8
    )
  }
}
