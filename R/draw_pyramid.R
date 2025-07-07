#' Plot a population pyramid
#'
#' This function draws a population pyramid using either raw population numbers or proportions,
#' displaying age groups in the center and population counts (e.g., by sex) on each side.
#'
#' @param data A data.frame containing the variables for age group (`x`),
#'    population (`y`), and grouping variable (`group`, e.g., sex). It must
#'    contain at least three columns: age group (x-axis labels),
#'    population size, and grouping (e.g., "male" and "female").
#' @param x A variable indicating age groups (quoted or unquoted).
#' @param y A variable indicating population counts (quoted or unquoted).
#' @param group A grouping variable, typically representing sex (quoted or
#'    unquoted).
#' @param facet Optional unquoted variable to facet the data by (e.g., year,
#'    region). A separate pyramid will be drawn for each unique value.
#' @param facet_label Optional character vector of labels for each facet.
#'    If `NULL`, labels will be extracted from the unique values of `facet`.
#' @param grid Optional vector of two integers specifying the layout of the
#'    facet grid (number of rows, number of columns). If `NULL`, it defaults to
#'    a vertical stack (`c(n, 1)` for `n` facets).
#' @param show_value Logical. If TRUE, displays the actual population values
#'    beside the bars. Default is FALSE.
#' @param show_prop Logical. If TRUE, the bars represent proportions (%)
#'    rather than absolute values. Default is TRUE.
#' @param left_axis Numeric vector of tick marks for the left side
#'    (e.g., males). If NULL, it will be generated using `pretty()`.
#' @param right_axis Numeric vector of tick marks for the right side (e.g.,
#'    females). If NULL, it will use `left_axis`.
#' @param left_label Character vector to customize axis labels on the left side.
#'    If NULL, generated using `formatC()`.
#' @param right_label Character vector for axis labels on the right side.
#'    Same rules as `left_label`.
#' @param cgap Numeric. Width of the central gap (relative to axis length).
#'    Default is 0.3.
#' @param cstep Integer. Step interval between age group labels.
#'    Default is 1 (every label shown).
#' @param csize Numeric. Scaling factor for text and lines. Default is 1.
#' @param labs A character vector of three labels: left side (e.g., "Males"),
#'    center (e.g., "Ages"), and right side (e.g., "Females"). Default is
#'    `c("Males", "Ages", "Females")`.
#' @param gl Integer. Indicating the line type of the grids.
#' @param cadj Numeric. Vertical adjustment for center age labels. Default is 0.
#' @param cols A character vector of two colors for the left and right bars.
#'    Default is `c("#006400", "#b32134")`.
#' @param dens A numeric vector indicating shading densities (lines per inch)
#'    for bars. Use -1 to fill solid bars. Default is `c(-1, -1)`.
#' @param main A character string for the main plot title. Default is an
#'    empty string.
#' @param ... Additional graphical parameters passed to the base `plot()`
#'    function.
#' @export
#' @return A base R graphics pyramid plot. It does not return a value.
#'
#' @examples
#' data("canregs")
#' pop <- canregs[[1]]$POP
#' draw_pyramid(pop, agegrp, rks, sex)
#' 
draw_pyramid <- function(
    data,
    x,
    y,
    group,
    facet = NULL,
    facet_label = NULL,
    grid = NULL,
    show_value = FALSE, show_prop = TRUE,
    left_axis = NULL, right_axis = NULL,
    left_label = NULL, right_label = NULL,
    cgap = 0.3, cstep = 1, csize = 1,
    labs = c("Males", "Ages", "Females"), gl = 2,
    cadj = 0, cols = c("#006400", "#b32134"), dens = c(-1, -1),
    main = "", ...
    ) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  facet_var <- rlang::enquo(facet)
  
  # Split the data according to the facet var
  if (!rlang::quo_is_null(facet_var)) {
    data_splited <- data |>
      dplyr::group_by(!!facet_var, .drop = TRUE, .add = TRUE) |>
      dplyr::group_split()
    if (is.null(facet_label)) {
      facet_label <- unique(as.character(dplyr::pull(data, !!facet_var)))
    }
  } else {
    data_splited <- list(data)
    facet_label <- main
  }

  # Set the grid value if not provided
  if (is.null(grid)) {
    grid <- c(length(data_splited), 1)
  }
  par(mfrow = grid)
  
  purrr::map(
    seq_along(data_splited),
    function(x) {
      pyramid(
        data_splited[[x]],
        x = !!x_var,
        y = !!y_var,
        group = !!group_var,
        show_value = show_value,
        show_prop = show_prop,
        left_axis = left_axis,
        right_axis = right_axis,
        left_label = left_label,
        right_label = right_label,
        cgap = cgap,
        cstep = cstep,
        csize = csize,
        labs = labs,
        gl = gl,
        cadj = cadj,
        cols = cols,
        dens = dens,
        main = facet_label[x]
      )
    }
  )
  invisible(NULL)
}

#' Draw the pyramid plot
#'
#' @noRd
#'
pyramid <- function(
    data,
    x,
    y,
    group,
    show_value = FALSE, show_prop = TRUE,
    left_axis = NULL, right_axis = NULL,
    left_label = NULL, right_label = NULL,
    cgap = 0.3, cstep = 1, csize = 1,
    labs = c("Males", "Ages", "Females"), gl = 2,
    cadj = 0, cols = c("#006400", "#b32134"), dens = c(-1, -1),
    main = "", ...
    ) {
  par(mar = c(0, 0, 2, 0))
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  
  group_values <- dplyr::pull(data, !!group_var) |> unique()
  y_value <- purrr::map(group_values, function(x) {
    dplyr::filter(data, !!group_var == x) |> pull(!!y_var)
  })
  
  x_value <- dplyr::pull(data, !!x_var) |> unique()
  
  if (show_prop == TRUE) {
    ll <- y_value[[1]]
    rr <- y_value[[2]]
    left_value <- ll / (sum(ll) + sum(rr)) * 100
    right_value <- rr / (sum(ll) + sum(rr)) * 100
  } else {
    left_value <- y_value[[1]]
    ll <- left_value
    right_value <- y_value[[2]]
    rr <- right_value
  }

  
  if (is.null(left_axis)) {
    left_axis <- pretty(range(c(0, c(left_value, right_value))))
  }
  
  if (is.null(right_axis)) {
    right_axis <- left_axis
  }
  side <- ifelse(show_value == TRUE, 1.2, 1)

  plot(
    c(-side - cgap / 2, side + cgap / 2),
    c(-0.2, 1.1),
    type = "n", axes = FALSE, xlab = "", ylab = "", main = main)
  max_left <- max(left_axis)
  min_left <- min(left_axis)
  range_left <- max_left - min_left
  max_right <- max(right_axis)
  min_right <- min(right_axis)
  range_right <- max_right - min_right


  # draw axis ticks for left axis
  segments(
    -(left_axis - min_left) / range_left - cgap / 2,
    -0.1,
    -(left_axis - min_left) / range_left - cgap / 2,
    -0.08,
    lwd = 0.3 * csize
  )

  # draw axis ticks for right axis
  segments(
    (right_axis - min_right) / range_right + cgap / 2,
    -0.1,
    (right_axis - min_right) / range_right + cgap / 2,
    -0.08,
    lwd = 0.3 * csize
  )

  # draw background grids
  if (!is.null(gl)) {
    segments(-(left_axis - min_left) / range_left - cgap / 2, 0, -(left_axis - min_left) / range_left -
               cgap / 2, 1, lty = gl, col = "blue", lwd = 0.2 * csize)
    segments((right_axis - min_right) / range_right + cgap / 2, 0, (right_axis - min_right) / range_right +
               cgap / 2, 1, lty = gl, col = "blue", lwd = 0.2 * csize)
  }
  # draw left axis line
  lines(c(-1 - cgap / 2, -cgap / 2), c(-0.09, -0.09), lty = 1, lwd = 0.5*csize)
  # draw right axis line
  lines(c(1 + cgap / 2, cgap / 2), c(-0.09, -0.09), lty = 1, lwd = 0.5*csize)
  # draw gap left line
  lines(c(-cgap / 2, -cgap / 2), c(0, 1.3), lty = 1, lwd = 0.4*csize)
  # draw gap left line
  lines(c(cgap / 2, cgap / 2), c(0, 1.3), lty = 1, lwd = 0.4*csize)

  sign <- ifelse(show_prop == TRUE, "(%)", "")
  text(-0.5 - cgap / 2, -0.18, paste(labs[1], sign),
    pos = 3,
    cex = csize * 1.1, font = 2
  )
  text(0.5 + cgap / 2, -0.18, paste(labs[3], sign),
    pos = 3,
    cex = csize * 1.1, font = 2
  )
  text(0, 1.05, labs[2], pos = 3, cex = csize * 1.2, font = 2)
  length_groups <- length(x_value)
  # draw center age labels
  for (i in 0:(length_groups - 1)) {
    if ((i %% cstep) == 0) {
      text(0, i / length_groups + cadj, paste(x_value[i + 1]), pos = 3, cex = csize)
    }
  }
  # draw the population number on two sides of pyramid
  if (show_value) {
    for (i in 0:(length_groups - 1)) {
      if ((i %% cstep) == 0) {
        text(-1.15 - cgap / 2, i / length_groups + cadj, paste(ll[i + 1]),
          pos = 3, cex = csize
        )
        text(1.15 + cgap / 2, i / length_groups + cadj, paste(rr[i + 1]),
          pos = 3, cex = csize
        )
      }
    }
    # draw the total population number.
    text(-cgap - 1, 1.06, paste("POP:", round(sum(ll, rr) / 10000, 2)),
      pos = 4, cex = csize, font = 2
    )
    # draw the population proportion on left.
    text(-0.5 - cgap / 2, 1.05, paste(round(sum(left_value), 2), sign),
      pos = 3, cex = csize
    )
    # draw the population proportion on right.
    text(0.5 + cgap / 2, 1.05, paste(round(sum(right_value), 2), sign),
      pos = 3, cex = csize
    )
  }
  
  # Set the default left axis label
  if (is.null(left_label)) {
    left_label <- paste(
      formatC(left_axis, format = "g", big.mark = "g", big.interval = 3)
      )
  }
  # Set the default right axis label
  if (is.null(right_label)) {
    right_label <- paste(
      formatC(right_axis, format = "g", big.mark = "g", big.interval = 3)
    )
  }
  
  # draw label for left axis
  text(
    -(left_axis - min_left) / range_left - cgap / 2,
    rep(-0.022, length(left_axis)),
    left_label,
    pos = 1, cex = csize
  )
  # draw label for right axis
  text(
    (right_axis - min_right) / range_right + cgap / 2,
    rep(-0.022, length(right_axis)),
    right_label, pos = 1, cex = csize
  )
  bottom_y <- 0:(length_groups - 1) / length_groups
  top_y <- 1:length_groups / length_groups
  left_rect <- -(left_value - min_left) / range_left - cgap / 2
  # draw left bar
  rect(left_rect, bottom_y, rep(-cgap / 2, length_groups), top_y,
    col = cols[1], density = dens[1], border = "white",
    lwd = 0.2 * csize
  )
  right_rect <- (right_value - min_right) / range_right + cgap / 2
  # draw right bar
  rect(rep(cgap / 2, length_groups), bottom_y, right_rect, top_y,
    col = cols[2], density = dens[2], border = "white",
    lwd = 0.2 * csize
  )
}
