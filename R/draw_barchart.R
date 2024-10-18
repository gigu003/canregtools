#' Draw bar chart.
#'
#' @param values List of values to be draw.
#' @param cates Categories for the groups.
#' @param axis Axis values.
#' @param groups Groups of the bar chart, default is length of the values list.
#' @param bar_side Orientation of the bar chart: 1 for left and 2 for right.
#' @param bar_way Draw the bar chart unidirectionally or bidirectionally, 1 for
#'                unidirectionally and 2 for bidirectionally.
#' @param gap The width of center gap. Default is 0.1.
#' @param csize The font size to write the labels on sides. Default is 1.
#' @param space Space between bars.
#' @param adj The vertical adjustment factor for the labels of age classes.
#'            Default is -0.01.
#' @param gl Logical value to draw the vertical dotted lines. Default is TRUE.
#' @param label Labels for the two sides.
#' @param cols The colors of the bars.
#' @param dens The density of hatching lines (/inch).
#' @param overlay Logical value, TRUE for overlay plot, FALSE for not.
#' @param side_label Draw the side label on the left or right. 1 for left and
#'                    2 for right. 
#' @param legend Logical value indicates plot legend or not.
#' @param legend_label Character for the legend label. 
#' @param ... Other options.
#' 
#' @return A bar chart.
#' @export
#' 
draw_bar <- function(values, cates = NULL, axis = NULL, groups = length(values),
                     bar_side = 1, bar_way = 1, gap = ifelse(side_label==1, 0.6, 0.1),
                     csize = 0.8, space = 0.9, adj = -0.01,  gl = TRUE,
                     label = c("Male","Female"),
                     cols = c("#006400", "#b32134"),
                     side_label = 1,
                     legend = FALSE,
                     legend_label = NULL,
                     dens = c(-1, -1), overlay = FALSE, ...){
  par(mar = c(0, 0, 1, 0))
  
  if (is.null(axis)){
    axis <- pretty(c(0, 1.005*max(unlist(values))))
  }
  if (is.null(cates)){
    cates <- seq(1, length(values[[1]]))
  }
  
  if(!overlay == TRUE){
    side <- ifelse(side_label == 1, 1.1, 1.3)
    if (bar_way == 2) {
      bx <- c(-side - gap / 2, side + gap / 2)  
    } else {
      if(bar_side == 1) {
        bx <- c(-side - gap / 2, 0)
      } else{
        bx <- c(0, side + gap / 2)
      }
    }
    by <- c(0, 1.10)
    plot(bx, by, type = "n", axes = FALSE, xlab = "", ylab = "", ...)
  }
  
  if (groups > length(cols)){
    cols <- c("#4C00FF", "#0019FF", "#0080FF", "#00E5FF", "#00FF4D", "#E6FF00",
              "#FFFF00", "#FFDE59")
    }
  
  max_axis <- max(axis)
  min_axis <- min(axis)
  range_axis <- max_axis-min_axis
  ticks <- length(axis)
  # draw axis ticks
  ticks_loc <- (axis-min_axis)/range_axis+gap/2
  axis_loc <- c(1 + gap / 2, gap / 2)
  axis_label_loc <- 0.5 + gap / 2
  if (bar_side == 1){
    ticks_loc <- -ticks_loc
    axis_loc <- -axis_loc
    axis_label_loc <- -axis_label_loc
  }
  
  # draw axis ticks
  segments(ticks_loc, 1.02, ticks_loc, 1.03)
  #segments((laxis - LR) / LS + gap / 2, 1.02, (laxis - LR) / LS + gap / 2, 1.03)
  # draw axis line
  lines(axis_loc, c(1.02, 1.02), lty = 1)
  # draw axis tick label
  text(ticks_loc, rep(1.02, length(ticks_loc)),
       paste(formatC(axis, format = "g")), pos = 3, cex= csize*0.9)
  # draw axis label
  text(axis_label_loc, 1.08,
       paste(ifelse(bar_side == 1, label[1], label[2])),
       pos = 3, cex = csize, font = 2)
  
  # draw grid lines
  if (gl) {
    segments(ticks_loc, 0, ticks_loc, 1, lty = 3, col = "gray")
  }
  
  Ci <- length(values[[1]])        #calculate groups number
  VB <- 0:(Ci - 1) / Ci    #calculate vertical start position for each group
  VT <- 1:Ci / Ci          #calculate vertical end position fro each group
  # draw the side label
  if (side_label == 1){
    text(ifelse(bar_side == 1, -gap/2, gap/2), (VB + VT) / 2 + adj,
         as.character(cates), pos = ifelse(bar_side==1,4,2), cex = csize*0.9)
  } else {
    text(axis_loc[1], (VB + VT) / 2 + adj,
         as.character(cates), pos = ifelse(bar_side==1,2,4), cex = csize*0.9)
  }

  VT <- (VT - (VT - VB) * (1 - space))
  h <- (VT - VB) / groups
  
  
  # draw the left bar
  left_ <- lapply(values, function(x) -(x - min_axis) / range_axis - gap / 2)
  bottom_ <- lapply(1:length(values), function(i) VB + (i-1)*h)
  top_ <- lapply(1:length(values), function(i) VB + i*h)
  
  if (bar_side == 1){
    for(i in 1:length(values)){
      rect(left_[[i]], bottom_[[i]], rep(-gap / 2, Ci), top_[[i]],
           col = cols[i], border = cols[i], density = dens[1])
    }
  } else {
    for(i in 1:length(values)){
      rect(-rep(-gap / 2, Ci), bottom_[[i]], -left_[[i]], top_[[i]],
           col = cols[i], border = cols[i], density = dens[1])
    }
  }
  if (legend & length(values)>1){
    if(is.null(legend_label)){ legend_label <- names(values) }
    legend(ifelse(bar_side == 1,-0.4-gap/2, 0.4+gap/2), 0.15,
           legend = legend_label, border = "transparent",
           bty = "n", fill = cols, cex = csize*0.8)  
  }
}



#' Draw group bar chart.
#'
#' @param data A data frame or tibble.
#' @param plot_var Var to be plotted.
#' @param cate_var Category variable name.
#' @param group_var Group variable name.
#' @param side_var Panel variable name.
#' @param side_label Labels for each side.
#' @param bar_side Orientation of the bar chart: 1 for left and 2 for right.
#' @param topn Top n 'plot_var' in each 'group_var' and 'side_var'. 
#' @param grid The number of rows and columns in the plot area.
#' @param ... Other options in draw_bar.
#'
#' @return Bar chart
#' @export
#'
#' @examples
#' data <- load_canreg()
#' rate <- create_asr(data, year, sex, cancer, event = fbs)
#' draw_barchart(rate)
draw_barchart <- function(data,
                          plot_var = cr,
                          cate_var = cancer,
                          group_var = year,
                          side_var = sex,
                          side_label = NULL,
                          bar_side = 1,
                          topn = 10,
                          grid = c(1, 1),
                          ...) {
  data <- data |> arrange({{side_var}})
  data_pre <- data %>% group_split({{side_var}})
  side_value <- data %>% pull({{side_var}}) %>% unique()
  max_plot_var <- data %>% pull({{plot_var}}) %>% max()
  axis <- pretty(c(0, max_plot_var))
  way <- ifelse(length(data_pre) == 2, 2, 1)
  if(length(data_pre) == 2){
    sides <- c(1, 2)
    over_lay <-  c(FALSE, TRUE)
  } else {
    sides <- rep(bar_side, length(data_pre))
    over_lay <- c(rep(FALSE, length(data_pre)))
  }
  
  if (!is.null(side_label)){
    side_value <- side_label
  }
  
  plotbar <- function(i){
    #arrange data by group_var and plot_var.
    yy <- data_pre[[i]] %>% arrange({{group_var}}, desc({{plot_var}}))
    cates <- yy %>% slice_head(n=topn) %>% pull({{cate_var}})
    cates <- rev(cates)
    yy2 <- yy %>% filter({{cate_var}} %in% cates) %>% pull({{plot_var}})
    group_name <- yy %>% pull({{group_var}}) %>% unique()
    values <- as.list(as.data.frame(array(yy2, dim = c(topn, length(group_name)))))
    values <- lapply(values, rev)
    values <- rev(values)
    names(values) <- group_name
    draw_bar(values, cates, axis,
             bar_way = way,
             bar_side = sides[i],
             overlay = over_lay[i],
             label = c(rep(side_value[i], 2)),
             side_label = ifelse(length(data_pre)==2, 2, 1),
             legend = ifelse(i == 1, TRUE, FALSE),
             ...)
  }

  par(mfrow = grid)
  
  for(i in 1:length(side_value)){
    plotbar(i)
  }
  
  par(mfrow=c(1, 1))
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
draw_dumbbell <- function(data,
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
