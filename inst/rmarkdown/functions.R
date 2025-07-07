cr_color_block2 <- function() {
  theme_minimal(base_size = 13) +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 0),
      strip.background = element_blank(),
      strip.text = element_text(size = 13, face = "bold.italic"),
      panel.grid = element_blank()
    )
}

cr_color_block <- function() {
  theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      strip.text = element_text(size = 10, face = "bold.italic"),
      strip.background = element_rect(color = "white", size = 0),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 9, face = "bold.italic"),
      axis.text.x = element_text(size = 9, face = "bold.italic", angle = 90),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8)
    )
}

cr_grid <- function() {
  theme_light() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 10, face = "bold.italic"),
      strip.background = element_rect(color = "white", linewidth = 0),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 9, face = "bold.italic"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(face = "bold.italic", size = 12,
                                   hjust = 0.5),
      panel.border = element_rect(colour = "blue", fill = NA, linetype = 2),
      panel.background = element_rect(fill = "aliceblue"),
      panel.grid.major.x = element_line(colour = "steelblue", linetype = 3,
                                        size = 0.5),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3,
                                         size = 0.5),
      panel.grid.minor.y = element_blank()
    )
}

bbarplot <- function(data, x, y, fill, facet, label,
                     xlab = "年份", ylab = "发病数/死亡数",
                     title = "历年发病数(死亡数)变化情况",
                     caption = "caption") {
  ggplot(data, aes(x = factor({{x}}), y = {{y}})) +
    geom_bar(aes(fill = {{fill}}), stat = "identity", position = "dodge") +
    facet_grid({{facet}}, scales = "free_y") +
    geom_text(aes(label = {{y}}), vjust = 2, color = "white", size = 2.5) +
    geom_text(aes(label = {{label}}), vjust = 4, color = "black", size = 2.5) +
    theme_classic() +
    xlab(xlab) +
    ylab(ylab) +
    labs(title = title, caption = caption) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold.italic"),
      strip.background = element_rect(color = "white", linewidth = 0),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )
}

bscatter <- function(data, x, y, fill, label, facet,
                     xlab = "年份",
                     ylab = "部位",
                     title = "",
                     caption = "") {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_tile(aes(fill = {{fill}})) +
    geom_text(aes(label = {{label}}), vjust = 1.5, color = "black",
              size = 2.5) +
    scale_x_discrete(position = "top") +
    scale_fill_gradient(low = "white", high = "purple") +
    facet_wrap({{facet}}, strip.position = "bottom") +
    labs(x = xlab, y = ylab, title = title, caption = caption) +
    cr_color_block()
}
