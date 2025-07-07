## code to prepare `create_logo` dataset goes here
library(hexSticker)
library(png)
chart <- grid::rasterGrob(readPNG("data-raw/logo.png"), interpolate = TRUE)
sticker(
  chart,
  s_x = 1,
  s_y = 1.2,
  s_width = 0.6,
  s_height = 0.6,
  package = "canregtools",
  p_y = 0.65,
  p_size = 40,
  p_color = "#393e46",
  p_family = "sans",
  p_fontface = "bold",
  h_size = 0.3,
  h_color = "#0f1e1d",
  h_fill = "#c1d1cf",
  filename = "logo.png",
  asp = 1,
  dpi = 600
)

use_logo("logo.png")
fs::file_delete("logo.png")
