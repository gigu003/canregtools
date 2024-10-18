## code to prepare `create_logo` dataset goes here
library(hexSticker)
library(png)
writer <- grid::rasterGrob(readPNG("data-raw/zhexiantu.png"), interpolate = TRUE)
sticker(
  writer,
  s_x = 1,
  s_y = 1.2,
  s_width = 0.6,
  s_height = 0.6,
  package = "canregtools",
  p_y = 0.65,
  p_size = 40,
  p_color = "black",
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
