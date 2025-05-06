## code to prepare `create_logo` dataset goes here
library(hexSticker)
library(png)
writer <- grid::rasterGrob(readPNG("data-raw/logo.png"), interpolate = TRUE)
sticker(
  writer,
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


std_var_names <- c("sex", "edu", "trib", "occu", "marri",
                   "grad", "beha", "basi", "treat",
                   "status", "caus", "deadplace", "lost",
                   "stats")
dates <- c("birthda", "inciden", "lastcontact", "deathda")
female_cancer <- c(29:37, 114:117, 206, 320:325)
male_cancer <- c(38:41, 118:119, 207, 326:328)
p_vars <- names(x)
av_vars <- p_vars[p_vars %in% std_var_names]
av_dates <- p_vars[p_vars %in% dates]
res <- x |>
  mutate(across(c(!!!rlang::syms(av_vars)),
                ~ tidy_var(.x, var_name = cur_column())),
         across(c(!!!rlang::syms(av_dates)), as.Date))
