# Plot dumbbell chart

Plot dumbbell chart

## Usage

``` r
draw_dumbbell(
  data,
  x = NULL,
  y1 = NULL,
  y2 = NULL,
  topn = 20,
  sort = "insc",
  legend = NULL,
  cols = c("#006400", "gray", "#b32134"),
  gl = NULL,
  gl_col = c("gray"),
  main = ""
)
```

## Arguments

- data:

  A data frame contains data to be plotted.

- x:

  A category variable in data.

- y1:

  Variable indicate start point.

- y2:

  Variable indicate end point.

- topn:

  Top n values to be plotted.

- sort:

  Sort options.

- legend:

  Legends.

- cols:

  Colors of the start and end points.

- gl:

  Integer. Indicating the line type of the grids.

- gl_col:

  Color of the background grid.

- main:

  Main title of the plot.

## Value

A dumbbell plot.

## Examples

``` r
asr <- create_asr(canregs[[1]], year, cancer, show_ci = TRUE) |>
  drop_others() |>
  drop_total() |>
  add_labels(vars = "cancer", lang = "en", label_type = "abbr")
draw_dumbbell(asr, "cancer_en", asr_lower_cn2000, asr_upper_cn2000, topn = 15)

```
