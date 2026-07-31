# draw_linechart validates input data and variables

    Code
      draw_linechart(list(x = 1:3, y = 1:3), x, y)
    Condition
      Error in `draw_line()`:
      ! `data` must be a data frame.

---

    Code
      draw_linechart(data.frame(x = 1:3, y = 1:3), missing_x, y)
    Condition
      Error in `draw_line()`:
      ! Variable `missing_x` not in data.

