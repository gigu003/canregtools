

create_table <- function(x,
                         lang = "en",
                         groups = c("sex"),
                         vars = c("no_cases", "cr", "asr_cn2000", "asr_wld85", "cumur")) {
  
  set_flextable_defaults(background.color = "lightblue",
                         font.color = "purple",
                         border.color = "gray")
  all_vars <- c(groups, vars)
  res <- x |> 
    select(all_of(groups), all_of(vars))
  
  label_vars <- label_vars |> 
    filter(varname %in% all_vars)
  
  if (lang == "en") {
    label_vars <- select(label_vars, varname, label_en)
  } else if (lang == "zh-cn"){
    label_vars <- select(label_vars, varname, label_cn)
  }
  
  # Create the flextable
  res <- res |>
    flextable() |> 
    set_header_df(mapping = label_vars, key = "varname") |> 
    merge_v(j = groups, part = "body") |> 
    colformat_double(digits = 2) |> 
    align(align = "right", part = "header") |>
    align(align = "right", part = "all") |> 
    hline_bottom() |> 
    hline_top(part = "all") |> 
    valign(j = 1, valign = "top") |>
    fix_border_issues()
  
  
  res <- set_table_properties(res, width = 0.9, layout = "autofit")
  
  # add footer line
  res <- res |> 
    add_footer_lines(as_paragraph(as_sup("1"),"Source: ")) |> 
    add_header_lines("Table:")
  
  return(res)
}
