# Add Variable Labels with Units

Returns formatted labels (with optional units) for commonly used
statistical variables in cancer registry reporting, such as incidence,
mortality, crude rate, age-standardized rate, and proportion.

## Usage

``` r
add_var_labels(x, label_type = "abbr", lang = "cn", break_line = TRUE)
```

## Arguments

- x:

  A character vector of variable codes to be labeled (e.g., `"cr"`,
  `"asr"`, `"prop"`).

- label_type:

  Label style: `"abbr"` (default) for abbreviation, `"full"` for full
  name.

- lang:

  Language for the labels. One of `"cn"` (default) for Chinese, `"en"`
  for English. Special options `"code"` or `"icd10"` return the variable
  code or ICD-10 code instead.

- break_line:

  Logical. If `TRUE` (default), adds a line break (`\\n`) before the
  unit. If `FALSE`, appends the unit directly after the label, suitable
  for HTML rendering (e.g., in `flextable`).

## Value

A character vector of the same length as `x`, where each element is a
formatted label.

## Details

This function looks up the corresponding label for each input variable
code in the internal dictionary `tidy_var_maps[["stats"]]`, and
optionally adds a unit suffix (e.g., `(1/10<sup>5</sup>)` or `(%)`)
depending on the variable type. It supports Chinese and English, full
names or abbreviations, and can format the unit with or without a line
break.

## Examples

``` r
add_var_labels(c("cr", "asr"))
#> [1] "粗率\n(1/10<sup>5</sup>)" "NA"                      
add_var_labels(c("cr", "prop"), label_type = "full", lang = "en")
#> [1] "Crude Rate\n(1/10<sup>5</sup>)" "Proportional Rate\n(%)"        
add_var_labels("asr", break_line = FALSE)
#> [1] "NA"
```
