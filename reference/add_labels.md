# Add variable labels for data set.

`add_labels()` adds labels for selected variables such as `sex`,
`cancer`, and `regicode` in the dataset based on the specified language
and label type.

## Usage

``` r
add_labels(
  x,
  vars = c("sex", "cancer", "regicode"),
  names = NULL,
  label_type = "full",
  lang = "zh",
  sep = " ",
  suffix = FALSE,
  as_factor = TRUE
)
```

## Arguments

- x:

  A data frame or tibble to be labeled.

- vars:

  A character vector of variable names to label. Default includes
  `"sex"`, `"cancer"`, and `"regicode"`.

- names:

  A character vector contains the labels added.

- label_type:

  Type of the label used ("full" or "abbr").

- lang:

  Character, specify the output language, options are 'cn', or 'en',
  default is 'cn'.

- sep:

  String used to separate labels when multiple languages are specified.

- suffix:

  Logical. Passed to
  [`tidy_var()`](https://gigu003.github.io/canregtools/reference/tidy_var.md).
  If `TRUE`, add a language-specific suffix to cancer labels when
  labeling `cancer`.

- as_factor:

  Logical, indicate whether output value as factor.

## Value

A data frame or tibble with labeled variables and reordered columns.

## Examples

``` r
data("canregs")
asr <- create_asr(canregs[[1]], year, sex, cancer)
asr <- add_labels(asr, label_type = "full", lang = "zh")
asr <- add_labels(asr, label_type = "full", lang = "en")
```
