# Count and classify `canreg` data

The `count_canreg()` function is a generic method used to summarize
population-based cancer registry data. It supports both single
(`canreg`) and multiple (`canregs`) registry objects. The function
aggregates cancer cases by age group and classifies cancer types using
standardized coding systems.

## Usage

``` r
count_canreg(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big",
  strategy = c("batch", "registry")
)

# S3 method for class 'canregs'
count_canreg(x, ..., strategy = c("batch", "registry"))

# S3 method for class 'canreg'
count_canreg(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big",
  strategy = c("batch", "registry")
)
```

## Arguments

- x:

  Object with class of 'canreg' or 'canregs'.

- age_breaks:

  A numeric vector specifying the breakpoints for age grouping. Defaults
  to `c(0, 1, seq(5, 85, 5))`.

- label_tail:

  Optional. A string to append to age group labels (e.g., `"+"` for
  open-ended intervals). Defaults to `NULL`.

- cancer_type:

  A character string specifying the classification method used to
  categorize ICD-10 codes. This determines how ICD-10 codes are
  classified. Options include `"big"` (classify ICD-10 codes into 26
  cancer categories), `"small"` (classify ICD-10 codes into 59 cancer
  categories, more specific categories), `"system"` (classify ICD-10
  codes into organ system), and `"gco"` (classify ICD-10 code into
  cancer categories same as classification published by the Global
  Cancer Observatory), and `"ci5"` (classify ICD-10 codes using the
  63-category CI5-XII classification, with shared aggregate codes `60`
  and `61`). This parameter is only available when the input data is a
  vector of ICD-10 codes, or object with class of `'canreg'` or
  `'canregs'`.

- strategy:

  Counting strategy for a canregs object. The default, "batch", cleans
  and aggregates all registry case tables together. Use "registry" for
  the original one-registry-at-a-time path.

- ...:

  Additional arguments passed to the method for individual `canreg`
  objects.

## Value

Object with class of 'fbswicd' or 'fbswicds'.

## See also

[`cr_clean()`](https://gigu003.github.io/canregtools/reference/cr_clean.md),
[`classify_icd10()`](https://gigu003.github.io/canregtools/reference/classify_icd10.md),
[`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)

## Examples

``` r
data("canregs")
fbsw <- count_canreg(canregs, age_breaks = c(0, 15, 65), cancer_type = "big")
fbsw <- count_canreg(canregs, cancer_type = "gco")
fbsw <- count_canreg(canregs, cancer_type = "ci5")

# Count object with class of `canregs`
fbsw <- count_canreg(canregs, cancer_type = "small")

# Count object with class of `canreg`
fbsw <- count_canreg(canregs[[1]], cancer_type = "big")
```
