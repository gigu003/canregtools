# Clean canreg data.

Clean canreg data.

## Usage

``` r
cr_clean(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
)

# S3 method for class 'canregs'
cr_clean(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
)

# S3 method for class 'canreg'
cr_clean(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
)

# S3 method for class 'FBcases'
cr_clean(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
)

# S3 method for class 'SWcases'
cr_clean(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
)

# S3 method for class 'POP'
cr_clean(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
)
```

## Arguments

- x:

  Data of class 'FBcases', 'SWcases' or 'population'.

- age_breaks:

  Cut points for age groups. Default is `c(0, 1, seq(5, 85, 5))`.

- label_tail:

  Tail of the labels.

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

## Value

Class 'canreg'.

## Details

Population input must contain one complete standard 18- or 19-group
schedule per year and sex, with numeric lower age bounds in `agegrp`.
Aligned groups are summed directly; finer groups use natural
interpolation with source-group totals preserved. The final open group
cannot be split. Empty population input remains empty.

## Examples

``` r
data("canregs")
data <- cr_clean(canregs)

data <- cr_clean(canregs, cancer_type = "small")

data <- cr_clean(canregs[[1]], cancer_type = "big")

fbcases <- purrr::pluck(canregs[[1]], "FBcases")
fbcases <- cr_clean(fbcases)

swcases <- purrr::pluck(canregs[[1]], "SWcases")
swcases <- cr_clean(swcases)

pop <- purrr::pluck(canregs[[1]], "POP")
pop <- cr_clean(pop)
```
