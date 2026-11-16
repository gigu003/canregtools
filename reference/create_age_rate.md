# Calculate age specific rate

`create_age_rate()` computes age-specific rates from object with class
of `canreg`,`fbswicd`, or `canregs`, `fbswicds`. It calculates the rates
for specified events (e.g., `fbs`) across age groups, stratified by
variables such as year, sex, or cancer type.

## Usage

``` r
create_age_rate(
  x,
  ...,
  event = "fbs",
  cancer_type = "big",
  format = "long",
  mp = 1e+05,
  decimal = 6,
  show_pop = FALSE,
  collapse = TRUE
)

# S3 method for class 'canreg'
create_age_rate(x, ..., cancer_type = "big")

# S3 method for class 'canregs'
create_age_rate(x, ..., cancer_type = "big", collapse = TRUE)

# S3 method for class 'fbswicds'
create_age_rate(
  x,
  ...,
  event = "fbs",
  format = "long",
  mp = 1e+05,
  decimal = 6,
  show_pop = FALSE,
  collapse = TRUE
)

# S3 method for class 'fbswicd'
create_age_rate(
  x,
  ...,
  event = "fbs",
  format = "long",
  mp = 1e+05,
  decimal = 6,
  show_pop = FALSE
)
```

## Arguments

- x:

  The input data, object with class of `'fbswicd'`, `'fbswicds'`,
  `'canreg'`, or `'canregs'`.

- ...:

  One or more variables used for stratification. For example, you can
  stratify by `sex`, `year`, `cancer`, or just by `year`. If sex is not
  passed as a parameter, the output will be the result for the combined
  gender.

- event:

  A variable used to specify the type of calculation, options are "fbs"
  or "sws", "fbs" for cancer incidence, and "sws" for cancer mortality.

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

- format:

  Format of the output data frame, either "long" or "wide".

- mp:

  A constant to multiply rates by (e.g. mp=1000 for rates per 1000).

- decimal:

  This parameter specifies the number of decimal places to round the
  results. The default is 2, which means rates will be rounded to two
  decimal places.

- show_pop:

  Logical value whether output population or not.

- collapse:

  Logical value whether output result as age_rate or age_rates.

## Value

A data frame of age-specific rates.

## Details

For summary input, existing `sex = "0"` rows are retained once. Missing
overall rows are computed from available male/female rows at each
original stratum before grouping. If an overall row and both
sex-specific rows are supplied, inconsistent totals cause an error.
Overall-only input is supported; sex-specific rows are not inferred from
overall values. Within each observed year/sex/cancer stratum, absent
summary age rows are interpreted as zero counts and completed from
matching population age groups. Entirely absent strata are not inferred.
Supplied missing, non-finite, or negative counts cause an error;
population rows are never invented or filled with zero.

## Examples

``` r
data("canregs")
agerate <- create_age_rate(canregs, year, sex, cancer)

data <- canregs[[1]]
agerate <- create_age_rate(data, year, sex)

agerate <- create_age_rate(canregs, year, cancer_type = "system")

fbsws <- count_canreg(canregs)
agerate <- create_age_rate(fbsws, year, sex)

data <- canregs[[2]]
fbsw <- count_canreg(data, cancer_type = "small")
agerate <- create_age_rate(fbsw, year, sex, cancer)
```
