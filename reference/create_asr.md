# Calculate age-standardized rate (ASR)

`create_asr()` calculate age-standardized rates (ASRs) from object with
class of `canreg`, `canregs`, `fbswicd`, or `fbswicds`. It supports
stratification by multiple variables, allows the specification of
different standard population structures, and provides flexibility in
the inclusion of variance, standard errors, confidence intervals, and
population data.

## Usage

``` r
create_asr(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  cancer_type = "big",
  mp = 1e+05,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE,
  collapse = TRUE
)

# S3 method for class 'canregs'
create_asr(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  cancer_type = "big",
  mp = 1e+05,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE,
  collapse = TRUE
)

# S3 method for class 'canreg'
create_asr(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  cancer_type = "big",
  mp = 1e+05,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE
)

# S3 method for class 'fbswicds'
create_asr(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  mp = 1e+05,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE,
  collapse = TRUE
)

# S3 method for class 'fbswicd'
create_asr(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  mp = 1e+05,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE
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

- std:

  Specify the standard population structure in the 'std_pop' data frame
  used for calculating standardized rates. When calculating standardized
  rates for multiple standard populations, specify std = c(segi, china).

- age_breaks:

  Numeric vector of lower bounds for input age groups. It must begin at
  0 and end no later than 85. The final group is open-ended. If omitted,
  the bounds are inferred from `agegrp`.

- std_method:

  Method used to align standard-population weights with `age_breaks`:
  `"exact"` combines original groups, while `"interpolate"` estimates
  single-year weights before regrouping.

- interpolation_method:

  Interpolation method used when `std_method = "interpolate"`.

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

- mp:

  A constant to multiply rates by (e.g. mp=1000 for rates per 1000).

- decimal:

  This parameter specifies the number of decimal places to round the
  results. The default is 2, which means rates will be rounded to two
  decimal places.

- show_pop:

  Logical value whether output population or not.

- show_var:

  Logical value indicating whether to output variances. Variances use
  the squared scale of the reported rates, so their square roots equal
  the corresponding standard errors before rounding.

- show_se:

  Logical value indicating whether to output standard errors. Standard
  errors use the same scale as the reported rates.

- show_ci:

  Logical value whether output confidence(lower or upper bound) or not.

- collapse:

  Logical value whether output result as asr or asrs.

## Value

A data frame or tibble contains the age standard rates and CIs.

## Details

Standard populations use 19 age groups by default: age 0, 1–4, 5–9, ...,
and 85+. When the input uses 18 five-year age groups beginning with 0–4,
`create_asr()` automatically combines the age-0 and age-1–4
standard-population weights before calculating the ASR.

Supply `age_breaks` to use another compatible grouping. By default,
groups must only combine the original standard-population groups; set
`std_method = "interpolate"` when a requested boundary splits one.

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

## See also

[`ageadjust`](https://gigu003.github.io/canregtools/reference/ageadjust.md)
for age-adjusted rate calculations.
[`truncrate`](https://gigu003.github.io/canregtools/reference/truncrate.md)
for truncated rate calculations.

## Examples

``` r
data("canregs")
asr_inci <- create_asr(canregs, event = "fbs", year, sex, cancer)
asr_mort <- create_asr(canregs, event = "sws", year, sex, cancer)

# calculate ASR based on object with class of `canregs`
asr <- create_asr(canregs, event = "sws", year, sex, cancer)

# calculate ASR based on object with class of `canreg`
data <- canregs[[1]]
# calculate ASR using default parameter
asr <- create_asr(data, year, sex, cancer)
head(asr)
#> # A tibble: 6 × 12
#>   year  sex   cancer no_cases    cr asr_cn2000 asr_wld85 truncr_cn2000
#>   <chr> <chr> <chr>     <dbl> <dbl>      <dbl>     <dbl>         <dbl>
#> 1 2021  0     101          38  5.53       3.74      3.79          5.6 
#> 2 2021  0     102           5  0.73       0.71      0.71          1.29
#> 3 2021  0     103          59  8.58       4.53      4.85          6.13
#> 4 2021  0     104         123 17.9       11.1      11.2          13.7 
#> 5 2021  0     105         236 34.3       20.6      20.2          28.4 
#> 6 2021  0     106         128 18.6       11.5      11.4          17.1 
#> # ℹ 4 more variables: truncr_wld85 <dbl>, cumur <dbl>, prop <dbl>, rank <int>
# calculate ASR using multiple standard population
asr_multi_std <- create_asr(data, year, sex, cancer,
  std = c("cn82", "cn2000", "wld85")
)
head(asr_multi_std)
#> # A tibble: 6 × 14
#>   year  sex   cancer no_cases    cr asr_cn82 asr_cn2000 asr_wld85 truncr_cn82
#>   <chr> <chr> <chr>     <dbl> <dbl>    <dbl>      <dbl>     <dbl>       <dbl>
#> 1 2021  0     101          38  5.53     3.07       3.74      3.79        6.14
#> 2 2021  0     102           5  0.73     0.74       0.71      0.71        1.32
#> 3 2021  0     103          59  8.58     3.26       4.53      4.85        6.85
#> 4 2021  0     104         123 17.9      8.33      11.1      11.2        15.0 
#> 5 2021  0     105         236 34.3     14.9       20.6      20.2        30.5 
#> 6 2021  0     106         128 18.6      8.6       11.5      11.4        18.4 
#> # ℹ 5 more variables: truncr_cn2000 <dbl>, truncr_wld85 <dbl>, cumur <dbl>,
#> #   prop <dbl>, rank <int>
# calculate ASR with confidence interval
asr_with_ci <- create_asr(data, year, sex, cancer, show_ci = TRUE)
head(asr_with_ci)
#> # A tibble: 6 × 18
#>   year  sex   cancer no_cases    cr cr_lower cr_upper asr_cn2000
#>   <chr> <chr> <chr>     <dbl> <dbl>    <dbl>    <dbl>      <dbl>
#> 1 2021  0     101          38  5.53     3.91     7.59       3.74
#> 2 2021  0     102           5  0.73     0.24     1.7        0.71
#> 3 2021  0     103          59  8.58     6.53    11.1        4.53
#> 4 2021  0     104         123 17.9     14.9     21.4       11.1 
#> 5 2021  0     105         236 34.3     30.1     39.0       20.6 
#> 6 2021  0     106         128 18.6     15.5     22.1       11.5 
#> # ℹ 10 more variables: asr_lower_cn2000 <dbl>, asr_upper_cn2000 <dbl>,
#> #   asr_wld85 <dbl>, asr_lower_wld85 <dbl>, asr_upper_wld85 <dbl>,
#> #   truncr_cn2000 <dbl>, truncr_wld85 <dbl>, cumur <dbl>, prop <dbl>,
#> #   rank <int>
# calculate ASR with population at risk
asr_with_pop <- create_asr(data, year, sex, cancer, show_pop = TRUE)
head((asr_with_pop))
#> # A tibble: 6 × 13
#>   year  sex   cancer    pop no_cases    cr asr_cn2000 asr_wld85 truncr_cn2000
#>   <chr> <chr> <chr>   <int>    <dbl> <dbl>      <dbl>     <dbl>         <dbl>
#> 1 2021  0     101    687356       38  5.53       3.74      3.79          5.6 
#> 2 2021  0     102    687356        5  0.73       0.71      0.71          1.29
#> 3 2021  0     103    687356       59  8.58       4.53      4.85          6.13
#> 4 2021  0     104    687356      123 17.9       11.1      11.2          13.7 
#> 5 2021  0     105    687356      236 34.3       20.6      20.2          28.4 
#> 6 2021  0     106    687356      128 18.6       11.5      11.4          17.1 
#> # ℹ 4 more variables: truncr_wld85 <dbl>, cumur <dbl>, prop <dbl>, rank <int>
# calculate ASR with standard errors
asr_with_se <- create_asr(data, year, sex, cancer, show_se = TRUE)
head(asr_with_se)
#> # A tibble: 6 × 15
#>   year  sex   cancer no_cases    cr cr_se asr_cn2000 asr_se_cn2000 asr_wld85
#>   <chr> <chr> <chr>     <dbl> <dbl> <dbl>      <dbl>         <dbl>     <dbl>
#> 1 2021  0     101          38  5.53  0.9        3.74          0.68      3.79
#> 2 2021  0     102           5  0.73  0.33       0.71          0.35      0.71
#> 3 2021  0     103          59  8.58  1.12       4.53          0.63      4.85
#> 4 2021  0     104         123 17.9   1.61      11.1           1.08     11.2 
#> 5 2021  0     105         236 34.3   2.23      20.6           1.41     20.2 
#> 6 2021  0     106         128 18.6   1.65      11.5           1.07     11.4 
#> # ℹ 6 more variables: asr_se_wld85 <dbl>, truncr_cn2000 <dbl>,
#> #   truncr_wld85 <dbl>, cumur <dbl>, prop <dbl>, rank <int>

# calculate ASR based on object with class of `fbswicds`
# convert object with class of `canregs` to object with class of `fbswicds`
fbsws <- count_canreg(canregs)
asrs <- create_asr(fbsws, event = "sws", year, sex, cancer)

# calculate ASR based on object with class of `fbswicd`
# convert object with class of `canreg` to object with class of `fbswicd`
fbsw <- count_canreg(canregs[[1]])
asr <- create_asr(fbsw, event = "sws", year, sex, cancer)
```
