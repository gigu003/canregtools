# Detect unusual annual rates or quality indicators

`detect_anomaly()` identifies annual observations that deserve review in
outputs from
[`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)
or
[`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md).
The default `"uncertainty"` method compares consecutive years on the
log-rate scale and uses the standard errors implied by the underlying
counts or reported variances. The `"robust"` method compares each
observation with the historical median using the median absolute
deviation (MAD).

## Usage

``` r
detect_anomaly(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 1e+05,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
)

# S3 method for class 'asr'
detect_anomaly(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 1e+05,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
)

# S3 method for class 'asrs'
detect_anomaly(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 1e+05,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
)

# S3 method for class 'quality'
detect_anomaly(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 1e+05,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
)

# S3 method for class 'qualities'
detect_anomaly(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 1e+05,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
)

# Default S3 method
detect_anomaly(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 1e+05,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
)
```

## Arguments

- x:

  An object returned by
  [`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)
  or
  [`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md).
  Objects returned with `collapse = FALSE` are also supported.

- metrics:

  Character vector of metric columns to check. For `asr` objects the
  default is `c("cr", "asr_wld85")`; for `quality` objects the default
  is `c("inci", "mort", "mv", "mi")`.

- method:

  Detection method: `"uncertainty"` compares consecutive years, while
  `"robust"` uses the historical median and MAD.

- by:

  Character vector of columns defining independent time series. The
  default uses the available columns among `regicode`, `sex`, and
  `cancer`.

- year:

  Name of the year column. Defaults to `"year"`.

- threshold:

  Absolute score at which an observation is flagged. Defaults to `3`.

- min_history:

  Minimum number of usable historical values required by the robust
  method. Defaults to `4`.

- multiplier:

  Rate multiplier used by
  [`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)
  and
  [`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md).
  For ASR variance columns, it is used only when
  `variance_scale = "unmultiplied"`. Defaults to `100000`.

- zero_policy:

  Whether zero-valued observations are excluded from comparisons or
  included where the selected method permits it. The default is
  `"exclude"`.

- variance_scale:

  Scale of ASR variance columns. The default, `"display"`, matches
  current
  [`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)
  output. Use `"unmultiplied"` for legacy variance columns.
  Standard-error columns are always interpreted on the displayed rate
  scale.

## Value

A tibble with one row per input observation and metric. It includes
grouping columns, `year`, `metric`, `value`, `expected`, `se`,
`reference_se`, `scale`, `score`, `p_value`, `direction`, `anomaly`, and
`status`.

## Details

This function is intended for quality-control screening. An `anomaly`
flag is not evidence that a registry value is wrong, and the unadjusted
`p_value` returned by the uncertainty method is not a substitute for a
prespecified trend model or multiplicity-adjusted inference.

## References

International Agency for Research on Cancer. *Cancer Incidence in Five
Continents, Volume XII: Statistical methods for registries*.
<https://publications.iarc.who.int/_publications/media/download/3523/>

## See also

[`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md),
[`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md)

## Examples

``` r
data("canregs")
fbsws <- count_canreg(canregs)
asr <- create_asr(
  fbsws,
  year,
  show_var = TRUE,
  collapse = TRUE
)
detect_anomaly(asr, metrics = "asr_wld85")
#> # A tibble: 3 × 15
#>   regicode sex   cancer year  metric    value expected    se reference_se scale
#>   <chr>    <chr> <chr>  <chr> <chr>     <dbl>    <dbl> <dbl>        <dbl> <dbl>
#> 1 410103   0     60     2021  asr_wld85  256.       NA  5.30           NA    NA
#> 2 410307   0     60     2021  asr_wld85  197.       NA  5.39           NA    NA
#> 3 411721   0     60     2016  asr_wld85  192.       NA  4.25           NA    NA
#> # ℹ 5 more variables: score <dbl>, p_value <dbl>, direction <chr>,
#> #   anomaly <lgl>, status <chr>
```
