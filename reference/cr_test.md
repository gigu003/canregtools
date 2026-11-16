# Run cancer-registry quality tests

`cr_test()` provides one entry point for the registry-level statistical
tests implemented by
[`compare_mi()`](https://gigu003.github.io/canregtools/reference/compare_mi.md),
[`compare_mv()`](https://gigu003.github.io/canregtools/reference/compare_mv.md),
and
[`compare_rate()`](https://gigu003.github.io/canregtools/reference/compare_rate.md).
For quality data, the default runs the M:I and MV% tests. For an `asr`
object, the default runs incidence and mortality rate tests.

## Usage

``` r
cr_test(
  x,
  test = c("all", "mi", "mv", "inci", "mort"),
  reference = NULL,
  standard = "wld85",
  alpha = 0.05,
  multiplier = 1e+05,
  variance_scale = c("display", "unmultiplied")
)
```

## Arguments

- x:

  An object with class `quality` or `qualities`, typically returned by
  [`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md),
  or an object with class `asr`/`asrs` returned by
  [`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md).

- test:

  The test to run. `"all"` runs M:I and MV% for quality data, or
  incidence and mortality rate tests for ASR data. `"mi"` and `"mv"`
  select quality tests; `"inci"` and `"mort"` select CI5-style rate
  tests.

- reference:

  An optional external reference table passed to the selected
  comparison. For `mi` and `mv`, it must contain the required counts;
  for `inci` and `mort`, it must contain the selected rate and variance
  columns. CI5-XII incidence data alone do not contain death counts or
  MV counts and cannot be used directly for the first two tests.

- standard:

  Standard-population suffix used by the rate test. Defaults to
  `"wld85"`. Use `"cr"` for crude rates.

- alpha:

  A single numeric significance level strictly between 0 and 1. Defaults
  to `0.05`.

- multiplier:

  Rate multiplier used only for legacy unmultiplied rate variances.
  Defaults to `100000`.

- variance_scale:

  Scale of rate variance columns passed to
  [`compare_rate()`](https://gigu003.github.io/canregtools/reference/compare_rate.md).
  The default, `"display"`, matches current
  [`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)
  output; use `"unmultiplied"` for legacy variance columns.

## Value

A tibble containing the selected result for a single test. With
`test = "all"`, a named list with class `cr_tests` is returned.

## Examples

``` r
quality_data <- structure(
  data.frame(
    regicode = c("A", "B", "C", "D"),
    year = "2024",
    sex = "1",
    cancer = "101",
    fbs = c(100, 100, 150, 200),
    sws = c(80, 40, 75, 80),
    mv = c(95, 70, 80, 75)
  ),
  class = c("quality", "data.frame")
)
tests <- cr_test(quality_data)
tests$mi
#> # A tibble: 4 × 15
#>   regicode year  sex   cancer   fbs   sws    mi reference_n theta   phi     z2
#>   <chr>    <chr> <chr> <chr>  <dbl> <dbl> <dbl>       <int> <dbl> <dbl>  <dbl>
#> 1 A        2024  1     101      100    80   0.8           3 0.433 0.788 21.9  
#> 2 B        2024  1     101      100    40   0.4           3 0.522 6.19   0.330
#> 3 C        2024  1     101      150    75   0.5           3 0.5   7.14   0    
#> 4 D        2024  1     101      200    80   0.4           3 0.557 4.82   1.31 
#> # ℹ 4 more variables: p_value <dbl>, critical_value <dbl>, unusual <lgl>,
#> #   status <chr>
```
