# Drop the total cancer

Filter out cases that summarized as `total` when using
[`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md),
[`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md),
or
[`create_age_rate()`](https://gigu003.github.io/canregtools/reference/create_age_rate.md).

## Usage

``` r
drop_total(x)
```

## Arguments

- x:

  A data frame containing a `cancer` variable created by
  [`classify_icd10()`](https://gigu003.github.io/canregtools/reference/classify_icd10.md).

## Value

A filtered data frame excluding `cancer` in `c("60", "61")`.

## Examples

``` r
data("canregs")
qua <- create_quality(canregs, year, sex, cancer)
qua2 <- qua |> drop_total()
```
