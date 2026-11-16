# Drop “other” cancer codes

Filters out cases classified as "other" cancer types based on specific
codes.

## Usage

``` r
drop_others(x)
```

## Arguments

- x:

  A data frame containing a `cancer` variable created by
  [`classify_icd10()`](https://gigu003.github.io/canregtools/reference/classify_icd10.md).

## Value

A filtered data frame excluding non-specific cancer codes.

## Examples

``` r
data("canregs")
asr <- create_quality(canregs, year, sex, cancer)
asr2 <- asr |> drop_others()
```
