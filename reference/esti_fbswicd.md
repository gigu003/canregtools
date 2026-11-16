# Estimate object of fbswicd

This function estimates `fbswicd` object based on existing `fbswicd`
object and using the total population data.

## Usage

``` r
esti_fbswicd(obj, pop = NULL)
```

## Arguments

- obj:

  An object with class of `fbswicd`.

- pop:

  An population dataset to override the population in `obj`.

## Value

An estimated object with class of `fbswicd`.

## Details

The estimation proceeds in the following steps:

1.  Calculate age-specific incidence and mortality rates for different
    sex and cancer sites.

2.  Derive estimated counts of `fbs` and `sws` using the rate ×
    population formula.

3.  Recalculate proportions (e.g., `mv`, `dco`) back to estimated case
    counts.

4.  Join all information and return a new object of the same class as
    `obj`.

## See also

[`create_age_rate()`](https://gigu003.github.io/canregtools/reference/create_age_rate.md),
[`cr_filter()`](https://gigu003.github.io/canregtools/reference/cr_filter.md)
