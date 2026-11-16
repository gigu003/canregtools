# Compare morphologically verified percentages across registries

`compare_mv()` applies the statistical test described by Bray and Parkin
(2009) to identify registry percentages of morphologically verified
cases (MV%) that differ from comparison registries. Each registry is
tested in turn, using all other registries in the same stratum as its
comparison population.

## Usage

``` r
compare_mv(x, alpha = 0.05, reference = NULL)
```

## Arguments

- x:

  An object with class `quality` or `qualities` containing the registry
  identifier `regicode`, incident case counts `fbs`, and MV percentages
  `mv`. An optional `mv_cases` column supplies exact MV case counts.

- alpha:

  A single numeric significance level strictly between 0 and 1. Defaults
  to `0.05`, corresponding to a chi-squared critical value of
  approximately 3.84 with one degree of freedom.

- reference:

  An optional external reference table. It must contain regicode, fbs,
  and mv, or mv_cases; registry and cases are accepted as aliases for
  regicode and fbs. At least two reference rows are needed in each
  matched stratum to estimate dispersion.

## Value

A tibble containing the stratum identifiers, `regicode`, `fbs`, the
observed `mv`, the MV count used in the calculation `mv_cases`, the
number of comparison registries `reference_n`, the estimated common
proportion `p`, the overdispersion estimate `phi`, the test statistic
`z2`, `p_value`, `critical_value`, the logical flag `unusual`, and an
evaluation `status`.

## Details

The function accepts either the merged `quality` data frame returned by
`create_quality(..., collapse = TRUE)` or the `qualities` list returned
by `create_quality(..., collapse = FALSE)`. Strata are detected
automatically from identifier columns such as `year`, `sex`, and
`cancer`.

By default, each registry is compared with the other registries in its
stratum. Supply reference to use an external reference table instead.
External rows are matched to each target stratum using the stratum
columns shared by x and reference. For comparison registries indexed by
\\i = 1, \ldots, n\\, let \\y_i\\ be the number of morphologically
verified cases and \\d_i\\ the total number of incident cases. The
common MV proportion and overdispersion are estimated as \$\$\hat{p} =
\frac{\sum_i y_i}{\sum_i d_i}\$\$ and \$\$\hat{\phi} = \frac{1}{n - 1}
\sum_i \frac{(y_i - \hat{p}d_i)^2}{d_i\hat{p}(1 - \hat{p})}.\$\$ For the
registry under test, indexed by \\j\\, the statistic is \$\$Z^2 =
\frac{(y_j - \hat{p}d_j)^2} {\hat{\phi}\hat{p}(1 - \hat{p})d_j}\$\$,
which is compared with a chi-squared distribution with one degree of
freedom.

When `x` contains an `mv_cases` column, it is used as the exact
numerator \\y\\. Otherwise, MV case counts are reconstructed as
`round(fbs * mv / 100)` from the percentage returned by
[`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md).
The `mvs` column is a different quality indicator and is not used in
this test.

A status other than `"ok"` indicates that the row could not be
evaluated. Possible values are `"zero_target_cases"`,
`"insufficient_reference_registries"`, `"zero_reference_cases"`,
`"boundary_reference_proportion"`, and `"zero_reference_dispersion"`. An
`unusual` result indicates a statistical difference from the comparison
registries; it does not by itself identify the reason for that
difference.

## References

Bray, F., & Parkin, D. M. (2009). Evaluation of data quality in the
cancer registry: Principles and methods. Part I: Comparability, validity
and timeliness. *European Journal of Cancer*, 45(5), 747-755.
[doi:10.1016/j.ejca.2008.11.032](https://doi.org/10.1016/j.ejca.2008.11.032)

## Examples

``` r
quality_data <- structure(
  data.frame(
    regicode = c("A", "B", "C", "D"),
    year = "2024",
    sex = "1",
    cancer = "101",
    fbs = c(100, 100, 150, 200),
    mv = c(95, 70, 80, 75)
  ),
  class = c("quality", "data.frame")
)
compare_mv(quality_data)
#> # A tibble: 4 × 15
#>   regicode year  sex   cancer   fbs    mv mv_cases reference_n     p   phi
#>   <chr>    <chr> <chr> <chr>  <dbl> <dbl>    <dbl>       <int> <dbl> <dbl>
#> 1 A        2024  1     101      100    95       95           3 0.756  1.65
#> 2 B        2024  1     101      100    70       70           3 0.811  8.79
#> 3 C        2024  1     101      150    80      120           3 0.788 11.0 
#> 4 D        2024  1     101      200    75      150           3 0.814 10.5 
#> # ℹ 5 more variables: z2 <dbl>, p_value <dbl>, critical_value <dbl>,
#> #   unusual <lgl>, status <chr>
```
