# Compare mortality-to-incidence ratios across registries

`compare_mi()` applies the statistical test described by Parkin and Bray
(2009) to identify registry mortality-to-incidence (M:I) ratios that
differ from comparison registries. Each registry is tested in turn,
using all other registries in the same stratum as its comparison
population.

## Usage

``` r
compare_mi(x, alpha = 0.05, reference = NULL)
```

## Arguments

- x:

  An object with class `quality` or `qualities` containing the registry
  identifier `regicode`, incident case counts `fbs`, and death counts
  `sws`.

- alpha:

  A single numeric significance level strictly between 0 and 1. Defaults
  to `0.05`, corresponding to a chi-squared critical value of
  approximately 3.84 with one degree of freedom.

- reference:

  An optional external reference table. It must contain regicode, fbs,
  and sws; registry and cases are accepted as aliases for regicode and
  fbs. At least two reference rows are needed in each matched stratum to
  estimate dispersion.

## Value

A tibble containing the stratum identifiers, `regicode`, `fbs`, `sws`,
the observed `mi`, the number of comparison registries `reference_n`,
the estimated common ratio `theta`, the dispersion estimate `phi`, the
test statistic `z2`, `p_value`, `critical_value`, the logical flag
`unusual`, and an evaluation `status`.

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
\\i = 1, \ldots, n\\, let \\d_i\\ be the number of incident cases,
\\m_i\\ the number of deaths, and \\n_i = m_i + d_i\\. The common M:I
ratio and dispersion are estimated as \$\$\hat{\theta} = \frac{\sum_i
m_i}{\sum_i d_i}\$\$ and \$\$\hat{\phi} = \frac{1}{n - 1} \sum_i
\frac{(m_i - \hat{\theta}d_i)^2}{n_i\hat{\theta}}.\$\$ For the registry
under test, indexed by \\j\\, the statistic is \$\$Z^2 = \frac{(m_j -
\hat{\theta}d_j)^2} {\hat{\phi}n_j\hat{\theta}}\$\$, which is compared
with a chi-squared distribution with one degree of freedom.

A status other than `"ok"` indicates that the row could not be
evaluated. Possible values are `"zero_target_incidence"`,
`"insufficient_reference_registries"`, `"zero_reference_incidence"`,
`"zero_reference_mi"`, and `"zero_reference_dispersion"`. An `unusual`
result indicates a statistical difference from the comparison
registries; it does not by itself establish incomplete registration.

## References

Parkin, D. M., & Bray, F. (2009). Evaluation of data quality in the
cancer registry: Principles and methods Part II. Completeness. *European
Journal of Cancer*, 45(5), 756-764.
[doi:10.1016/j.ejca.2008.11.033](https://doi.org/10.1016/j.ejca.2008.11.033)

## Examples

``` r
quality_data <- structure(
  data.frame(
    regicode = c("A", "B", "C", "D"),
    year = "2024",
    sex = "1",
    cancer = "101",
    fbs = c(100, 100, 150, 200),
    sws = c(80, 40, 75, 80)
  ),
  class = c("quality", "data.frame")
)
compare_mi(quality_data)
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
