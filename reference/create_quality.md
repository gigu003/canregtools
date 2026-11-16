# Calculate quality indicators

`create_quality()`calculate quality indicators from object with class of
`canreg`, `canregs`, `fbswicd`, or `fbswicds`. The quality indicators
for population-based cancer registries (PBCRs) including:

- `fbs`: Number of incident cases.

- `inci`: Cancer incidence rate.

- `sws`: Number of death cases.

- `mort`: Mortality rate.

- `mv`: Percentage of cases with microscopic verification.

- `mi`: Mortality-to-incidence ratio.

- And other relevant quality metrics for cancer data evaluation.

## Usage

``` r
create_quality(x, ..., decimal = 2, collapse = TRUE)

# S3 method for class 'canreg'
create_quality(x, ..., cancer_type = "big")

# S3 method for class 'canregs'
create_quality(x, ..., cancer_type = "big", collapse = TRUE)

# S3 method for class 'fbswicds'
create_quality(x, ..., decimal = 2, collapse = TRUE)

# S3 method for class 'fbswicd'
create_quality(x, ..., decimal = 2)
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

- decimal:

  The number of decimal places to include in the resulting quality
  indicator values. Defaults to 2.

- collapse:

  Logical value whether output result as quality or qualites.

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

A data frame (if applied to a single registry object, 'canreg' or
'fbswicd') or a list of data frames (if applied to a grouped registry
object, 'canregs' or 'fbswicds') with a class of either `'quality'` or
`'qualities'`.

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
fbsws <- count_canreg(canregs, cancer_type = "system")
qua2 <- create_quality(fbsws, year, sex, cancer)
head(qua2)
#> # A tibble: 6 × 15
#>   regicode year  sex   cancer    rks   fbs  inci   sws  mort    mi    mv   dco
#>   <chr>    <chr> <chr> <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 410103   2021  0     201    687356    43  6.26    22  3.2   0.51  69.8  2.33
#> 2 410103   2021  0     202    687356   665 96.8    433 63     0.65  69.8  1.35
#> 3 410103   2021  0     203    687356   534 77.7    289 42.0   0.54  69.7  0.75
#> 4 410103   2021  0     204    687356    41  5.96    15  2.18  0.37  75.6  0   
#> 5 410103   2021  0     205    687356   244 35.5     53  7.71  0.22  83.2  1.64
#> 6 410103   2021  0     206    358941   142 39.6     38 10.6   0.27  86.6  0   
#> # ℹ 3 more variables: ub <dbl>, sub <dbl>, mvs <dbl>

# Calculate the quality indicators based on object with class of `canreg`
data <- canregs[[1]]
qua <- create_quality(data, year, sex, cancer, cancer_type = "big")
head(qua)
#> # A tibble: 6 × 14
#>   year  sex   cancer    rks   fbs  inci   sws  mort    mi    mv   dco    ub
#>   <chr> <chr> <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 2021  0     101    687356    38  5.53    20  2.91  0.53  73.7  2.63     0
#> 2 2021  0     102    687356     5  0.73     2  0.29  0.4   40    0        0
#> 3 2021  0     103    687356    59  8.58    58  8.44  0.98  74.6  0        0
#> 4 2021  0     104    687356   123 17.9     82 11.9   0.67  71.5  1.63     0
#> 5 2021  0     105    687356   236 34.3    103 15.0   0.44  77.5  0.85     0
#> 6 2021  0     106    687356   128 18.6    101 14.7   0.79  54.7  1.56     0
#> # ℹ 2 more variables: sub <dbl>, mvs <dbl>

# Calculate the quality indicators based on object with class of `canregs`
qua <- create_quality(canregs, year, sex, cancer, cancer_type = "big")
head(qua)
#> # A tibble: 6 × 15
#>   regicode year  sex   cancer    rks   fbs  inci   sws  mort    mi    mv   dco
#>   <chr>    <chr> <chr> <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 410103   2021  0     101    687356    38  5.53    20  2.91  0.53  73.7  2.63
#> 2 410103   2021  0     102    687356     5  0.73     2  0.29  0.4   40    0   
#> 3 410103   2021  0     103    687356    59  8.58    58  8.44  0.98  74.6  0   
#> 4 410103   2021  0     104    687356   123 17.9     82 11.9   0.67  71.5  1.63
#> 5 410103   2021  0     105    687356   236 34.3    103 15.0   0.44  77.5  0.85
#> 6 410103   2021  0     106    687356   128 18.6    101 14.7   0.79  54.7  1.56
#> # ℹ 3 more variables: ub <dbl>, sub <dbl>, mvs <dbl>

# Calculate the quality indicators based on object with class of `fbswicds`
fbsws <- count_canreg(canregs, cancer_type = "small")
qua <- create_quality(fbsws, year, sex, cancer)
head(qua)
#> # A tibble: 6 × 15
#>   regicode year  sex   cancer    rks   fbs  inci   sws  mort    mi    mv   dco
#>   <chr>    <chr> <chr> <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 410103   2021  0     1      687356     0  0        1  0.15 NA     NA   NA   
#> 2 410103   2021  0     10     687356    59  8.58    58  8.44  0.98  74.6  0   
#> 3 410103   2021  0     11     687356   123 17.9     82 11.9   0.67  71.5  1.63
#> 4 410103   2021  0     12     687356    13  1.89     6  0.87  0.46  84.6  0   
#> 5 410103   2021  0     13     687356   129 18.8     63  9.17  0.49  77.5  0.78
#> 6 410103   2021  0     14     687356   104 15.1     39  5.67  0.38  76.9  0.96
#> # ℹ 3 more variables: ub <dbl>, sub <dbl>, mvs <dbl>

# Calculate the quality indicators based on object with class of `fbswicd`
fbsw <- count_canreg(canregs[[1]], cancer_type = "big")
qua <- create_quality(fbsw, year, sex, cancer)
head(qua)
#> # A tibble: 6 × 14
#>   year  sex   cancer    rks   fbs  inci   sws  mort    mi    mv   dco    ub
#>   <chr> <chr> <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 2021  0     101    687356    38  5.53    20  2.91  0.53  73.7  2.63     0
#> 2 2021  0     102    687356     5  0.73     2  0.29  0.4   40    0        0
#> 3 2021  0     103    687356    59  8.58    58  8.44  0.98  74.6  0        0
#> 4 2021  0     104    687356   123 17.9     82 11.9   0.67  71.5  1.63     0
#> 5 2021  0     105    687356   236 34.3    103 15.0   0.44  77.5  0.85     0
#> 6 2021  0     106    687356   128 18.6    101 14.7   0.79  54.7  1.56     0
#> # ℹ 2 more variables: sub <dbl>, mvs <dbl>
```
