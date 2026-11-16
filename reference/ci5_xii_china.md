# CI5-XII China reference data for registry comparisons

A compact reference object built from the China registry records in
CI5-XII and CI5-XIId. The `asr` component is the pooled China incidence
reference with one row per `sex` and CI5-XII `cancer` category. It
contains crude rates and age-standardized rates for the World (1985) and
China (2000) standard populations, together with variances on the
displayed per-100,000 rate scale. `asr_registry` keeps the corresponding
registry-specific rates for audit and exploratory use.

## Usage

``` r
ci5_xii_china
```

## Format

A named list with components `registry`, `asr`, `asr_registry`, `mv`,
`mi`, `availability`, and `metadata`.

## Source

International Agency for Research on Cancer, Cancer Incidence in Five
Continents, Volume XII and Volume XII detailed data. Source files are
maintained in the easyCI5 project and are not copied into this package.

## Details

CI5-XII/XIId provide incident cases and population at risk only. They do
not provide death counts, diagnosis basis, or exact microscopically
verified case counts. Consequently, `mv` and `mi` are typed empty tables
and `availability` records why those two reference standards are
unavailable. They must not be passed to
[`compare_mv()`](https://gigu003.github.io/canregtools/reference/compare_mv.md)
or
[`compare_mi()`](https://gigu003.github.io/canregtools/reference/compare_mi.md)
until an appropriate external source is joined to them.

## Examples

``` r
data("ci5_xii_china")
head(ci5_xii_china$asr)
#>   cancer sex no_cases       pop        cr       cr_var asr_wld85 asr_var_wld85
#> 1      1   0     1326 805935597 0.1645293 2.041469e-05 0.1221237  1.193679e-05
#> 2      2   0     7292 805935597 0.9047869 1.122654e-04 0.6821883  6.623915e-05
#> 3      3   0     9152 805935597 1.1355746 1.409014e-04 0.8536993  8.382952e-05
#> 4      4   0     5645 805935597 0.7004282 8.690870e-05 0.5589780  6.003110e-05
#> 5      5   0     1346 805935597 0.1670109 2.072261e-05 0.1263565  1.243845e-05
#> 6      6   0     1871 805935597 0.2321525 2.880535e-05 0.1751282  1.715313e-05
#>   asr_cn2000 asr_var_cn2000            icd10
#> 1  0.1262139   1.256593e-05 C00             
#> 2  0.7128859   7.313443e-05 C01-02          
#> 3  0.8767725   8.793051e-05 C03-06          
#> 4  0.5974970   6.859700e-05 C07-08          
#> 5  0.1300873   1.306067e-05 C09             
#> 6  0.1793078   1.797362e-05 C10             
#>                                                                       cancer_label
#> 1 Lip                                                                             
#> 2 Tongue                                                                          
#> 3 Mouth                                                                           
#> 4 Salivary glands                                                                 
#> 5 Tonsil                                                                          
#> 6 Other oropharynx                                                                
```
