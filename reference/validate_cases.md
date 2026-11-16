# Validate cancer registry data

These functions inspect cancer case, population, and complete registry
objects without stopping at the first problem. They return a structured
table in which each row describes one issue and, where possible,
identifies the source row and variable.

## Usage

``` r
validate_cases(x, type = c("incidence", "mortality"))

validate_population(x)

validate_canreg(x)

validate_fbswicd(x)
```

## Arguments

- x:

  A case data frame, population data frame, or object of class `canreg`,
  `canregs`, `fbswicd`, or `fbswicds`.

- type:

  Type of case data passed to `validate_cases()`: `"incidence"` or
  `"mortality"`.

## Value

A tibble of class `canreg_validation` with columns `registry`,
`component`, `row`, `variable`, `severity`, `rule`, and `message`. An
empty result means that no issues were detected by the implemented
rules.

## Details

`validate_cases()` validates an incidence or mortality data frame.
`validate_population()` validates a population data frame.
`validate_canreg()` validates a `canreg` object or every registry in a
`canregs` object. `validate_fbswicd()` validates a summary-level
`fbswicd` object or every registry in a `fbswicds` object, including
checks between its case-summary and population components.

## See also

[`as_canreg()`](https://gigu003.github.io/canregtools/reference/as_canreg.md),
[`as_fbswicd()`](https://gigu003.github.io/canregtools/reference/as_fbswicd.md),
[`read_canreg()`](https://gigu003.github.io/canregtools/reference/read_canreg.md)

## Examples

``` r
data("canregs")
validation <- validate_canreg(canregs[[1]])
validation
#> Cancer registry validation
#> - Errors: 6
#> - Warnings: 4
#>  registry component  row        variable severity        rule
#>    410103   FBcases  996 inciden/deathda    error  date_order
#>    410103   FBcases 1197 inciden/deathda    error  date_order
#>    410103   FBcases 1853 inciden/deathda    error  date_order
#>    410103   FBcases 1952 inciden/deathda    error  date_order
#>    410103   FBcases 2035 inciden/deathda    error  date_order
#>    410103   FBcases 2045 inciden/deathda    error  date_order
#>    410103   FBcases 1381           icd10  warning code_format
#>    410103   FBcases 1900           icd10  warning code_format
#>    410103   FBcases 2293           icd10  warning code_format
#>    410103   SWcases 1024           icd10  warning code_format
#>                                         message
#>   `deathda` must not be earlier than `inciden`.
#>   `deathda` must not be earlier than `inciden`.
#>   `deathda` must not be earlier than `inciden`.
#>   `deathda` must not be earlier than `inciden`.
#>   `deathda` must not be earlier than `inciden`.
#>   `deathda` must not be earlier than `inciden`.
#>  ICD-10 values should use a C00-D48 style code.
#>  ICD-10 values should use a C00-D48 style code.
#>  ICD-10 values should use a C00-D48 style code.
#>  ICD-10 values should use a C00-D48 style code.
is_valid_canreg(canregs[[1]])
#> [1] FALSE
```
