# Test whether a validation result contains no errors

Test whether a validation result contains no errors

## Usage

``` r
is_valid_canreg(x)
```

## Arguments

- x:

  A `canreg` or `canregs` object, or a `canreg_validation` result.

## Value

A single logical value. Warnings do not make a result invalid.

## Examples

``` r
data("canregs")
is_valid_canreg(canregs[[1]])
#> [1] FALSE
```
