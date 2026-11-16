# Classify ICD-O-3 morphology codes into categories

Classify ICD-O-3 morphology codes into categories

## Usage

``` r
classify_morp(x, group = c("60", "125"))
```

## Arguments

- x:

  Character vector of ICD-O-3 morphology codes. Values not present in
  the internal dictionary will be returned as `"99"`.

- group:

  Morphology grouping system. `"60"` uses the broad morphology grouping
  dictionary; `"125"` uses leukemia-specific morphology groups.

## Value

Character vector of categories corresponding to `x`.

## Examples

``` r
morps <- c("8140","8070","8050","8051","9900","9800","9993")
classify_morp(morps)
#> [1] "6"  "3"  "3"  "3"  "99" "52" "59"
classify_morp(c("9811", "9840", "9801"), group = "125")
#> [1] "201" "202" "204"
```
