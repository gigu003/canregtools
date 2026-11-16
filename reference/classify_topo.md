# Classify ICD-O-3 topography codes into categories

Classify ICD-O-3 topography codes into categories

## Usage

``` r
classify_topo(x, cancer_type = "big")
```

## Arguments

- x:

  Character vector of ICD-O-3 topography codes. Values not present in
  the internal dictionary will be returned as `NA`.

- cancer_type:

  Cancer type.

## Value

Character vector of categories corresponding to `x`.
