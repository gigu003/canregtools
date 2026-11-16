# list dictionary used in package

list dictionary used in package

## Usage

``` r
ls_dict(dict = "registry")
```

## Arguments

- dict:

  Character, name of dictionary.

## Value

The effective dictionary including defaults and user overrides. Mapping
dictionaries have `regicode` and `value` columns; `regicode` labels have
`regicode`, `cname`, `ename`, `abbr_cn`, and `abbr_en`.

## Examples

``` r
ls_dict("registry")
#> # A tibble: 27 × 2
#>    regicode value 
#>    <chr>    <chr> 
#>  1 410102   410101
#>  2 410103   410101
#>  3 410104   410101
#>  4 410105   410101
#>  5 410302   410301
#>  6 410303   410301
#>  7 410304   410301
#>  8 410305   410301
#>  9 410307   410301
#> 10 410402   410401
#> # ℹ 17 more rows
ls_dict("area_type")
#> # A tibble: 3 × 2
#>   regicode value 
#>   <chr>    <chr> 
#> 1 410307   920000
#> 2 411222   920000
#> 3 410308   920000
```
