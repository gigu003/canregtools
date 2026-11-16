# Classify codes for the administrative divisions of China

Categorizes six-digit administrative division codes of the People's
Republic of China (as per GB/T 2260-2007) into several structured
components, including province, city, area type (urban/rural), registry
code, and regional classification.

## Usage

``` r
classify_areacode(x, attr = regicode_vars())
```

## Arguments

- x:

  A vector of six-digit Chinese administrative area codes, either as
  numeric or character strings.

- attr:

  A character vector of attributes to return. Defaults to all built-in
  area-code attributes. Options include:

  - `"regicode"`: Original area codes.

  - `"registry"`: Cancer registry codes.

  - `"province"`: Province-level codes.

  - `"city"`: City-level codes.

  - `"area_type"`: Urban/rural classification.

  - `"region"`: Regional classification based on province.

  - Any other string will be treated as a custom dictionary name written
    using
    [`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md).

## Value

A list with the following named elements:

- `regicode`: Validated area codes. Invalid entries are replaced with
  `NA`.

- `registry`: Registry codes corresponding to each area, using a
  built-in or cached dictionary.

- `province`: Province-level codes formed by taking the first two digits
  and appending `"0000"`.

- `city`: City-level codes formed by taking the first four digits and
  appending `"00"`.

- `area_type`: Urban-rural classification codes: `"910000"` for urban,
  `"920000"` for rural. This can be updated using
  [`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md)
  with `attr = "area_type"`.

- `region`: Region classification codes derived from province codes,
  ending in `"0000"`.

## Details

This function standardizes and validates area codes, identifies their
administrative levels, and attaches metadata used in cancer registration
systems. It also supports external dictionaries (from the `canregtools`
configuration folder) to provide more accurate classification of area
types and registry mapping. Classify Codes for the administrative
divisions of the People's Republic of China(GB/T 2260-2007) into
different categories, including 'province', city', 'area_type', and
'registry' attributes.

## See also

[`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md),
[`write_registry()`](https://gigu003.github.io/canregtools/reference/write_registry.md)

## Examples

``` r
classify_areacode(c("110000", "320500", "440300"))
#> $regicode
#> [1] "110000" "320500" "440300"
#> 
#> $registry
#> [1] "110000" "320500" "440300"
#> 
#> $province
#> [1] "110000" "320000" "440000"
#> 
#> $city
#> [1] "110000" "320500" "440300"
#> 
#> $area_type
#> [1] "910000" "910000" "910000"
#> 
#> $region
#> [1] "720000" "740000" "760000"
#> 
classify_areacode(c("110000", "320500", "440300"), attr = "registry")
#> $registry
#> [1] "110000" "320500" "440300"
#> 
```
