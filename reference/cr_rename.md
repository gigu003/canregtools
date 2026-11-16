# Rename registries in a registry collection

`cr_rename()` renames selected elements of a `canregs` or `fbswicds`
object and updates the `regicode` stored inside each renamed registry
element.

## Usage

``` r
cr_rename(x, mapping)

# S3 method for class 'canregs'
cr_rename(x, mapping)

# S3 method for class 'fbswicds'
cr_rename(x, mapping)
```

## Arguments

- x:

  An object of class `canregs` or `fbswicds`.

- mapping:

  A named character vector. Names identify existing registry names in
  `x`, and values provide their new names.

## Value

An object with the same class as `x`, with selected element names and
their internal `regicode` values updated.

## Examples

``` r
data("canregs")
renamed <- cr_rename(
  canregs,
  c("410103" = "REG_A", "410307" = "REG_B")
)
names(renamed)
#> [1] "REG_A"  "REG_B"  "411721"
renamed[["REG_A"]]$regicode
#> [1] "REG_A"

summarized <- count_canreg(canregs)
renamed_summaries <- cr_rename(summarized, c("410103" = "REG_A"))
renamed_summaries[["REG_A"]]$regicode
#> [1] "REG_A"
```
