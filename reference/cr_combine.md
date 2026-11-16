# Combine cancer registry collections

`cr_combine()` combines multiple `canregs` or multiple `fbswicds`
objects by registry name. Collections cannot be mixed in one call. For
each registry, corresponding data-frame components are appended by row.
Optional components available in only some inputs are appended from
those inputs. Registries that occur in only some inputs are retained.

## Usage

``` r
cr_combine(object)
```

## Arguments

- object:

  A non-empty list containing only `canregs` objects or only `fbswicds`
  objects. Every collection must be named, and each name must match the
  `regicode` stored in its corresponding element.

## Value

A `canregs` or `fbswicds` object, matching the input collection class,
with one combined element per unique registry name.

## Examples

``` r
data("canregs")
combined <- cr_combine(list(canregs, canregs))
names(combined)
#> [1] "410103" "410307" "411721"
fbsws <- count_canreg(canregs)
combined_fbsw <- cr_combine(list(fbsws, fbsws))
```
