# Count ICD-10 site and morphology groups by strata

`create_site_morp()` counts ICD-10 site codes or morphology groups
within user-defined strata. It can be applied to objects with class
`canreg`, `canregs`, `fbswicd`, or `fbswicds`.

## Usage

``` r
create_site_morp(
  x,
  ...,
  type = c("icd10", "morp", "both"),
  flatten = TRUE,
  collapse = TRUE,
  wrap_subsite = FALSE,
  class_morp = TRUE,
  drop_nos = TRUE,
  cancer_type = "big"
)

# S3 method for class 'canreg'
create_site_morp(
  x,
  ...,
  type = c("icd10", "morp", "both"),
  flatten = TRUE,
  wrap_subsite = FALSE,
  class_morp = TRUE,
  drop_nos = TRUE,
  cancer_type = "big"
)

# S3 method for class 'canregs'
create_site_morp(
  x,
  ...,
  type = c("icd10", "morp", "both"),
  flatten = TRUE,
  collapse = TRUE,
  wrap_subsite = FALSE,
  class_morp = TRUE,
  drop_nos = TRUE,
  cancer_type = "big"
)

# S3 method for class 'fbswicds'
create_site_morp(
  x,
  ...,
  type = c("icd10", "morp", "both"),
  flatten = TRUE,
  collapse = TRUE,
  wrap_subsite = FALSE,
  class_morp = TRUE,
  drop_nos = TRUE
)

# S3 method for class 'fbswicd'
create_site_morp(
  x,
  ...,
  type = c("icd10", "morp", "both"),
  flatten = TRUE,
  wrap_subsite = FALSE,
  class_morp = TRUE,
  drop_nos = TRUE
)
```

## Arguments

- x:

  An object with class `canreg`, `canregs`, `fbswicd`, or `fbswicds`.

- ...:

  Variables used to define strata. For example, `year`, `sex`, and
  `cancer` will return frequencies within each combination of year, sex,
  and cancer group.

- type:

  Character. Type of frequency table to return when `flatten = TRUE`.
  Options are `"icd10"`, `"morp"`, and `"both"`. `"icd10"` returns
  ICD-10 site-code frequencies and is the default; `"morp"` returns
  morphology-code or morphology-class frequencies; `"both"` returns both
  results as a named list with elements `icd10` and `morp`.

- flatten:

  Logical. If `TRUE`, flatten the `icd10` or `morp` list-column
  frequency tables in a single `fbswicd` object into ordinary data
  frames with one row per stratum-code combination. If `FALSE`, keep the
  intermediate list-column structure with one row per stratum and two
  list-columns, `icd10` and `morp`.

- collapse:

  Logical. Only used for `canregs` and `fbswicds`. If `TRUE`, merge
  multiple registry objects before creating site/morphology frequency
  tables. If `FALSE`, return one result per registry object as a list.

- wrap_subsite:

  Logical. If `TRUE`, collapse ICD-10 subsite codes into two broad
  groups: codes ending with `.9` are coded as `"CXX.9"`; all other
  subsite codes are coded as `"CXX.0"`. Counts are then summed within
  these broad groups.

- class_morp:

  Logical. If `TRUE`, classify morphology codes with
  [`classify_morp()`](https://gigu003.github.io/canregtools/reference/classify_morp.md)
  before summing counts. If `FALSE`, counts are returned by the original
  morphology code. When `cancer` is included in the stratification
  variables, its value is used as the `group` argument passed to
  [`classify_morp()`](https://gigu003.github.io/canregtools/reference/classify_morp.md),
  currently `"60"` or `"125"`.

- drop_nos:

  Logical. Reserved for future use. Currently this argument is accepted
  but not used by the function.

- cancer_type:

  Character. Cancer classification type passed to
  [`count_canreg()`](https://gigu003.github.io/canregtools/reference/count_canreg.md)
  when `x` is a `canreg` or `canregs` object.

## Value

The return value depends on input class, `flatten`, `collapse`, and
`type`:

- For a single `fbswicd` or `canreg` object with `flatten = TRUE` and
  `type = "icd10"`, returns a data frame with the stratification
  variables, `icd10`, and `count`.

- With `flatten = TRUE` and `type = "morp"`, returns a data frame with
  the stratification variables, `morp`, and `count`.

- With `flatten = TRUE` and `type = "both"`, returns a named list
  containing `icd10` and `morp` data frames.

- With `flatten = FALSE`, returns a data frame with the stratification
  variables, `icd10`, and `morp`, where `icd10` and `morp` are
  list-columns of frequency tables.

- For `canregs` or `fbswicds`, `collapse = TRUE` returns a merged
  result; `collapse = FALSE` returns one result per registry object as a
  list.

## Details

For `canreg` and `canregs` objects, the function first calls
[`count_canreg()`](https://gigu003.github.io/canregtools/reference/count_canreg.md)
to create `fbswicd`/`fbswicds` objects, and then counts the `icd10`
and/or `morp` frequency tables stored in the `sitemorp` element.

For `fbswicd` and `fbswicds` objects, a `sitemorp` component must be
present. If it was omitted when constructing the object, the function
reports that site/morphology data are unavailable. An explicitly
supplied zero-row component is treated as available and returns an empty
result.

## Examples

``` r
if (FALSE) { # \dontrun{
data("canregs")

create_site_morp(canregs, year, sex, cancer)
create_site_morp(canregs, year, sex, cancer, type = "morp")
create_site_morp(canregs, year, sex, cancer, type = "both")
create_site_morp(canregs, year, sex, cancer, collapse = FALSE)

fbsw <- count_canreg(canregs[[1]])
create_site_morp(fbsw, year, sex, cancer, flatten = FALSE)
} # }
```
