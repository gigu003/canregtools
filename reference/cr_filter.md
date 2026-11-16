# Filter rows from canregtools objects

Filter rows from canregtools objects

## Usage

``` r
cr_filter(.data, ..., drop = c("none"), part = "all")

# S3 method for class 'canregs'
cr_filter(.data, ..., drop = c("none"), part = "all")

# S3 method for class 'canreg'
cr_filter(.data, ..., drop = c("none"), part = "all")

# S3 method for class 'fbswicds'
cr_filter(.data, ..., drop = c("none"), part = "all")

# S3 method for class 'fbswicd'
cr_filter(.data, ..., drop = c("none"), part = "all")

# S3 method for class 'asrs'
cr_filter(.data, ..., drop = c("none"))

# S3 method for class 'age_rates'
cr_filter(.data, ..., drop = c("none"))

# S3 method for class 'qualities'
cr_filter(.data, ..., drop = c("none"))

# Default S3 method
cr_filter(.data, ..., drop = c("none"))
```

## Arguments

- .data:

  An object with class of `canreg`, `canregs`, `fbswicd`, `fbswicds`, or
  a data frame-like object.

- ...:

  Filtering conditions passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).
  These conditions are applied to selected components when all variables
  in the expressions are available in that component. Direct comparisons
  of `year` with numeric values (including `%in%`) temporarily interpret
  single-year labels as integers. Character comparisons retain character
  semantics. Period or invalid labels become missing with a warning in
  numeric comparisons. The stored `year` column is not changed.

- drop:

  Drop specific cancer categories. Use `"total"` to remove total cancer
  categories and `"others"` to remove other/non-specific cancer
  categories when a selected component has a `cancer` column.

- part:

  A character vector specifying which components of `.data` to filter.
  For `canreg` objects, use one or more of `"FBcases"`, `"SWcases"`, or
  `"POP"`. For `fbswicd` objects, use one or more of `"fbswicd"`,
  `"sitemorp"`, `"pop"`, or `"exclude"`. Use `"all"` to filter all
  available components.

## Value

An object of the same class as `.data` (`canreg` or `canregs`) with the
specified components filtered accordingly. The structure and component
order are preserved.
