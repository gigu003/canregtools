# Subset or summarize population data

Extracts population data from canreg-style objects and optionally
summarizes it by specified grouping variables.

## Usage

``` r
get_pop(data, sum_by = NULL, collapse = FALSE)

# S3 method for class 'canreg'
get_pop(data, sum_by = NULL, collapse = FALSE)

# S3 method for class 'canregs'
get_pop(data, sum_by = NULL, collapse = FALSE)

# S3 method for class 'fbswicd'
get_pop(data, sum_by = NULL, collapse = FALSE)

# S3 method for class 'fbswicds'
get_pop(data, sum_by = NULL, collapse = FALSE)
```

## Arguments

- data:

  An object of class `canreg`, `canregs`, `fbswicd`, or `fbswicds`.

- sum_by:

  Character vector of grouping variables to summarize population.

- collapse:

  Logical, if `TRUE`, collapses the list of results into a single data
  frame (only for list-type objects).

## Value

A data frame or a list of data frames depending on `collapse`.
