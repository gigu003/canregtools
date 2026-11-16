# Select elements from objects with class `"canregs"`, or `"fbswicds"`

This function allows you to select specific elements from objects of
class 'canregs', 'fbswicds', or 'asrs' based on provided indices,
logical conditions, or expressions. The selected elements are returned
while preserving the class of the input object.

## Usage

``` r
cr_select(data, ..., index = names(data))

# S3 method for class 'canregs'
cr_select(data, ..., index = names(data))

# S3 method for class 'asrs'
cr_select(data, ..., index = names(data))

# S3 method for class 'age_rates'
cr_select(data, ..., index = names(data))

# S3 method for class 'fbswicds'
cr_select(data, ..., index = names(data))

# S3 method for class 'summaries'
cr_select(data, ..., index = names(data))
```

## Arguments

- data:

  An object of class 'canregs', 'fbswicds', or 'asrs' from which
  elements will be selected.

- ...:

  Optional conditions or expressions used to filter elements within the
  list or data frame. Conditions are evaluated for each element of the
  input object. For `canregs` and `fbswicds`, population-year
  expressions can refer to `POP` or `pop`, respectively. The aliases
  `pop_years`, `pop_n_years`, `pop_year_min`, `pop_year_max`, and
  `pop_year_range` are also available. They mean: `pop_years` is the
  sorted unique non-missing population year labels; `pop_n_years` is the
  number of those years; `pop_year_min` and `pop_year_max` are the
  minimum and maximum calendar-year bounds; and `pop_year_range` is a
  length-two vector containing those bounds. Period labels such as
  `"2003-2007"` contribute 2003 and 2007 to the respective bounds. For
  example, use `all(pop_years %in% as.character(2018:2022))` to require
  all population years to fall within 2018–2022.

- index:

  A vector of indices specifying the elements to select. This can be a
  character vector (matching element names), a numeric vector
  (specifying positions), or a logical vector (indicating inclusion).

## Value

An object of the same class as the input object, containing only the
selected elements that meet the specified indices or conditions.

## Details

Each comma-separated condition must be satisfied by at least one
non-missing value within an element. The conditions may be satisfied by
different rows. To require conditions on the same row, combine them with
`&` in a single expression. Missing condition values do not count as
matches.

## Examples

``` r
data("canregs")
# Select elements which mi greather than 0.5 from `canregs`
canregs_mi <- cr_select(canregs, mi > 0.5)
# Select registries with population data for exactly three years
canregs_three_years <- cr_select(canregs, pop_n_years == 3)
# Select registries whose population years are all within 2018--2022
canregs_period <- cr_select(
  canregs,
  all(pop_years %in% as.character(2018:2022))
)
# The original population data can also be used directly
canregs_three_years <- cr_select(
  canregs,
  length(unique(POP$year)) == 3
)

# Select elements from obejct with class of `fbswicds`
fbsws <- count_canreg(canregs)
# Select elements which `inci` greater than 250 per 100000 population
fbsws_inci <- cr_select(fbsws, inci > 250)

# Select elements from object with class of `summaries`
summ <- summary(canregs, collapse = FALSE)
# Select elements for whcih `mi` greater than 0.5
summ_mi <- cr_select(summ, mi > 0.5)
names(summ_mi)
#> [1] "411721"
```
