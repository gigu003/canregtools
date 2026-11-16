# Get standardized population data

Retrieves standardized population data for a specified standard from
dict_maps.

## Usage

``` r
get_stdpop(
  std = "wld85",
  sep_zero = NULL,
  age_breaks = NULL,
  method = c("exact", "interpolate"),
  interpolation_method = "linear"
)
```

## Arguments

- std:

  Character string specifying the population standard. Supported values
  are "cn64", "cn82", "cn2000", "wld85", "wld2000", "eur76", "eur2013",
  and "usa2000". Defaults to "wld85".

- sep_zero:

  Logical value indicating whether age 0 should be treated as a separate
  group. Retained for compatibility; it cannot be supplied together with
  `age_breaks`.

- age_breaks:

  Numeric vector of lower bounds for the requested age groups. It must
  begin at 0 and end no later than 85; the final group is open-ended.
  Defaults to the original 19 groups, or to 18 five-year groups when
  `sep_zero = FALSE`.

- method:

  Method for obtaining the requested grouping. With `"exact"` (the
  default), requested groups must only combine the original
  standard-population groups. `"interpolate"` expands the standard
  population to single-year ages before regrouping, and should be used
  only when a requested boundary splits an original group.

- interpolation_method:

  Interpolation method passed to
  [`expand_age_pop()`](https://gigu003.github.io/canregtools/reference/expand_age_pop.md)
  when `method = "interpolate"`. With `"sprague"`, requested age groups
  cannot split 0–4 because the source 0 and 1–4 groups are combined
  before interpolation.

## Value

A numeric vector containing the standardized population weights for the
requested age groups, or `NULL` if the standard is not supported.

## Details

`eur2013` follows the 2013 European Standard Population. Its source
separates ages 85–89, 90–94, and 95 years and over; these are combined
into the final `85+` group to match the 19-group structure used by this
package.

## Examples

``` r
if (FALSE) { # \dontrun{
  get_stdpop("cn64")
  get_stdpop("wld2000", age_breaks = c(0, 1, 15, 45, 65, 85))
} # }
```
