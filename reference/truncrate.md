# Calculate truncated age standardized rate

Calculates the **truncated age-standardized rate** (ASR) over a
specified age range (e.g., 35–64 years). It uses the **direct method of
standardization** by applying age-specific rates to a standard
population within the truncated age group. This is particularly useful
when comparing disease rates in middle-aged or other focused subgroups.

## Usage

``` r
truncrate(
  cases,
  pop,
  stdpop = NULL,
  trunc_age = c(35, 64),
  agewidth = 5,
  sep_zero = TRUE,
  mp = 100,
  decimal = 2
)
```

## Arguments

- cases:

  Number of cases.

- pop:

  Number of population at risk.

- stdpop:

  The standard population.

- trunc_age:

  Inclusive truncated age range. Both ends must align with complete age
  groups; the group starting after the upper end is excluded.

- agewidth:

  Age groups width, default is 5.

- sep_zero:

  Logical value, if the 0 age group was a separate group.

- mp:

  A multiplier used to scale the calculated rates. Default is 100.

- decimal:

  Decimals of the calculated rates, default is 2.

## Value

Truncated age standardized rate.

## Examples

``` r
px <- c(
  20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
  163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
)
dx <- c(
  156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
  891, 831, 926, 731, 269
)
stdpop <- c(2.4, 9.6, 10, 9, 9, 8, 8, 6, 6, 6, 6, 5, 4, 4, 3, 2, 1, 0.5, 0.5)
truncrate(dx, px, stdpop, trunc_age = c(35, 64))
#> [1] 0.46
```
