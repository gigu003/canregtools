# Calculate the cumulative incidence or mortality rate

Computes the cumulative rate up to a specified age limit, typically used
in cancer epidemiology to estimate the probability of developing or
dying from a disease over a lifetime or up to a target age.

## Usage

``` r
cumrate(
  count,
  pop,
  rate = NULL,
  eage = 70,
  agewidth = 5,
  sep_zero = TRUE,
  mp = 1,
  decimal = 6
)
```

## Arguments

- count:

  Numeric vector, number of incident cases or deaths in each age group.

- pop:

  Numeric vector, corresponding population at risk for each age group.

- rate:

  Numeric vector, age-specific incidence or mortality rates. If not
  supplied, it will be calculated as `count / pop`.

- eage:

  Integer, the upper age limit (e.g., 70) up to which the cumulative
  rate is calculated. The upper bound is exclusive: `eage = 70` includes
  ages below 70, excluding the 70–74 age group. A bound within an age
  interval uses the corresponding fraction of that interval.

- agewidth:

  Integer, width of the age intervals (e.g., 5 for 5-year bands).

- sep_zero:

  Logical, whether the 0–1 age group is separated (i.e., age groups are
  0, 1–4, 5–9, ...). Default is `TRUE`.

- mp:

  Numeric. A multiplier used to scale the final cumulative rate (e.g.,
  100,000 or 1). Default is `1`.

- decimal:

  Integer, number of decimal places to round the result. Default is `6`.

## Value

A named numeric value representing the cumulative rate, scaled by `mp`.

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
mx <- dx / px
cumrate(mx, eage = 70)
#> Cumulative Rate(1/1) 
#>             0.295111 
```
