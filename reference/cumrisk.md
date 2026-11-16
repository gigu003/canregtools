# Calculate the cumulative risk

Converts a cumulative rate to a cumulative risk using the standard
exponential formula. This is commonly used in cancer epidemiology to
estimate the probability of developing or dying from cancer up to a
certain age, under the assumption of constant rates.

## Usage

``` r
cumrisk(cumrate, mp = 100, decimal = 2)
```

## Arguments

- cumrate:

  Numeric. The cumulative incidence or mortality rate, typically
  calculated using
  [`cumrate`](https://gigu003.github.io/canregtools/reference/cumrate.md).

- mp:

  Numeric. The rate multiplier used in `cumrate`. This is used for
  labeling purposes only. Default is `100`.

- decimal:

  Integer. Number of decimal places to round the result. Default is `2`.

## Value

A named numeric value representing the cumulative risk (as a
percentage).

## Details

The cumulative risk is calculated as: \$\$1 - \exp(-\text{cumrate})\$\$
This converts the cumulative rate to a probability, assuming the event
rate is constant over each age interval and the competing risks are
ignored.

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
cumrisk(cumrate(mx, eage = 70))
#> Cumulative Risk (1/100) 
#>                   25.56 
```
