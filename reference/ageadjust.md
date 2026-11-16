# Calculate the age standardized rate using the direct method

This function calculates the **age-standardized rate (ASR)** using the
**direct method** of standardization. It allows for adjustment of crude
disease or mortality rates to a standard population structure, enabling
valid comparisons across populations with different age distributions.
The function also supports computation of confidence intervals using
gamma, normal, or log-normal methods, and returns both crude rate (CR)
and ASR with associated variances and interval estimates.

## Usage

``` r
ageadjust(
  count,
  pop,
  rate = NULL,
  stdpop = NULL,
  method = "gamma",
  conf_level = 0.95,
  mp = 1e+05
)
```

## Arguments

- count:

  The number of cases of a specific disease or condition.

- pop:

  The total population of the same group or region where the disease
  cases (count) were observed.

- rate:

  Disease rate, which is the number of cases (count) per unit of
  population (pop).

- stdpop:

  Standardized population for age standardization.

- method:

  Method used for calculating the age-standardized rate, options are
  'gamma', 'normal', or 'lognormal', default is 'gamma'.

- conf_level:

  Confidence level for calculating confidence intervals, value between 0
  and 1, default is 0.95.

- mp:

  A multiplier used to scale the calculated rates. Default is 100000.

## Value

Age standardized rate and its confidence interval.

## Details

Inputs must have equal, non-zero lengths. Counts and standard weights
must be finite and non-negative; population must be strictly positive.
Standard weights must have a positive total. Zero-event rates use Gamma
bounds (zero lower bound) for all interval methods because the normal
and log-normal approximations are unsuitable at zero. Variances remain
on the unmultiplied rate scale.

## Examples

``` r
cases <- c(50, 60, 45, 70)
pop <- c(1000, 1200, 1100, 900)
spop <- c(800, 1000, 1100, 900)
ageadjust(cases, pop, stdpop = spop, mp = 100000)
#> $cases
#> [1] 225
#> 
#> $cr
#> [1] 5357.143
#> 
#> $cr_var
#> [1] 1.27551e-05
#> 
#> $cr_lci
#> [1] 4679.972
#> 
#> $cr_uci
#> [1] 6104.765
#> 
#> $asr
#> [1] 5394.737
#> 
#> $asr_var
#> [1] 1.306556e-05
#> 
#> $asr_lci
#> [1] 4709.493
#> 
#> $asr_uci
#> [1] 6154.046
#> 
```
