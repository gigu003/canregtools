# Blindly benchmark age-splitting methods against a single-age schedule

Aggregate an independent single-age population schedule to
`source_breaks`, restore it with each method, and compare the restored
schedule with the held-out source. This provides a reproducible
validation workflow for an official single-age population series without
allowing the target series to be used as a `"reference"` schedule.

## Usage

``` r
benchmark_age_split(
  population,
  source_breaks = c(0, 1, seq(5, 85, 5)),
  methods = c("linear", "sprague"),
  reference = NULL,
  cases = NULL,
  stdpop = NULL,
  mp = 1e+05,
  high_age = "keep",
  pclm_control = list()
)
```

## Arguments

- population:

  Held-out single-age population schedule. Supply a named numeric vector
  or a data frame accepted by
  [`expand_age_pop()`](https://gigu003.github.io/canregtools/reference/expand_age_pop.md).
  Ages at or above the final `source_breaks` value are treated as the
  open group.

- source_breaks:

  Lower bounds used to blind the held-out schedule.

- methods:

  Character vector of methods passed to
  [`expand_age_pop()`](https://gigu003.github.io/canregtools/reference/expand_age_pop.md).

- reference:

  Independent reference schedule used only by the `"reference"` method.

- cases:

  Optional held-out single-age case schedule. If supplied,
  age-standardized-rate bias is calculated using the restored
  population.

- stdpop:

  Optional single-age standard-population weights for ASR. Equal weights
  are used when `cases` is supplied without `stdpop`.

- mp:

  Multiplier for the ASR, defaulting to 100,000.

- high_age:

  High-age configuration passed to
  [`expand_age_pop()`](https://gigu003.github.io/canregtools/reference/expand_age_pop.md).

- pclm_control:

  PCLM control settings passed to
  [`expand_age_pop()`](https://gigu003.github.io/canregtools/reference/expand_age_pop.md).

## Value

A data frame with MAE, RMSE, total variation distance, ASR bias, maximum
source-group error, number of negative values, and available
uncertainty-interval diagnostics. The restored schedules are stored in
the `estimates` attribute; the error for every effective source group is
stored in the `group_errors` attribute.
