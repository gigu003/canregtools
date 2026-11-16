# Compare incidence or mortality rates

Compare a registry rate with an external CI5-style reference rate using
the standard errors of the two rates. The test is performed on the
logarithm of the rate ratio, so the null hypothesis is that the rate
ratio equals one.

## Usage

``` r
compare_rate(
  x,
  event = c("inci", "mort"),
  standard = "wld85",
  reference = NULL,
  rate_col = NULL,
  se_col = NULL,
  variance_col = NULL,
  alpha = 0.05,
  multiplier = 1e+05,
  variance_scale = c("display", "unmultiplied")
)
```

## Arguments

- x:

  An asr data frame, an asrs list, or a data frame containing regicode,
  the selected rate column, and its standard-error or variance column.

- event:

  The event label to report: inci for incidence or mort for mortality.

- standard:

  Standard-population suffix used to find the rate columns. Defaults to
  wld85. Use cr to compare crude rates.

- reference:

  An external reference table with the same rate and uncertainty columns
  as x. It may omit regicode when it contains one row. Reference rows
  are matched using stratum columns shared with x.

- rate_col:

  Optional explicit rate column name. Defaults to asr_standard, or cr
  when standard is cr.

- se_col:

  Optional explicit standard-error column name. When neither
  uncertainty-column argument is supplied, existing variance columns are
  preferred, followed by `asr_se_standard` or `cr_se`.

- variance_col:

  Optional explicit variance column name. Used only when `se_col` is not
  supplied.

- alpha:

  Two-sided significance level. Defaults to 0.05.

- multiplier:

  Rate multiplier used only when `variance_scale = "unmultiplied"`.
  Defaults to `100000`.

- variance_scale:

  Scale of supplied variance columns. The default, `"display"`, matches
  current
  [`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)
  output. Use `"unmultiplied"` for legacy variance columns that have not
  been multiplied by `multiplier^2`. Standard-error columns are always
  interpreted on the displayed rate scale.

## Value

A tibble with the target stratum, rate, reference rate, rate difference,
rate ratio, log-rate-ratio standard error, z statistic, p_value,
confidence limits for the rate ratio, unusual, and status.

## Details

The input is normally an object returned by
`create_asr(..., show_var = TRUE)` or `create_asr(..., show_se = TRUE)`.
Existing variance columns are preferred when both forms are present. The
reference table must contain one matching row for each target stratum.
