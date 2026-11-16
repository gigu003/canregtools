# Tidy age description

Parses age descriptions written in Chinese and converts them into
numeric values expressed in years, months, or days. It interprets age
strings containing Chinese characters such as (years), (months), and
(days), and converts them to a numeric vector representing age in the
specified unit.

## Usage

``` r
tidy_age(x, unit = "year")
```

## Arguments

- x:

  A character vector containing age descriptions in Chinese.

- unit:

  A character string specifying the unit of the returned values. Options
  are `"year"` (default), `"month"`, or `"day"`.

## Value

A numeric vector representing ages in the specified unit:

- year: Truncated age in years.

- month: Truncated age in months.

- day: Rounded age in days.

## Examples

``` r
agedes <- c(
  "50\u5c8110\u67083\u6708", "19\u5c815\u6708",
  "1\u5c8130\u6708", "3\u670820\u6708", "30\u6708"
)
tidy_age(agedes, unit = "year")
#> [1] 50 19  3  1  2
tidy_age(agedes, unit = "month")
#> [1] 602 232  41  20  30
tidy_age(agedes, unit = "day")
#> [1] 18354  7092  1278   609   913
```
