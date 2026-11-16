# Expand grouped population to single-year ages while preserving an open group

`expand_age_pop()` converts any set of closed age groups to single-year
estimates and keeps the final source group as an actual open age group.
It never represents an open group as a fabricated sequence of high ages.
Every effective source-group total is constrained to its input total
exactly, and estimates are non-negative.

## Usage

``` r
expand_age_pop(
  x,
  source_breaks = c(0, 1, seq(5, 85, 5)),
  method = "linear",
  reference = NULL,
  high_age = "keep",
  pclm_control = list()
)
```

## Arguments

- x:

  Numeric population totals. Its length must equal
  `length(source_breaks)`; the last value is the open age group.

- source_breaks:

  Integer lower bounds of the input age groups. The last bound starts
  the open group. The default is 0, 1–4, 5–9, ..., 85+.

- method:

  Splitting method: `"linear"`, `"constant"`, `"fmm"`, `"natural"`,
  `"monoh.fc"`, `"reference"`, `"sprague"`, or `"pclm"`. The spline
  options are retained for compatibility and apply only to closed
  groups. `"reference"` uses an external single-age schedule.
  `"sprague"` requires regular closed five-year groups; the default 0
  and 1–4 groups are automatically combined to 0–4. `"pclm"` requires
  `ungroup`.

- reference:

  An external single-age schedule for `method = "reference"`. Supply a
  named numeric vector (names are ages), or a data frame with `age`/`x`
  and `population`/`pop`/`y` columns. It must cover every closed age to
  be split. It should be independent of data evaluated in a blind
  validation.

- high_age:

  A character method or list supplied to
  [`expand_open_age_pop()`](https://gigu003.github.io/canregtools/reference/expand_open_age_pop.md).
  The default, `"keep"`, retains the source open group. Use a list with
  `method = "reference"`, `reference`, and `max_age` only when an
  external high-age schedule is available.

- pclm_control:

  A list passed to `ungroup::pclm(control = ...)`.

## Value

A data frame with `x` (age lower bound), `y` (population), `age_end`,
`open`, `source_group`, `lower`, and `upper`. Closed rows have
`age_end == x`; the final open row has `age_end = Inf`. PCLM uncertainty
intervals are returned when supplied by `ungroup`; other methods report
missing intervals. Attributes record the effective `source_breaks`,
method, and uncertainty method.

## References

Sprague, T. B. (1880). Explanation of a new formula for interpolation.
*Journal of the Institute of Actuaries*, 22, 270–285.

Rizzi, S., Gampe, J., and Eilers, P. H. C. (2015). Efficient ungrouping
of coarse histograms and age heaping in demographic data. *Statistical
Methods in Medical Research*, 24(3), 291–313.

## Examples

``` r
population <- c(
  5053, 17743, 25541, 32509, 30530, 34806, 36846, 38691, 40056,
  39252, 37349, 30507, 26363, 21684, 15362, 11725, 7461, 3260, 915
)
expand_age_pop(population, method = "sprague")
#>     x    y age_end  open source_group lower upper
#> 1   0 4985       0 FALSE            1    NA    NA
#> 2   1 4622       1 FALSE            1    NA    NA
#> 3   2 4420       2 FALSE            1    NA    NA
#> 4   3 4357       3 FALSE            1    NA    NA
#> 5   4 4412       4 FALSE            1    NA    NA
#> 6   5 4564       5 FALSE            2    NA    NA
#> 7   6 4792       6 FALSE            2    NA    NA
#> 8   7 5074       7 FALSE            2    NA    NA
#> 9   8 5391       8 FALSE            2    NA    NA
#> 10  9 5720       9 FALSE            2    NA    NA
#> 11 10 6087      10 FALSE            3    NA    NA
#> 12 11 6514      11 FALSE            3    NA    NA
#> 13 12 6755      12 FALSE            3    NA    NA
#> 14 13 6697      13 FALSE            3    NA    NA
#> 15 14 6456      14 FALSE            3    NA    NA
#> 16 15 6245      15 FALSE            4    NA    NA
#> 17 16 6005      16 FALSE            4    NA    NA
#> 18 17 5905      17 FALSE            4    NA    NA
#> 19 18 6043      18 FALSE            4    NA    NA
#> 20 19 6332      19 FALSE            4    NA    NA
#> 21 20 6585      20 FALSE            5    NA    NA
#> 22 21 6843      21 FALSE            5    NA    NA
#> 23 22 7047      22 FALSE            5    NA    NA
#> 24 23 7148      23 FALSE            5    NA    NA
#> 25 24 7183      24 FALSE            5    NA    NA
#> 26 25 7238      25 FALSE            6    NA    NA
#> 27 26 7294      26 FALSE            6    NA    NA
#> 28 27 7356      27 FALSE            6    NA    NA
#> 29 28 7435      28 FALSE            6    NA    NA
#> 30 29 7523      29 FALSE            6    NA    NA
#> 31 30 7599      30 FALSE            7    NA    NA
#> 32 31 7665      31 FALSE            7    NA    NA
#> 33 32 7733      32 FALSE            7    NA    NA
#> 34 33 7809      33 FALSE            7    NA    NA
#> 35 34 7885      34 FALSE            7    NA    NA
#> 36 35 7952      35 FALSE            8    NA    NA
#> 37 36 8013      36 FALSE            8    NA    NA
#> 38 37 8046      37 FALSE            8    NA    NA
#> 39 38 8040      38 FALSE            8    NA    NA
#> 40 39 8005      39 FALSE            8    NA    NA
#> 41 40 7957      40 FALSE            9    NA    NA
#> 42 41 7886      41 FALSE            9    NA    NA
#> 43 42 7828      42 FALSE            9    NA    NA
#> 44 43 7800      43 FALSE            9    NA    NA
#> 45 44 7781      44 FALSE            9    NA    NA
#> 46 45 7743      45 FALSE           10    NA    NA
#> 47 46 7707      46 FALSE           10    NA    NA
#> 48 47 7583      47 FALSE           10    NA    NA
#> 49 48 7327      48 FALSE           10    NA    NA
#> 50 49 6989      49 FALSE           10    NA    NA
#> 51 50 6655      50 FALSE           11    NA    NA
#> 52 51 6302      51 FALSE           11    NA    NA
#> 53 52 6010      52 FALSE           11    NA    NA
#> 54 53 5827      53 FALSE           11    NA    NA
#> 55 54 5713      54 FALSE           11    NA    NA
#> 56 55 5578      55 FALSE           12    NA    NA
#> 57 56 5439      56 FALSE           12    NA    NA
#> 58 57 5290      57 FALSE           12    NA    NA
#> 59 58 5121      58 FALSE           12    NA    NA
#> 60 59 4935      59 FALSE           12    NA    NA
#> 61 60 4756      60 FALSE           13    NA    NA
#> 62 61 4589      61 FALSE           13    NA    NA
#> 63 62 4385      62 FALSE           13    NA    NA
#> 64 63 4124      63 FALSE           13    NA    NA
#> 65 64 3830      64 FALSE           13    NA    NA
#> 66 65 3542      65 FALSE           14    NA    NA
#> 67 66 3247      66 FALSE           14    NA    NA
#> 68 67 3002      67 FALSE           14    NA    NA
#> 69 68 2840      68 FALSE           14    NA    NA
#> 70 69 2731      69 FALSE           14    NA    NA
#> 71 70 2615      70 FALSE           15    NA    NA
#> 72 71 2505      71 FALSE           15    NA    NA
#> 73 72 2376      72 FALSE           15    NA    NA
#> 74 73 2209      73 FALSE           15    NA    NA
#> 75 74 2020      74 FALSE           15    NA    NA
#> 76 75 1841      75 FALSE           16    NA    NA
#> 77 76 1667      76 FALSE           16    NA    NA
#> 78 77 1492      77 FALSE           16    NA    NA
#> 79 78 1317      78 FALSE           16    NA    NA
#> 80 79 1144      79 FALSE           16    NA    NA
#> 81 80  974      80 FALSE           17    NA    NA
#> 82 81  808      81 FALSE           17    NA    NA
#> 83 82  646      82 FALSE           17    NA    NA
#> 84 83  490      83 FALSE           17    NA    NA
#> 85 84  342      84 FALSE           17    NA    NA
#> 86 85  915     Inf  TRUE           18    NA    NA
```
