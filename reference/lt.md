# Construct a life table from age-specific mortality rates

`lt()` constructs a life table based on a vector of age-specific
mortality rates (mx), starting ages of age groups, and specified sex. It
calculates standard life table columns including the probability of
dying (qx), number of survivors (lx), number of deaths (dx),
person-years lived (Lx), total person-years remaining (Tx), and life
expectancy (ex). It can also compute a cause-deleted life table if
cancer_death is provided.

## Usage

``` r
lt(
  death = NULL,
  cancer_death = NULL,
  pop = NULL,
  mx = NULL,
  sage = c(0, 1, seq(5, 85, 5)),
  sex = "total",
  cohort = 1e+05,
  qx_method = "constant"
)
```

## Arguments

- death:

  Number of deaths from vital statistics.

- cancer_death:

  Cancer related death.

- pop:

  Average population size.

- mx:

  Numeric vector of age-specific mortality rates.

- sage:

  Numeric vector of starting ages for the age groups, default is
  `c(0, 1, seq(5, 85, 5))`.

- sex:

  Character string specifying the sex: "male", "female", or "total"
  (default is "total").

- cohort:

  The size of the initial cohort (default is 100000).

- qx_method:

  Character string specifying the method used to estiamte the
  probability of dying between age x and x + n (qx).

## Value

A data frame with the following columns:

- age:

  Starting age of each age group.

- mx:

  Age-specific mortality rate.

- qx:

  Probability of dying between age x and x+n.

- lx:

  Number of survivors at exact age x, starting from a radix of cohort.

- dx:

  Number of deaths between ages x and x+n.

- Lx:

  Person-years lived between ages x and x+n.

- Tx:

  Total person-years remaining after age x.

- ex:

  Life expectancy at exact age x.

Optionally includes pop, death, and cancer_death if provided.

## Details

The function uses standard demographic formulas to compute life table
values. The average number of person-years lived in the interval by
those dying in the interval (ax) is estimated based on standard formulas
for infant ages, and n/2 for other ages. The calculations assume a
starting population (radix) of cohort.

If cancer_death is provided, a cause-deleted life table is computed by
adjusting mx to exclude cancer deaths.

## Examples

``` r
pop <- c(3605201, 14795034, 41758253, 44202275, 44834666, 42184137, 40868806,
43408408, 33111965, 16344101, 6336435)
death <- c(19538, 2027, 9730, 37218, 71511, 104616, 193514, 450972, 686178,
816715, 963781)
cancer_death <- c(80, 328, 924, 1893, 2841, 10917, 33965, 87945, 152843,
169826, 144958)
lt(death = death, pop = pop, sage = c(0,1, seq(5, 85, 10)))
#>    age      mx      qx        lx       dx        Lx        Tx    ex      pop
#> 1    0 0.00542 0.00540 100000.00   540.47  99494.04 7716548.0 77.17  3605201
#> 2    1 0.00014 0.00055  99459.53    54.49 397729.12 7617054.0 76.58 14795034
#> 3    5 0.00023 0.00233  99405.04   231.35 992893.60 7219324.9 72.63 41758253
#> 4   15 0.00084 0.00838  99173.68   831.53 987579.19 6226431.3 62.78 44202275
#> 5   25 0.00159 0.01582  98342.15  1556.11 975641.00 5238852.1 53.27 44834666
#> 6   35 0.00248 0.02449  96786.05  2370.76 956006.66 4263211.1 44.05 42184137
#> 7   45 0.00474 0.04625  94415.29  4366.38 922320.97 3307204.4 35.03 40868806
#> 8   55 0.01039 0.09868  90048.91  8885.66 856060.75 2384883.5 26.48 43408408
#> 9   65 0.02072 0.18717  81163.24 15191.08 735677.01 1528822.7 18.84 33111965
#> 10  75 0.04997 0.39329  65972.16 25946.02 529991.49  793145.7 12.02 16344101
#> 11  85 0.15210 1.00000  40026.14 40026.14 263154.20  263154.2  6.57  6336435
#>     death
#> 1   19538
#> 2    2027
#> 3    9730
#> 4   37218
#> 5   71511
#> 6  104616
#> 7  193514
#> 8  450972
#> 9  686178
#> 10 816715
#> 11 963781
```
