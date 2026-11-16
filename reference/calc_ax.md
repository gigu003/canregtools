# Calculate ax for life table

`calc_ax()` calculates the average number of person-years lived in the
interval by those dying in the interval (ax) based on age-specific
mortality rates (mx), starting ages (sage), and sex.

## Usage

``` r
calc_ax(mx, sage, sex = "male")
```

## Arguments

- mx:

  Numeric vector of age-specific mortality rates.

- sage:

  Numeric vector of starting ages for the age groups.

- sex:

  Character string specifying the sex: "male", "female", or "total"
  (default is "male").

## Value

A numeric vector of ax values.

## Details

For most age groups, \\a_x = n_x / 2\\, where \\n_x\\ is the width of
the age interval. For the infant group (age 0), ax is adjusted based on
sex and m0:

- Male: if m0 \>= 0.1, ax=0.33; else 0.045 + 2.684 \* m0

- Female: if m0 \>= 0.1, ax=0.35; else 0.053 + 2.8 \* m0

- Total: if m0 \>= 0.1, ax=0.34; else 0.049 + 2.742 \* m0

## References

- Coale, A. J., Demeny, P., & Vaughan, B. (1983). Regional Model Life
  Tables and Stable Populations (2nd ed.). New York: Academic Press.

- Preston, S. H., Heuveline, P., & Guillot, M. (2001). Demography:
  Measuring and Modeling Population Processes. Malden, MA: Blackwell
  Publishers.

## Examples

``` r
mx <- c(0.05, 0.01, 0.005)
sage <- c(0, 1, 5)
calc_ax(mx, sage, "male")
#> [1] 0.1792 2.0000    Inf
```
