# Calculate the actual age in completed years between two dates

Computes the exact age in full years at the time of an event (e.g.,
diagnosis, death, or survey) by comparing a person's birth date to the
date of the event.

## Usage

``` r
calc_age(birth_date, onset_date)
```

## Arguments

- birth_date:

  A vector of birth dates in `Date` format.

- onset_date:

  A vector of corresponding event dates in `Date` format (e.g., date of
  diagnosis).

## Value

A numeric vector of ages in years, with the same length as `birth_date`
and `onset_date`.

## Details

This function calculates age in *completed years*, taking into account
whether the birthday has occurred before the event date in the given
year. If `birth_date` or `onset_date` contains `NA`, the corresponding
result will be `NA`.

## Examples

``` r
# Generate random birth dates
set.seed(123)
sdate <- as.Date("1960-01-01")
edate <- as.Date("1980-12-31")
bdate <- sample(seq(sdate, edate, by = "1 day"), 100, replace = TRUE)

# Generate random event dates
sdate <- as.Date("2020-01-01")
edate <- as.Date("2023-07-08")
event <- sample(seq(sdate, edate, by = "1 day"), 100, replace = TRUE)

# Calculate ages
ages <- calc_age(bdate, event)
head(ages)
#> [1] 54 55 54 60 48 54
# Handle missing values
bdate[1] <- NA
event[2] <- NA
calc_age(bdate, event)[1:5]
#> [1] NA NA 54 60 48
```
