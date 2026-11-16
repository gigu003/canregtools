# Tidy gender variable

Standardizes gender-related values into consistent character codes or
factors. This function maps various gender-related character strings
(e.g., "male", "female", "man", "woman", "1", "2", etc.) to standardized
numeric values: `1` for male, `2` for female, and `0` for total. It
supports both Chinese and English labels. Optionally, the result can be
returned as a factor with appropriate labels.

## Usage

``` r
tidy_sex(x, lang = "cn", as_factor = FALSE)
```

## Arguments

- x:

  A character or numeric vector containing gender information.

- lang:

  Character, specify the output language, options are 'cn', or 'en',
  default is 'cn'.

- as_factor:

  Logical, indicate whether output value as factor.

## Value

A character vector or a factor representing gender:

- 0:

  Total

- 1:

  Male

- 2:

  Female

If `as_factor = TRUE`, a factor is returned with labels in the specified
language (`lang`).

## Examples

``` r
gender <- c("male", "men", "women", "female", "women", "man", "1", "2")
tidy_sex(gender)
#> [1] "1" "1" "2" "2" "2" "1" "1" "2"
```
