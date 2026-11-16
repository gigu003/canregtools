# Expand one open-ended population age group

This high-age module is deliberately separate from ordinary
interpolation. By default it keeps the open group intact.
Reference-based splitting needs a high-age schedule and always retains a
final open group at `max_age`.

## Usage

``` r
expand_open_age_pop(
  x,
  lower,
  method = c("keep", "reference"),
  reference = NULL,
  max_age = NULL
)
```

## Arguments

- x:

  Non-negative total of the open group.

- lower:

  Lower bound of the open group.

- method:

  Either `"keep"` or `"reference"`.

- reference:

  Reference single-age schedule required for `"reference"`.

- max_age:

  Final lower bound for an open group after reference splitting.

## Value

A data frame with `x`, `y`, `age_end`, `open`, `lower`, and `upper`.
