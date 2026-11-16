# Write custom registry identifier labels to dictionary

Saves user-defined registry identifiers and their associated labels (in
both Chinese and English) to the local dictionary used by `canregtools`.
Changes are immediately used by
[`tidy_var()`](https://gigu003.github.io/canregtools/reference/tidy_var.md),
[`add_labels()`](https://gigu003.github.io/canregtools/reference/add_labels.md),
and `ls_vars("regicode")`. See
[`write_registry()`](https://gigu003.github.io/canregtools/reference/write_registry.md)
for storage and migration details.

## Usage

``` r
write_regicode(x = NULL, cache_refresh = FALSE)
```

## Arguments

- x:

  A data frame containing at least the following columns: `regicode`,
  `cname`, `ename`, `abbr_cn`, and `abbr_en`, all character. Registry
  identifiers must be non-missing, non-empty, and unique.

- cache_refresh:

  Logical. If TRUE, refresh the dictionary to default values before
  updating.

## Value

Invisibly returns `NULL`. The function is called for its side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
dict <- data.frame(
  regicode = c("410302"),
  cname = c("\u8001\u57CE\u533A"),
  ename = c("Laocheng District"),
  abbr_cn = c("\u8001\u57CE"),
  abbr_en = c("Laocheng")
)
write_regicode(dict)
} # }
```
