# Set registry grouping mappings for cr_reframe

Adds or updates user mappings from registry identifiers to grouping
attributes used by
[`cr_reframe()`](https://gigu003.github.io/canregtools/reference/cr_reframe.md).
User mappings take precedence over built-in mappings.

## Usage

``` r
cr_map(x = NULL, attr = "registry", cache_refresh = FALSE, quiet = TRUE)
```

## Arguments

- x:

  A named character vector, named list of scalar values, or a data frame
  containing `regicode` and the column(s) named by `attr`. Multiple
  attributes require a data frame. Registry identifiers must be
  non-empty and unique within an update. `NULL` writes the current
  overrides without adding mappings.

- attr:

  One or more unique grouping attributes: `"registry"`, `"area_type"`,
  or custom names also supplied to `cr_reframe(strat = ...)`. The
  built-in attributes `regicode`, `province`, `city`, and `region` are
  derived from identifiers and cannot be configured here. Use
  [`write_regicode()`](https://gigu003.github.io/canregtools/reference/write_regicode.md)
  for labels.

- cache_refresh:

  If `TRUE`, clear existing user overrides before applying `x`,
  retaining a backup. Defaults to `FALSE`.

- quiet:

  Suppress update and backup messages and return invisibly.

## Value

A tibble with `regicode` and `value` columns containing the effective
mappings for one attribute, or a named list of such tibbles for multiple
attributes, invisibly when `quiet = TRUE`.

## Details

Set `options(canregtools.dict_dir = "path/to/dictionaries")` to isolate
mappings for a project. See
[`write_registry()`](https://gigu003.github.io/canregtools/reference/write_registry.md)
for storage, validation, migration, and write protection.
[`write_registry()`](https://gigu003.github.io/canregtools/reference/write_registry.md)
remains available as a compatible single-attribute interface using
`dict` instead of `attr`. Batch updates validate all inputs and targets
under writer locks before writing. On an R-level write error, prior
files are restored and newly created files removed. This is not a
crash-safe multi-file transaction; readers may observe intermediate
states while files are replaced.

## See also

[`cr_reframe()`](https://gigu003.github.io/canregtools/reference/cr_reframe.md),
[`ls_dict()`](https://gigu003.github.io/canregtools/reference/ls_dict.md),
[`reset_dict()`](https://gigu003.github.io/canregtools/reference/reset_dict.md)

## Examples

``` r
withr::with_options(
  list(canregtools.dict_dir = tempfile("canregtools-example-")),
  {
    cr_map(c("410307" = "910000"), attr = "area_type")
    cr_map(list("410103" = "410100"), attr = "registry")
    mapping <- data.frame(
      regicode = c("410103", "410307", "411721"),
      custom = c("A", "A", "B"),
      hdi = c("high", "high", "low")
    )
    cr_map(mapping, attr = c("custom", "hdi"))
    grouped <- cr_reframe(canregs, strat = "custom")
    names(grouped)
  }
)
#> [1] "A" "B"
```
