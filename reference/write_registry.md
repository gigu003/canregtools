# Write registry attributes to a user-defined dictionary

Compatibility interface for
[`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md),
using `dict` instead of `attr`. New grouping workflows can use
[`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md)
directly.

## Usage

``` r
write_registry(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
)

# Default S3 method
write_registry(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
)

# S3 method for class 'data.frame'
write_registry(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
)

# S3 method for class 'list'
write_registry(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
)

# S3 method for class '`NULL`'
write_registry(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
)

# S3 method for class 'character'
write_registry(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
)
```

## Arguments

- x:

  A named list, a named character vector or a data frame that includes
  the columns `regicode` and the target attribute e.g., `registry`,
  `area_type`.

- dict:

  A character string specifying the dictionary to update. Built-in
  defaults are available for "registry" and "area_type"; any other name
  is treated as a user-defined dictionary name. The name `regicode` is
  reserved for
  [`write_regicode()`](https://gigu003.github.io/canregtools/reference/write_regicode.md).

- cache_refresh:

  If TRUE, refresh the dictionary to default values before updating.
  User-supplied mappings are then applied on top of those defaults.

- quiet:

  If TRUE, suppresses update and backup messages and returns the updated
  dictionary invisibly.

## Value

A tibble with two columns: `regicode` and the corresponding attribute
values. The result is returned invisibly when `quiet = TRUE`.

## Details

Stores mappings from registry identifiers to their attributes, e.g.,
China-specific registry names or area types, in a dictionary file. This
supports consistent labeling and downstream processing in cancer
registry data.

This function allows users to build or update local mapping dictionaries
for registry-level attributes. It supports multiple input formats and
updates files saved in the user-specific R configuration directory.
Registry identifiers must be non-empty and unique within one update. Set
`options(canregtools.dict_dir = "path/to/dictionaries")` to isolate
dictionaries for a project. By default the user configuration directory
is used. Only user overrides are stored; current built-in defaults are
merged when reading. Legacy dictionaries are backed up and migrated on
their next update, retaining all legacy entries as overrides because
their origin is unknown. Corrupt files stop the update and are retained.
Writes use a verified temporary file and replacement, with a lock
against concurrent writers. Use
[`reset_dict()`](https://gigu003.github.io/canregtools/reference/reset_dict.md)
to clear overrides (with a backup). `cache_refresh = TRUE` retains its
reset-before-update behavior.

## Examples

``` r
withr::with_options(
  list(canregtools.dict_dir = tempfile("canregtools-example-")),
  write_registry(list("410302" = "410301"))
)

# Registry attributes stored in data frame
registry_dict <- data.frame(
regicode = c("410302", "410303", "410304", "410305", "410306", "410307"),
registry = c(rep("410301", 5), "410300"),
area_type = rep("urban", 6)
)
withr::with_options(
  list(canregtools.dict_dir = tempfile("canregtools-example-")),
  write_registry(registry_dict)
)

# Registry attributes stored in list
dict <- list(
'410302' = '410301',
'410303' = '410301',
'410304' = '410301',
'410305' = '410301',
'410306' = '410301',
'410307' = '410301'
)
withr::with_options(
  list(canregtools.dict_dir = tempfile("canregtools-example-")),
  write_registry(dict)
)

# Registry attributes using built-in information
dict <- NULL
withr::with_options(
  list(canregtools.dict_dir = tempfile("canregtools-example-")),
  write_registry(dict)
)

# Registry attributes stored in named character vector with regicode as
# name and attributes as values
dict <- rep("410301", 5)
names(dict) <- c("410302", "410303", "410304", "410305", "410306")
withr::with_options(
  list(canregtools.dict_dir = tempfile("canregtools-example-")),
  write_registry(dict)
)
```
