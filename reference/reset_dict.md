# Reset user dictionary overrides

Clears user overrides for one dictionary, restoring current built-in
defaults. Existing files are backed up in the same directory before
replacement. Corrupt dictionaries are never overwritten; inspect or move
them first.

## Usage

``` r
reset_dict(dict = "registry", quiet = TRUE)
```

## Arguments

- dict:

  Dictionary name; use `"regicode"` for registry labels.

- quiet:

  Suppress update and backup messages.

## Value

The effective dictionary, invisibly.

## Examples

``` r
withr::with_options(
  list(canregtools.dict_dir = tempfile("canregtools-example-")),
  reset_dict("registry")
)
```
