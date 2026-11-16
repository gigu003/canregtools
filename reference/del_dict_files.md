# Delete a dictionary file from canregtools config

Removes a specified dictionary file (e.g., "registry", "area_type",
"custom") from the user's local configuration directory.

## Usage

``` r
del_dict_files(dict = "custom")
```

## Arguments

- dict:

  A character string specifying the dictionary name. Default is
  "custom".

## Value

Invisibly returns TRUE if deleted, FALSE otherwise.

## Examples

``` r
del_dict_files("custom")
#> Dictionary 'custom' does not exist.
```
