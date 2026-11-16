# List dictionary files used by canregtools

Lists all dictionary `.rds` files stored in the user's config directory
for `canregtools`, or the project directory set by
`canregtools.dict_dir`. Migration and reset backup files are excluded.

## Usage

``` r
ls_dict_files(full.names = FALSE, with_info = FALSE)
```

## Arguments

- full.names:

  Logical. Whether to return full file paths. Default is `FALSE`.

- with_info:

  Logical. Whether to return file size and last modified time. Default
  is `FALSE`.

## Value

A character vector of file names, or a tibble with file info if
`with_info = TRUE`.

## Examples

``` r
ls_dict_files()
#> character(0)
ls_dict_files(full.names = TRUE)
#> character(0)
ls_dict_files(with_info = TRUE)
#> # A tibble: 0 × 4
#> # ℹ 4 variables: file <chr>, size_kb <dbl>, modified <dttm>, path <chr>
```
