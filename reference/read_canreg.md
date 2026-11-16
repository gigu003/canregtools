# Read cancer registration data

Reads cancer registry data from one or more Excel files or from a
directory containing Excel files. It extracts case and population data
and returns objects of class `"canreg"` or a list of such objects
(`"canregs"`).

## Usage

``` r
read_canreg(
  x,
  pop_type = "long",
  age_var = "agegroup",
  pop_var = "popu",
  death_var = "death"
)
```

## Arguments

- x:

  A path to an Excel file, a character vector of file paths, or a
  directory containing Excel files.

- pop_type:

  A character string specifying the format of the population sheet. Must
  be either `"long"` (default) or `"wide"`.

- age_var:

  Name of the age group column (used only when `pop_type = "long"`).
  Default is `"agegroup"`.

- pop_var:

  Name of the population count column (used only when
  `pop_type = "long"`). Default is `"popu"`.

- death_var:

  Name of the death count column (used only when `pop_type = "long"`).
  If not present in the sheet, a column with 0s will be added. Default
  is `"death"`.

## Value

A `canreg` object (a list with components: `regicode`, `FBcases`,
`SWcases`, `POP`) if a single file is read. A named list of such objects
with class `"canregs"` if multiple files or a directory is provided.

## Examples

``` r
if (FALSE) { # \dontrun{
file_address <- "410302.xlsx"
canreg <- read_canreg(file_address, pop_type = "long")
} # }
```
