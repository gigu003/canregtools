# Construct a canreg object

This function constructs a `canreg` object, which contains cancer
registry data, including incidence, mortality, and population
information. Users can provide either a raw `regi_data` data frame
containing `inciden` and `deathda` columns, or supply `inci` and `mort`
data frames directly.

## Usage

``` r
as_canreg(
  regi_data = NULL,
  inci = NULL,
  mort = NULL,
  pop = NULL,
  year = NULL,
  regicode = NULL,
  regicode_var = NULL
)
```

## Arguments

- regi_data:

  A data frame containing raw registry data with at least the columns:

  - `inciden`: date of diagnosis

  - `deathda`: date of death If provided, `inci` and `mort` will be
    automatically derived.

- inci:

  A data frame containing incidence (case) data. If not provided, it can
  be derived from `regi_data`.

- mort:

  A data frame containing mortality (death) data. If not provided, it
  can be derived from `regi_data`.

- pop:

  A data frame containing population data. This is required.

- year:

  Optional vector of four-digit calendar years or inclusive
  `"YYYY-YYYY"` periods. If specified, only cases or deaths from those
  calendar years will be included, for both raw and separate input
  tables. Incidence uses `inciden` and mortality uses `deathda`; missing
  event dates are excluded when filtering. Population rows are not
  filtered.

- regicode:

  Character. A registry or area identifier for the data. It is stored as
  a character vector and is not limited to administrative division
  codes. Default is `NULL`.

- regicode_var:

  Optional character string naming the column that identifies registries
  in all supplied data frames. When provided, the data are split by this
  column and one `canreg` object is created per registry. A single
  registry still returns a `canreg`; multiple registries return a named
  `canregs` object.

## Value

A `canreg` object (S3 class `canreg`) or, when multiple registry codes
are found with `regicode_var`, a named `canregs` object. Each `canreg`
contains `regicode`, `FBcases`, `SWcases`, and `POP`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using raw registry data
canreg_obj <- as_canreg(
  regi_data = my_registry_df,
  pop = my_population_df,
  year = 2023,
  regicode = "410100"
)

# Build one canreg per registry using a grouping column
canregs_obj <- as_canreg(
  inci = incidence_df,
  mort = mortality_df,
  pop = population_df,
  regicode_var = "registry_id"
)

# Using separate incidence and mortality data frames
canreg_obj <- as_canreg(
  inci = my_incidence_df,
  mort = my_mortality_df,
  pop = my_population_df
)
} # }
```
