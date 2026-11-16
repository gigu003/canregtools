# Create a standardized cancer registry report data object

`create_canreg_report_data()` builds a compact report-ready object from
`canregs` or `fbswicds` data. The object stores summary-level indicators
for the province/all areas, urban areas, and rural areas, as well as
registry-level indicators for individual registries.

## Usage

``` r
create_canreg_report_data(
  data,
  year,
  province = NULL,
  province_en = NULL,
  population_total = NULL,
  summary_codes = c(all = "410000", urban = "910000", rural = "920000"),
  cancer_codes = c(101:110, 112, 114:118, 120:125),
  cancer_total_code = 60,
  cancer_type = "big",
  keep_source = TRUE,
  include_age_registry = TRUE
)
```

## Arguments

- data:

  An object of class `canregs` or `fbswicds`.

- year:

  Data year stored as metadata. The statistical calculations still use
  the `year` column in the input data as a grouping variable.

- province, province_en:

  Province names stored as metadata.

- population_total:

  Optional total provincial population stored as metadata.

- summary_codes:

  Named character vector identifying the summary groups. Defaults to
  `c(all = "410000", urban = "910000", rural = "920000")`.

- cancer_codes:

  Cancer codes to store as metadata for report generation.

- cancer_total_code:

  Code for all malignant cancers. Defaults to `60`.

- cancer_type:

  Cancer classification type passed to
  [`count_canreg()`](https://gigu003.github.io/canregtools/reference/count_canreg.md)
  when `data` is a `canregs` object.

- keep_source:

  Logical. If `TRUE`, keep intermediate `fbswicd` objects in `source`.

- include_age_registry:

  Logical. If `TRUE`, calculate registry-level age-specific incidence
  and mortality rates.

## Value

An object of class `canreg_report`.

## Examples

``` r
if (FALSE) { # \dontrun{
data("canregs")
report_data <- create_canreg_report_data(canregs, year = 2022)
summary(report_data)
} # }
```
