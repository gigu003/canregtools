# Render standardized cancer registry reports from built-in templates

The `create_report()` function generates a report from objects with
class of `canreg` or `canregs` using pre-defined rmarkdown templates.

## Usage

``` r
create_report(
  data,
  template = "annual",
  title = "Cancer Registry Report",
  output_format = "html_document",
  output_dir = NULL,
  ...
)

# S3 method for class 'canregs'
create_report(
  data,
  template = "annual",
  title = "Cancer Registry Report",
  output_format = "html_document",
  output_dir = NULL,
  ...
)

# S3 method for class 'canreg'
create_report(
  data,
  template = "annual",
  title = "Cancer Registry Report",
  output_format = "html_document",
  output_dir = NULL,
  ...
)
```

## Arguments

- data:

  An object of class `canreg` or `canregs`.

- template:

  Character string specifying the report template to use. Options
  include `"annual"`, `"quality"`, or `"quality-list-city"`. Default is
  `"annual"`.

- title:

  Character. Title of the generated report. Default is
  `"Cancer Registry Report"`.

- output_format:

  Character. Format of the rendered report. Options are
  `"html_document"`, `"word_document"`, or `"pdf_document"`. Default is
  `"html_document"`.

- output_dir:

  Character. Directory where the report will be saved.

- ...:

  Additional arguments passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Value

No return value; generates a report as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
data("canregs")
create_report(canregs, template = "quality", title = "QC Report")
} # }

if (FALSE) { # \dontrun{
create_report(canregs, template = "annual", title = "Annual Report")
} # }

if (FALSE) { # \dontrun{
data <- canregs[[1]]
create_report(data, template = "annual", title = "Annual Report")
} # }
```
