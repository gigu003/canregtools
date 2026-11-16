# Construct `fbswicd` objects from summary data frames

`as_fbswicd()` constructs the summary-level object used by
[`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md),
[`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md),
and related functions. Supply one summarized incidence table and its
matching population table. Optional site/morphology frequencies and
excluded records can also be included.

## Usage

``` r
as_fbswicd(
  fbsw,
  pop,
  sitemorp = NULL,
  exclude = NULL,
  regicode = NULL,
  regicode_var = NULL
)
```

## Arguments

- fbsw:

  A data frame with the key columns `year`, `sex`, `cancer`, and
  `agegrp`, plus the required incidence count `fbs`. Optional count
  columns `sws`, `mv`, `dco`, `mvs`, `ub`, `sub`, `ids`, and `mult` are
  retained when supplied.

- pop:

  A population data frame with `year`, `sex`, `agegrp`, and `rks`. An
  optional `death` column is retained.

- sitemorp:

  Optional data frame with `year`, `sex`, `cancer`, `icd10`, and `morp`.
  The last two columns must be list-columns of named numeric frequency
  vectors.

- exclude:

  Optional data frame containing records excluded during case cleaning.
  Its columns are retained without imposing a case schema.

- regicode:

  One non-missing registry identifier. It must be `NULL` when
  `regicode_var` is supplied.

- regicode_var:

  Optional name of a registry-code column. When supplied, `fbsw` and
  `pop` must contain exactly the same codes. Optional components may
  contain a subset of those codes. The grouping column is removed from
  constructed components.

## Value

One object of class `fbswicd`, or a named `fbswicds` object when
`regicode_var` contains multiple registry codes. A single code still
returns one `fbswicd` object.

## Details

Count columns may be numeric or numeric text. They are converted to
numeric values and must be finite and non-negative. Population
denominators cannot be missing; missing optional `death` counts are
preserved. Omitted optional count columns are not filled with zeros.
When `sitemorp` or `exclude` is `NULL`, that component is omitted. An
explicitly supplied zero-row data frame is retained, distinguishing an
available empty component from one that was not supplied. Rate and
quality functions interpret absent age rows within an observed summary
year/sex/cancer stratum as zero counts, using matching population ages.
Entirely absent strata and missing population rows are not inferred.
Registry order follows first appearance in `fbsw`. Functions calculating
mortality or quality indicators still require their corresponding count
columns.

## Examples

``` r
data("canregs")
counted <- count_canreg(canregs[[1]])
rebuilt <- as_fbswicd(
  fbsw = counted$fbswicd,
  pop = counted$pop,
  sitemorp = counted$sitemorp,
  exclude = counted$exclude,
  regicode = counted$regicode
)
```
