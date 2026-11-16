# Using canregtools for Population-Based Cancer Registry Analysis

``` r

library(canregtools)
data("canregs")
registry <- canregs[[1]]
```

## What canregtools does

`canregtools` is organised around a practical population-based cancer
registry workflow. It helps users move from registry files or data
frames to standardised case records, age-grouped cancer counts, rates,
data-quality indicators, and analysis-ready reports.

The package uses two main object types. A `canreg` object keeps the
registry at the record level. An `fbswicd` object keeps the same
registry after cases have been cleaned, grouped by age, classified by
cancer site, and counted. The separation is useful in practice: users
can keep the source records for audit and reclassification while using
the summary object for routine indicators.

### The case-level `canreg` object

A single `canreg` object contains a registry identifier and three core
tables:

- `FBcases`: incidence or first-primary case records;
- `SWcases`: mortality records; and
- `POP`: population denominators by year, sex, and age group.

The case tables retain variables such as dates, sex, ICD-10 codes,
morphology, and basis of diagnosis. The population table contains the
denominator `rks`. Several registries are stored as a named `canregs`
collection.

### The summary-level `fbswicd` object

An `fbswicd` object is designed for downstream indicators. Its main
summary table is keyed by `year`, `sex`, `cancer`, and `agegrp`, and
contains the required first-primary count `fbs`. Its `pop` table is
keyed by `year`, `sex`, and `agegrp`, and contains the denominator
`rks`.

The summary table can also contain mortality and quality counts such as
`sws`, `mv`, `dco`, `mvs`, `ub`, `sub`, `ids`, and `mult`. When
available, `sitemorp` stores site and morphology frequency vectors, and
`exclude` stores records excluded during cleaning. Several summary
objects are stored as a named `fbswicds` collection.

## Step 1: import or construct a canreg object

### Read the standard Excel layout

When registry data are stored in the package’s expected Excel layout,
use
[`read_canreg()`](https://gigu003.github.io/canregtools/reference/read_canreg.md).
A single workbook returns one `canreg`; a directory or a vector of
workbooks returns a named `canregs` collection. The workbook name is
used as the registry identifier.

``` r

registry <- read_canreg("data/410100.xlsx", pop_type = "long")
registries <- read_canreg("data/registries", pop_type = "long")
```

For a wide population sheet, set `pop_type = "wide"` and provide the
relevant population-column arguments when the source layout differs from
the defaults.

### Construct from data frames already in R

Use
[`as_canreg()`](https://gigu003.github.io/canregtools/reference/as_canreg.md)
when the incidence, mortality, and population data have already been
imported or prepared. Separate incidence and mortality tables are
accepted directly:

``` r

registry <- as_canreg(
  inci = incidence_data,
  mort = mortality_data,
  pop = population_data,
  regicode = "410100"
)
```

If one raw case table contains both incidence and death dates, it can be
passed as `regi_data`:

``` r

registry <- as_canreg(
  regi_data = raw_registry_data,
  pop = population_data,
  year = 2023,
  regicode = "410100"
)
```

For multiple registries, keep the identifier in a column and use
`regicode_var`. The column must be present in the required input tables,
and the result is split into one named `canreg` object per registry:

``` r

registries <- as_canreg(
  inci = incidence_data,
  mort = mortality_data,
  pop = population_data,
  regicode_var = "registry_id"
)
```

The constructors standardise dates and year/sex fields and attach the
component classes `FBcases`, `SWcases`, and `POP`. Keep registry
identifiers as character values so that leading zeros are not lost.

## Step 2: manage registry data before counting

The `cr_*` functions are the data-management layer between import and
analysis. They work with the package’s object structure, so users can
select registries, filter rows, combine batches, and aggregate areas
without first flattening the object into one large data frame.

### Select registries with `cr_select()`

Use
[`cr_select()`](https://gigu003.github.io/canregtools/reference/cr_select.md)
when the question is “which registry objects should I keep?” It selects
elements from a `canregs` or `fbswicds` collection. Conditions are
evaluated separately for each element. Population-oriented aliases make
it possible to select registries by the number or range of available
population years:

``` r

selected <- cr_select(canregs, pop_n_years == 3)
selected <- cr_select(
  canregs,
  all(pop_years %in% as.character(2018:2022))
)
```

The `index` argument can also select by registry names, positions, or a
logical vector. For example,
`cr_select(canregs, index = c("410103", "410307"))` keeps only the named
elements. For a summary collection, conditions can refer to summary
indicators such as `inci` or `mv`.

### Filter rows with `cr_filter()`

Use
[`cr_filter()`](https://gigu003.github.io/canregtools/reference/cr_filter.md)
when the question is “which records or summary strata should I keep
inside these objects?” The `part` argument controls the components to
which the row filter is applied. For a `canreg`, common choices are
`"FBcases"`, `"SWcases"`, and `"POP"`; for an `fbswicd`, they are
`"fbswicd"`, `"sitemorp"`, `"pop"`, and `"exclude"`. Use `part = "all"`
when the same condition should be applied wherever its variables are
available.

``` r

male_cases <- cr_filter(registry, sex == "1", part = "FBcases")
one_year <- cr_filter(summary_object, year == "2021", part = "all")
without_totals <- cr_filter(
  summary_object,
  drop = "total",
  part = "fbswicd"
)
```

This is the key distinction:
[`cr_select()`](https://gigu003.github.io/canregtools/reference/cr_select.md)
chooses registry elements, whereas
[`cr_filter()`](https://gigu003.github.io/canregtools/reference/cr_filter.md)
filters rows within the chosen object’s components. Filtering case
records can return `NULL` when both incidence and mortality become
empty, so check the result before continuing the workflow.

### Clean and inspect the standard components with `cr_clean()`

[`cr_clean()`](https://gigu003.github.io/canregtools/reference/cr_clean.md)
prepares `FBcases`, `SWcases`, and `POP` for counting. It assigns years,
sex values, age groups, and cancer classifications while preserving the
`canreg` structure. It is useful when users want to inspect the cleaned
record-level tables before creating counts.
[`count_canreg()`](https://gigu003.github.io/canregtools/reference/count_canreg.md)
calls the same cleaning stage internally when the next goal is an
`fbswicd` summary.

``` r

cleaned <- cr_clean(
  registry,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  cancer_type = "big"
)
head(cleaned$FBcases)
```

### Combine batches with `cr_combine()`

Use
[`cr_combine()`](https://gigu003.github.io/canregtools/reference/cr_combine.md)
to append compatible batches that represent the same registries, such as
separate data deliveries or time periods. It accepts a list of
homogeneous `canregs` objects or a list of homogeneous `fbswicds`
objects. It retains registries that occur in only one input and
preserves optional components when they are available.

``` r

all_batches <- cr_combine(list(registries_2021, registries_2022))
```

The inputs must be named collections, and the collection names must
agree with the `regicode` stored in each element. Use
[`cr_combine()`](https://gigu003.github.io/canregtools/reference/cr_combine.md)
for row-appending; use
[`cr_merge()`](https://gigu003.github.io/canregtools/reference/cr_merge.md)
when the intended result is an aggregated registry.

### Aggregate with `cr_merge()`

Use
[`cr_merge()`](https://gigu003.github.io/canregtools/reference/cr_merge.md)
to pool the elements of a `canregs` or `fbswicds` collection into one
object. Case records are appended, while population strata are summed by
year, sex, and age group. For summary objects, matching count columns
are aggregated into one `fbswicd` object. The result records the source
registry identifiers so that the pooled result remains traceable.

``` r

pooled_registry <- cr_merge(canregs)
pooled_summary <- cr_merge(count_canreg(canregs, cancer_type = "big"))
```

### Rename and reframe registry collections

[`cr_rename()`](https://gigu003.github.io/canregtools/reference/cr_rename.md)
changes selected registry names and updates the top-level `regicode`
inside each renamed element. It is useful when source file names are not
suitable for presentation or when analysis-specific labels are needed.

``` r

renamed <- cr_rename(
  canregs,
  c("410103" = "REG_A", "410307" = "REG_B")
)
```

[`cr_reframe()`](https://gigu003.github.io/canregtools/reference/cr_reframe.md)
groups registries into a new administrative or reporting level and
merges the registries within each group. Available classification levels
can be inspected with `ls_vars("reframe")`; typical choices include
`"province"`, `"city"`, `"area_type"`, and `"region"`.

``` r

city_level <- cr_reframe(canregs, strat = "city")
province_level <- cr_reframe(
  count_canreg(canregs, cancer_type = "big"),
  strat = "province"
)
```

Choose the operation according to the data-management goal:

- select registry elements with
  [`cr_select()`](https://gigu003.github.io/canregtools/reference/cr_select.md);
- filter rows or cancer categories with
  [`cr_filter()`](https://gigu003.github.io/canregtools/reference/cr_filter.md);
- prepare components for counting with
  [`cr_clean()`](https://gigu003.github.io/canregtools/reference/cr_clean.md);
- append compatible batches with
  [`cr_combine()`](https://gigu003.github.io/canregtools/reference/cr_combine.md);
- pool registries by summing compatible strata with
  [`cr_merge()`](https://gigu003.github.io/canregtools/reference/cr_merge.md);
- change presentation identifiers with
  [`cr_rename()`](https://gigu003.github.io/canregtools/reference/cr_rename.md);
  and
- regroup registries by an administrative or reporting level with
  [`cr_reframe()`](https://gigu003.github.io/canregtools/reference/cr_reframe.md).

## Step 3: count cases and create an fbswicd object

The usual next step is
[`count_canreg()`](https://gigu003.github.io/canregtools/reference/count_canreg.md).
It cleans the case records, assigns age groups, classifies cancer sites,
and combines incidence, mortality, and population information into an
`fbswicd` object.

``` r

summary_object <- count_canreg(registry, cancer_type = "big")
names(summary_object)
#> [1] "regicode" "fbswicd"  "sitemorp" "pop"      "exclude"
```

The default age groups follow the package’s standard breaks. For a
specific analysis, supply `age_breaks` explicitly. For example, the
following creates three broad age groups:

``` r

summary_object <- count_canreg(
  registry,
  age_breaks = c(0, 15, 65),
  cancer_type = "big"
)
```

Use `cancer_type = "big"`, `"small"`, `"gco"`, or `"ci5"` according to
the classification needed by the analysis. The age grouping and cancer
classification are part of the summary step, so document them alongside
the results.

If summary tables have already been calculated outside the package, use
[`as_fbswicd()`](https://gigu003.github.io/canregtools/reference/as_fbswicd.md)
to create the same object contract:

``` r

summary_object <- as_fbswicd(
  fbsw = summary_table,
  pop = population_summary,
  sitemorp = site_morphology_summary,
  exclude = excluded_records,
  regicode = "410100"
)
```

Here `fbsw` must contain `year`, `sex`, `cancer`, `agegrp`, and `fbs`;
`pop` must contain `year`, `sex`, `agegrp`, and `rks`. Optional
components are kept when supplied. For multiple registries, use
`regicode_var` in the two required tables.

## Step 4: calculate the indicators you need

Once the `fbswicd` object is available, the same object can be passed to
the main analysis functions. For example, calculate age-standardized
rates, quality indicators, and age-specific rates as follows:

``` r

asr <- create_asr(summary_object, year, sex, cancer)
quality <- create_quality(summary_object, cancer)
age_rate <- create_age_rate(summary_object)

head(asr)
#> # A tibble: 6 × 12
#>   year  sex   cancer no_cases    cr asr_cn2000 asr_wld85 truncr_cn2000
#>   <chr> <chr> <chr>     <dbl> <dbl>      <dbl>     <dbl>         <dbl>
#> 1 2021  0     101          38  5.53       3.74      3.79          5.6 
#> 2 2021  0     102           5  0.73       0.71      0.71          1.29
#> 3 2021  0     103          59  8.58       4.53      4.85          6.13
#> 4 2021  0     104         123 17.9       11.1      11.2          13.7 
#> 5 2021  0     105         236 34.3       20.6      20.2          28.4 
#> 6 2021  0     106         128 18.6       11.5      11.4          17.1 
#> # ℹ 4 more variables: truncr_wld85 <dbl>, cumur <dbl>, prop <dbl>, rank <int>
head(quality)
#> # A tibble: 6 × 14
#>   year  sex   cancer    rks   fbs  inci   sws  mort    mi    mv   dco    ub
#>   <chr> <chr> <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 9000  0     101    687356    38  5.53    20  2.91  0.53  73.7  2.63     0
#> 2 9000  0     102    687356     5  0.73     2  0.29  0.4   40    0        0
#> 3 9000  0     103    687356    59  8.58    58  8.44  0.98  74.6  0        0
#> 4 9000  0     104    687356   123 17.9     82 11.9   0.67  71.5  1.63     0
#> 5 9000  0     105    687356   236 34.3    103 15.0   0.44  77.5  0.85     0
#> 6 9000  0     106    687356   128 18.6    101 14.7   0.79  54.7  1.56     0
#> # ℹ 2 more variables: sub <dbl>, mvs <dbl>
head(age_rate)
#> # A tibble: 6 × 6
#>   year  sex   cancer agegrp   cases  rate
#>   <chr> <chr> <chr>  <fct>    <dbl> <dbl>
#> 1 9000  0     60     0 岁         3  44.3
#> 2 9000  0     60     1-4 岁      12  31.4
#> 3 9000  0     60     5-9 岁       9  16.4
#> 4 9000  0     60     10-14 岁    13  32.5
#> 5 9000  0     60     15-19 岁     8  26.2
#> 6 9000  0     60     20-24 岁     8  27.1
```

[`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)
returns the crude rate and standardised rates, including the China 2000
and World 1985 standard populations by default. It can also return
confidence intervals, variance, or population columns.
[`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md)
returns indicators such as mortality-to-incidence, morphologically
verified, death-certificate-only, and unknown-primary proportions when
the corresponding counts are available.
[`create_age_rate()`](https://gigu003.github.io/canregtools/reference/create_age_rate.md)
returns rates by age group.

For site and morphology summaries, use the `sitemorp` component produced
by
[`count_canreg()`](https://gigu003.github.io/canregtools/reference/count_canreg.md):

``` r

site_morphology <- create_site_morp(summary_object)
```

The package also provides comparison functions for multi-registry
outputs. For example,
[`compare_rate()`](https://gigu003.github.io/canregtools/reference/compare_rate.md)
compares rate outputs and
[`compare_mv()`](https://gigu003.github.io/canregtools/reference/compare_mv.md)
evaluates morphologically verified percentages using the comparison
registries in the same stratum. These functions should be used only
after checking that the registries use compatible years, age groups,
cancer classifications, and population definitions.

## The complete user workflow

From a user’s perspective, the normal sequence is:

1.  read an Excel workbook or prepare data frames;
2.  create one `canreg` object per registry;
3.  choose the age grouping and cancer classification;
4.  call
    [`count_canreg()`](https://gigu003.github.io/canregtools/reference/count_canreg.md)
    to obtain `fbswicd` objects;
5.  calculate rates, quality indicators, age-specific results, and site
    or morphology summaries; and
6.  compare registries or pass the results to a report workflow.

The sample data illustrate the complete path:

``` r

data("canregs")
registry <- canregs[[1]]
summary_object <- count_canreg(registry, cancer_type = "big")
asr <- create_asr(summary_object, year, sex, cancer)

c(registry = class(registry)[1], summary = class(summary_object)[1])
#>  registry   summary 
#>  "canreg" "fbswicd"
head(asr[c("year", "sex", "cancer", "cr", "asr_cn2000", "asr_wld85")])
#> # A tibble: 6 × 6
#>   year  sex   cancer    cr asr_cn2000 asr_wld85
#>   <chr> <chr> <chr>  <dbl>      <dbl>     <dbl>
#> 1 2021  0     101     5.53       3.74      3.79
#> 2 2021  0     102     0.73       0.71      0.71
#> 3 2021  0     103     8.58       4.53      4.85
#> 4 2021  0     104    17.9       11.1      11.2 
#> 5 2021  0     105    34.3       20.6      20.2 
#> 6 2021  0     106    18.6       11.5      11.4
```

For an audit-oriented workflow,
[`validate_canreg()`](https://gigu003.github.io/canregtools/reference/validate_cases.md)
and
[`validate_fbswicd()`](https://gigu003.github.io/canregtools/reference/validate_cases.md)
are available as optional checks before or after the summary step. They
return structured issue tables without modifying the input; they
complement, rather than replace, review of the source data and analysis
assumptions.

## Overall and sex-specific summary rows

Summary and population tables may supply male/female rows, overall rows
(`sex = "0"`), or both. Before calculating rates or quality indicators,
[`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md),
[`create_quality()`](https://gigu003.github.io/canregtools/reference/create_quality.md),
and
[`create_age_rate()`](https://gigu003.github.io/canregtools/reference/create_age_rate.md)
retain an existing overall row once at each original stratum. A missing
overall row is generated from the available sex-specific rows. This
happens before years or other strata are combined, preventing double
counting when only some strata already have overall rows. When male,
female, and overall rows are all supplied, inconsistent count or
population totals cause an error. If only one sex is available alongside
a supplied overall row, the supplied overall is retained; the missing
sex is not inferred. Overall-only tables remain overall-only.

## Sparse summary tables and zero-case age groups

For each year/sex/cancer stratum present in a summary table, rate and
quality functions interpret absent age rows as zero cases and complete
them using the matching population ages. Population for zero-case ages
remains in the overall denominator. This interpretation requires a
complete population table for the intended analysis and treats the
summary table as a sparse count table, not as an incomplete collection
of unknown observations.

An explicitly supplied `NA`, non-finite, or negative summary count
causes an error rather than becoming zero. Entirely absent years, sexes,
or cancers are not invented. Missing population rows are never generated
as zero; ASR output requires every declared age group to be covered. If
missing case rows mean unknown rather than zero, resolve those omissions
before calculation instead of using them as a sparse zero-case table.

## Project registry dictionaries

Use a project directory to keep registry mappings and labels with the
analysis:

``` r

options(canregtools.dict_dir = file.path("config", "canregtools"))
cr_map(c("410103" = "410100"), attr = "registry")
write_regicode(data.frame(
  regicode = "410103", cname = "项目登记处", ename = "Project registry",
  abbr_cn = "项目", abbr_en = "Project"
))
ls_dict("registry")
ls_dict("regicode")
ls_dict_files(with_info = TRUE)
```

[`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md)
updates grouping attributes for
[`cr_reframe()`](https://gigu003.github.io/canregtools/reference/cr_reframe.md);
[`write_regicode()`](https://gigu003.github.io/canregtools/reference/write_regicode.md)
updates multilingual labels used immediately by
[`tidy_var()`](https://gigu003.github.io/canregtools/reference/tidy_var.md),
[`add_labels()`](https://gigu003.github.io/canregtools/reference/add_labels.md),
and `ls_vars("regicode")`. Label columns must be character, with unique
non-empty registry identifiers. The dictionary name `regicode` is
reserved for labels.

Data frames and named lists can also define mappings. A data frame needs
a `regicode` column and a column matching `attr`:

``` r

mapping <- data.frame(
  regicode = c("410103", "410307", "411721"),
  custom = c("A", "A", "B"),
  hdi = c("high", "high", "low")
)
cr_map(mapping, attr = c("custom", "hdi"))
ls_vars("reframe") # Includes custom and hdi from the active directory
grouped <- cr_reframe(canregs, strat = "custom")
cr_map(list("410307" = "910000"), attr = "area_type")
```

With several names in `attr`, each selected column is stored separately
and
[`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md)
returns a named list of effective dictionaries. Unselected columns are
ignored. All selected inputs and target files are checked under writer
locks before updates begin. An R-level write error restores previous
files and removes new files; this is not a crash-safe multi-file
transaction, and readers may see intermediate states during replacement.
`cache_refresh = TRUE` clears previous overrides for every selected
attribute before applying the new values.

The older `write_registry(x, dict = ...)` interface remains supported.
Built-in `province`, `city`, `region`, and `regicode` groupings are
derived from the identifiers and cannot be set through
[`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md).

New files store only user overrides, tagged with a schema version and
dictionary type. Reads merge these with the defaults in the installed
package, so newly added defaults appear after a package update. Built-in
`area_type` exceptions now apply even before any local dictionary file
is created (for example, the built-in mapping classifies `410307` as
`920000`). Older versions used digit-based inference until an area-type
dictionary file existed, which could produce different urban/rural
groupings. Use the public
[`ls_dict()`](https://gigu003.github.io/canregtools/reference/ls_dict.md)
reader rather than depending on the internal RDS layout. Reading a
dictionary does not create or change a file.

Existing vector and label-table files remain readable. On their next
successful update, a copy is retained alongside the file as
`*_dict.rds.backup-*`, and the file is migrated. All legacy entries are
retained as overrides: the old format cannot distinguish user edits from
copied defaults. As a result, legacy entries can still mask changed
defaults until explicitly reset. Backup files are omitted from
[`ls_dict_files()`](https://gigu003.github.io/canregtools/reference/ls_dict_files.md)
and are not automatically deleted.

To restore current defaults, use `reset_dict("registry")` or
`reset_dict("regicode")`. These operations back up an existing
dictionary first. The older `cache_refresh = TRUE` option still resets
overrides before applying the supplied update. Corrupt or incompatible
files stop reads and updates, including reset, and remain untouched for
inspection or recovery. After moving a damaged file aside, a new
dictionary can be created normally.

Writes are verified in a temporary file before replacing the original. A
writer lock prevents simultaneous updates or deletion; a conflicting
operation reports an error rather than silently losing changes. If a
process is terminated while holding a lock, verify that no writer is
active before removing its `*_dict.rds.lock` directory and retrying. All
dictionary operations respect the same `canregtools.dict_dir` setting.

## Reproducibility notes

For reproducible registry analyses, record the following with the
output:

- the source files or data-frame preparation code;
- the registry identifiers and the `canreg` construction method;
- `age_breaks`, `cancer_type`, and any `label_tail` used by
  [`count_canreg()`](https://gigu003.github.io/canregtools/reference/count_canreg.md);
- the standard population and options used for rate estimation; and
- any exclusions, denominator changes, or external comparison
  references.

This makes it possible to move from a published rate or quality
indicator back to the source registry records and the exact `fbswicd`
object used to calculate it.
