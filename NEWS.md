# canregtools 0.2.11

* `ls_vars("reframe")` now includes valid custom grouping dictionaries from
  the active project/user directory, using their names as default labels and
  skipping invalid files with a warning. Built-in selection routing is unchanged.

* `create_asr()`, `create_quality()`, and `create_age_rate()` now complete
  absent age rows as zero counts within observed summary strata using the
  population table, preventing omitted zero-case ages from removing denominators.
  Explicit missing, non-finite, or negative summary counts are rejected, and
  ASR calculation requires coverage of all declared age groups.

* `create_asr()`, `create_quality()`, and `create_age_rate()` no longer double
  count existing overall-sex rows in summary or population data. Missing overall
  rows are generated at the original stratum before grouping; inconsistent
  totals are rejected when male, female, and overall rows coexist.

* `cr_map()` accepts multiple names in `attr` for data-frame batch updates,
  returning a named list of effective dictionaries. Batch writes preflight all
  inputs and targets under writer locks and roll back files on R-level errors.

* `cr_map()` is the recommended interface for configuring `cr_reframe()`
  grouping mappings via `attr`, accepting data frames, named character vectors,
  and named lists. `write_registry()` remains available for compatibility.

* Registry dictionaries now store typed user overrides and merge current built-in
  defaults on read. Legacy files are backed up on first update, retaining all
  old entries as overrides. Corrupt files stop updates without being overwritten;
  verified replacement writes and writer locks protect dictionary files.
  Built-in `area_type` exceptions now apply even without a local dictionary
  file, so affected urban/rural groupings can change.
* `ls_dict()` now displays effective mappings and registry labels;
  `ls_dict_files()` respects project dictionary directories and excludes backups.
* `reset_dict()` explicitly resets overrides, retaining a backup.
* `write_regicode()` updates now take effect immediately in `tidy_var()`,
  `add_labels()`, and `ls_vars("regicode")`; label keys and types are validated.
* `write_registry()` reserves the `regicode` dictionary for label updates,
  preventing accidental format replacement.

* `cr_filter()` now accepts injected conditions containing function objects.
* `cr_merge()` preserves optional survival cases regardless of registry order
  and supports further merging of already merged `fbswicd` objects.
* `cr_select()` treats missing condition values as non-matches; its documentation
  clarifies that separate conditions can match different rows.
* `create_quality()` now respects `decimal` when processing registry lists.
* `cumrate()` now excludes ages at or above `eage`, supports partial intervals,
  and validates the requested age range. Cumulative rates (including those in
  `create_asr()`) may decrease compared with earlier versions, which included
  an extra age group.

* `cr_filter()` now interprets `year` as integers in direct numeric comparisons
  and numeric `%in%` conditions, while preserving character comparisons and
  stored year labels. Period or invalid labels produce a warning in numeric
  comparisons.

* `get_stdpop()` now includes the 1976 and 2013 European Standard
  Populations and the 2000 U.S. Standard Population.
* `ls_vars("std")` now lists the added European and U.S. standard populations.
* `cr_select()` now avoids repeating the full cleaning pipeline when selecting
  registries by summary or population metrics.
* `count_canreg()` now benefits from faster standard-code handling during case
  cleaning; classification results and non-standard sex labels are unchanged.
* `compare_mv()`, `compare_mi()`, and `compare_rate()` now calculate each
  stratum in vectorized form instead of repeatedly slicing one-row data frames.
* `compare_rate()`, `cr_test()`, and `detect_anomaly()` now interpret rate
  variances on the displayed scale by default. Set
  `variance_scale = "unmultiplied"` for legacy variance inputs.
* `create_asr()` now extracts age-standardization results from list-columns
  without a rowwise data-frame pass.
* `create_asr()` now constructs optional variance, standard-error, and
  confidence-interval columns only when requested, reusing one variance
  extraction when both `show_var` and `show_se` are enabled.
* `create_asr()` gains `show_se` to return crude- and age-standardized-rate
  standard errors. `show_var` now returns variance on the same displayed rate
  scale, so `sqrt(var)` directly gives the corresponding standard error.
* `create_asr()` now reports missing or non-positive population denominators
  with registry and age-group context, and skips invalid registries when
  calculating a collection.
* `create_quality()` avoids sorting an intermediate case table that is sorted
  again in the final output.
* `create_age_rate()` uses vectorized row sums for wide-format totals.
* `cr_reframe()` now reuses computed registry-group indices and avoids an
  intermediate selection pass for each output group.
* `cr_merge()` now batches component binding in `cmerge()`, reducing repeated
  per-registry data-frame copies while preserving source identifiers.
* `count_canreg()` now aggregates incidence and mortality rows in one grouped
  pass instead of joining separately summarized tables.
* `count_canreg()` now supports a batch strategy for `canregs`, cleaning case
  tables once across registries while retaining `strategy = "registry"`
  for compatibility checks; batch mode does not emit a progress bar.

* `as_fbswicd()` constructs one `fbswicd` or registry-split `fbswicds`
  directly from summary, population, optional site/morphology, and exclusion
  data frames. Only `fbs` is required among statistic columns; omitted
  mortality and quality counts remain absent rather than being filled with
  zeros. Optional `sitemorp` and `exclude` components are omitted when `NULL`;
  explicitly supplied zero-row data frames are retained.

* `cr_combine()` now combines collections of `fbswicds` by registry name,
  including minimal incidence-only objects and sparse optional components.

* Added `validate_fbswicd()` for summary-level objects and collections. It
  checks component schemas, non-negative counts, duplicate strata, population
  coverage, registry identifiers, and optional site/morphology components.

* `create_site_morp()` now distinguishes an unavailable `sitemorp` component
  from an explicitly supplied zero-row component.

* `calc_age()` now extracts date components once per date vector, reducing
  case-cleaning time while preserving calendar-age and missing-value semantics.
* `count_canreg()` now marks duplicate IDs with vectorized operations instead
  of grouping each ID, retaining the existing non-missing duplicate counts.
* `cr_clean()` now accepts numeric text in population count columns, preserves
  missing optional `death` counts, and reports the exact variable and stratum
  for invalid population values. Population denominators (`rks`) remain strict.

* `ageadjust()` now returns finite Gamma bounds for zero-event rates under all
  interval methods, validates inputs, and correctly derives missing counts or
  populations from rates. Fractional case totals are no longer rounded.
* `as_canreg()` now applies `year` to separate inputs too: diagnosis years for
  incidence and death years for mortality. Population rows remain unchanged.
* `count_canreg()` now retrieves excluded mortality identifiers from `SWcases`.
* `cr_merge()` now gives merged `canreg` objects a scalar identifier and retains
  original identifiers in `source_regicodes`, satisfying registry validation.

* Dictionary storage can be isolated with `options(canregtools.dict_dir = ...)`.
  Tests now use temporary dictionaries instead of modifying user configuration.
* `cr_clean()` now matches population by age-group codes, supports standard
  18- and 19-group schedules, preserves fractional totals and empty inputs,
  and rejects incomplete or duplicated schedules instead of replacing them
  with zeros.
* `truncrate()` now excludes the group beyond the inclusive upper age limit
  and rejects boundaries that do not align with complete age groups.

* `expand_age_pop()` now falls back to non-negative linear interpolation when
  a cubic interpolation method would overshoot below zero.

* `benchmark_age_split()` now supports blind validation against an independent
  single-age schedule, reporting MAE, RMSE, total variation, ASR bias,
  source-group conservation, negative values, and available uncertainty
  diagnostics.

* `classify_icd10()` now supports the 63-category CI5-XII cancer
  classification (`cancer_type = "ci5"`), with site codes from 401 through
  461 and the shared aggregate codes 60 and 61.

* `tidy_var()` cancer labels now use consistent CI5/IARC English terminology,
  standardized ICD-10 range notation, and corrected full-name and abbreviation
  fields across the cancer classification tables.

* `expand_age_pop()` now accepts arbitrary `source_breaks`, preserves the
  final open age group, and enforces non-negative, exact effective
  source-group totals. It adds reference-schedule allocation, Sprague for
  regular closed five-year groups, and optional PCLM through `ungroup`.

* `expand_open_age_pop()` now provides a separate high-age module that either
  retains the open group or uses an external high-age reference schedule while
  retaining a final open group.

* `get_stdpop()` now returns standard-population weights for requested age
  groupings, using exact aggregation by default and opt-in interpolation only
  when a requested boundary splits an original group. `create_asr()` uses this
  mechanism when input age groups differ from the default structure.

* `create_asr()` now accepts 18 five-year age groups with age 0 included in
  the 0--4 group, combining the corresponding standard-population weights
  automatically.

* compare_rate() and cr_test() now support CI5-style statistical tests of
  incidence or mortality rate differences using rate variances.

* Added `ci5_xii_china`, a pooled China incidence reference from CI5-XII with
  registry-level ASR audit data. CI5-XII/XIId do not contain the death or
  diagnosis-basis fields required for MI or MV references, so those components
  are explicitly marked unavailable.

* compare_mi(), compare_mv(), and cr_test() can now use an external
  reference table instead of the other registries in the target data.

* Added `detect_anomaly()` for uncertainty-based consecutive-year and robust
  historical-level screening of annual rates and quality indicators returned
  by `create_asr()` and `create_quality()`.

* Registry identifiers now use `regicode` throughout `canreg` and `fbswicd`
  workflows, including comparison, validation, reports, and bundled
  dictionaries. This breaking change replaces `areacode`; arbitrary character
  identifiers are supported, while `classify_areacode()` remains specific to
  Chinese six-digit administrative codes.
* `compare_mi()` is added to test each registry's mortality-to-incidence ratio against other registries in the same stratum.
* `compare_mv()` is added to test each registry's morphologically verified percentage against other registries in the same stratum.
* `count_canreg()` now retains cases with missing IDs while excluding them from duplicate-case counts.
* `count_canreg()` now identifies `canreg` components by name, so its result no
  longer depends on their list order.
* `cr_combine()` is added to combine multiple `canregs` objects by registry name.
* `cr_select()` can now filter `canregs` and `fbswicds` by population-year count and range.
* `cr_test()` now provides a unified entry point for the M:I and MV% quality tests.
* `create_quality()` now reports rates and proportions with a zero denominator as missing rather than zero.
* Improved package metadata in DESCRIPTION, including a more informative package description, author ORCID, and affiliation.
* `year` and `sex` are now character identifiers throughout `canreg`,
  `fbswicd`, bundled data, and derived workflows. Population years accept
  single calendar-year labels and inclusive `"YYYY-YYYY"` period labels;
  period labels remain intact in results. `detect_anomaly()` remains limited
  to single calendar years because it evaluates consecutive annual values.
* Updated CRAN submission metadata and removed the unused VignetteBuilder field to avoid CRAN check notes when no package vignette is provided.
* `validate_canreg()`, `validate_cases()`, and `validate_population()` now provide structured, row-level data-quality checks before analysis.
* `classify_areacode()` now uses the updated area-code classification implementation with `attr` support and custom dictionaries; `classify_areacode2()` has been merged into `classify_areacode()`.
* `create_site_morp()` is added for counting ICD-10 site-code and morphology-code frequencies by user-defined strata.
* `cr_reframe()` again applies the registry dictionary for
  `strat = "registry"`, while preserving arbitrary identifiers that cannot be
  classified.
* `cr_rename()` renames selected registries in `canregs` and `fbswicds`
  objects while synchronizing their internal `regicode` values.
* `read_canreg()` now uses `readxl` as an optional dependency, reducing required package dependencies when Excel import is not needed.
* `as_canreg()` can now split incidence, mortality, and population data by a specified registry-code column and return a `canregs` object.
* `as_canreg()` now uses the shorter `inci`, `mort`, and `pop` argument names.
* `write_registry()` now validates registry mappings, preserves built-in defaults when refreshing, and returns the updated dictionary for inspection.

# canregtools 0.2.9

* Fixed an issue where mortality rates were not displayed when incidence rates were missing for certain cancer types.
* Corrected a calculation error in the m8000 statistics.

# canregtools 0.2.8

* Initial submission to CRAN.
