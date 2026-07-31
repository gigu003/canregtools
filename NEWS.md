# canregtools 0.2.11

* `areacode` identifiers are now preserved as character values throughout `canreg` and `fbswicd` workflows, allowing non-administrative registry codes.
* Improved package metadata in DESCRIPTION, including a more informative package description, author ORCID, and affiliation.
* Standardized `year` and `sex` columns as integer values across `canreg`, `fbswicd`, and derived summary workflows.
* Updated CRAN submission metadata and removed the unused VignetteBuilder field to avoid CRAN check notes when no package vignette is provided.
* `classify_areacode()` now uses the updated area-code classification implementation with `attr` support and custom dictionaries; `classify_areacode2()` has been merged into `classify_areacode()`.
* `create_site_morp()` is added for counting ICD-10 site-code and morphology-code frequencies by user-defined strata.
* `cr_reframe()` fixes missing deaths variable from population data in merging.
* `cr_rename()` is added to rename selected registries in a `canregs` object while synchronizing their internal `areacode` values.
* `read_canreg()` now uses `readxl` as an optional dependency, reducing required package dependencies when Excel import is not needed.

# canregtools 0.2.9

* Fixed an issue where mortality rates were not displayed when incidence rates were missing for certain cancer types.
* Corrected a calculation error in the m8000 statistics.

# canregtools 0.2.8

* Initial submission to CRAN.
