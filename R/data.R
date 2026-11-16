#' Quality Indicators from the China Cancer Registry Annual Report
#'
#' This dataset contains key quality indicators of population-based cancer
#' registry (PBCR) data, as published in the China Cancer Registry Annual Report
#' by the National Cancer Center of China.
#'
#' @docType data
#' @format A data frame with quality indicators for various cancer types across
#'        different years and area types.
#' \describe{
#'   \item{\code{year}}{Calendar year of the PBCR data.}
#'   \item{\code{area_type}}{Area classification code: `910000` for urban areas,
#'   `920000` for rural areas.}
#'   \item{\code{cancer}}{Cancer type code.}
#'   \item{\code{mv}}{Proportion of morphologically verified (MV) cases.}
#'   \item{\code{dco}}{Proportion of cases identified through sdeath
#'          certificates only (DCO).}
#'   \item{\code{mi}}{Mortality-to-incidence (MI) ratio.}
#' }
#'
#' @source China Cancer Registry Annual Report, National Cancer Center, China.
#'
#' @examples
#'
#' data("quality")
"quality"

#' CI5-XII China reference data for registry comparisons
#'
#' A compact reference object built from the China registry records in
#' CI5-XII and CI5-XIId. The `asr` component is the pooled China incidence
#' reference with one row per `sex` and CI5-XII `cancer` category. It contains
#' crude rates and age-standardized rates for the World (1985) and China (2000)
#' standard populations, together with variances on the displayed per-100,000
#' rate scale. `asr_registry` keeps
#' the corresponding registry-specific rates for audit and exploratory use.
#'
#' CI5-XII/XIId provide incident cases and population at risk only. They do not
#' provide death counts, diagnosis basis, or exact microscopically verified case
#' counts. Consequently, `mv` and `mi` are typed empty tables and
#' `availability` records why those two reference standards are unavailable.
#' They must not be passed to [compare_mv()] or [compare_mi()] until an
#' appropriate external source is joined to them.
#'
#' @format A named list with components `registry`, `asr`, `asr_registry`,
#'   `mv`, `mi`, `availability`, and `metadata`.
#'
#' @source International Agency for Research on Cancer, Cancer Incidence in
#'   Five Continents, Volume XII and Volume XII detailed data. Source files are
#'   maintained in the easyCI5 project and are not copied into this package.
#'
#' @examples
#' data("ci5_xii_china")
#' head(ci5_xii_china$asr)
"ci5_xii_china"

#' Example Population-Based Cancer Registry Data (`canregs`)
#'
#' This dataset provides example data from population-based cancer registries
#' (PBCRs), structured using the `canregs` class. It can be used for
#' demonstration, testing, or development purposes within the `canregtools`
#' package.
#'
#' @docType data
#' @format A list of PBCR datasets with class `canregs`.
#' @source Henan Province Cancer Registry, China.
#'
#' @examples
#'
#' data("canregs")
#' summary(canregs[[1]])
"canregs"
