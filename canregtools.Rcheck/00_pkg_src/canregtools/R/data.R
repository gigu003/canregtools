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
