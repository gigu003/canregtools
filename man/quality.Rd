% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{quality}
\alias{quality}
\title{Quality Indicators from the China Cancer Registry Annual Report}
\format{
A data frame with quality indicators for various cancer types across different years and area types.
\describe{
\item{\code{year}}{Calendar year of the PBCR data.}
\item{\code{area_type}}{Area classification code: \code{910000} for urban areas,
\code{920000} for rural areas.}
\item{\code{cancer}}{Cancer type code.}
\item{\code{mv}}{Proportion of morphologically verified (MV) cases.}
\item{\code{dco}}{Proportion of cases identified through sdeath certificates only (DCO).}
\item{\code{mi}}{Mortality-to-incidence (MI) ratio.}
}
}
\source{
China Cancer Registry Annual Report, National Cancer Center, China.
}
\usage{
quality
}
\description{
This dataset contains key quality indicators of population-based cancer
registry (PBCR) data, as published in the China Cancer Registry Annual Report
by the National Cancer Center of China.
}
\examples{
data(quality)
}
\keyword{datasets}
