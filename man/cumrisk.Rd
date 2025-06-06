% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cumrate.R
\name{cumrisk}
\alias{cumrisk}
\title{Calculate the cumulative risk.}
\usage{
cumrisk(cumrate, mp = 100, decimal = 2)
}
\arguments{
\item{cumrate}{Cumulative rate.}

\item{mp}{A multiplier used to scale the calculated rates. Default is 100.}

\item{decimal}{Decimals of the calculated rates.}
}
\value{
Cumulative risk.
}
\description{
It was used to calculate the cumulative risk based on cumulative rate.
}
\examples{
px <- c(20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
        163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909)
dx <- c(156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
        891, 831, 926, 731, 269)
mx <- dx / px
cumrate(mx, eage = 70)
cumrisk(cumrate(mx, eage = 70))
}
