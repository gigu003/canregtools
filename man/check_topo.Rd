% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{check_topo}
\alias{check_topo}
\title{Verify if the topographic site codes for ICDO3 comply with the ICDO3
coding rules.}
\usage{
check_topo(x, return = "logical")
}
\arguments{
\item{x}{Character vector contains the topographic site codes for ICDO3.}

\item{return}{Character, specify the output type of the result.
Options are as below.
\itemize{
\item{'logical': Logical vector (default). Indicates whether
the topographic site codes comply with ICDO3 coding rules.
(TRUE for compliant, FALSE for non-compliant).}
\item{'formatted': Character vector containing formatted
topographic site code for ICDO3.}
}}
}
\value{
Logical vector or Character vector.
}
\description{
Verify if the topographic site codes for ICDO3 comply with the ICDO3
coding rules.
}
\examples{
topos <- c("C50.9", "16.2", "C151", "33.2")
check_topo(topos)
check_topo(topos, "formatted")

}
