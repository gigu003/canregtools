% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_asr.R
\name{create_asr.fbswicd}
\alias{create_asr.fbswicd}
\title{Calculate age-standardized rate.}
\usage{
\method{create_asr}{fbswicd}(
  x,
  ...,
  event = fbs,
  pop = rks,
  agegrp = agegrp,
  std = c(china, segi),
  mp = 1e+05,
  decimal = 2,
  show_var = FALSE,
  show_ci = FALSE,
  keep_sex_specific = FALSE
)
}
\arguments{
\item{x}{data with class of fbswicd.}

\item{...}{Stratification factors.}

\item{event}{A variable within the input data that corresponds to the cases
counts.}

\item{pop}{A variable within the input data that corresponds to the
population in risk in each stratification.}

\item{agegrp}{A variable within the input data that corresponds to the age
categories.}

\item{std}{Specify the standard population structure in the 'std_pop'
data frame used for calculating standardized rates. When calculating
standardized rates for multiple standard populations, specify
std = c("segi", "china").}

\item{mp}{A constant to multiply rates by (e.g. mp=1000 for rates per 1000).}

\item{decimal}{This parameter specifies the number of decimal places to
round the results. The default is 2, which means rates will be
rounded to two decimal places.}

\item{show_var}{Logical value whether output variance or not.}

\item{show_ci}{Logical value whether output confidence(lower or upper
bound) or not.}

\item{keep_sex_specific}{deal with sex specific disease.}
}
\value{
A data frame or tibble contains the age standard rates and CIs.
}
\description{
Calculate age-standardized rate.
}
