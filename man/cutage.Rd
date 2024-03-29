% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age.R
\name{cutage}
\alias{cutage}
\title{Cut age into groups.}
\usage{
cutage(
  x,
  method = "distance",
  length = 5,
  maxage = 85,
  sep_zero = TRUE,
  breaks = c(seq(0, 85, 5)),
  labels = NULL,
  lang = "cn",
  label_tail = NULL,
  right = FALSE
)
}
\arguments{
\item{x}{Vector contains the ages.}

\item{method}{The method to use for age grouping.
Options are "interval", "distance", or "quantile".}

\item{length}{The length of intervals for age grouping when method is
set to "distance".}

\item{maxage}{The maximum age for age grouping.}

\item{sep_zero}{A logical value indicating whether to include a separate
group for age 0.}

\item{breaks}{Custom breakpoints for the "interval" method.}

\item{labels}{labels for the levels of the resulting category.
By default, labels are constructed using "(a,b]" interval notation.
If labels = FALSE, simple integer codes are returned instead of
a factor.}

\item{lang}{Language used for output. Options are 'cn' or 'en'.
Default is 'cn.'.}

\item{label_tail}{A string to be appended at the end of each label.}

\item{right}{A logical value indicating whether the intervals are
right-closed or right-open.}
}
\value{
Factor of age groups.
}
\description{
Cut age into groups.
}
\examples{
age <- sample(0:101, 200, replace = TRUE)
agegrp <- cutage(age,
  method = "distance", length = 5,
  maxage = 60, sep_zero = TRUE)
agegrp
}
