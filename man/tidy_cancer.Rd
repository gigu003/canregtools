% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy.R
\name{tidy_cancer}
\alias{tidy_cancer}
\title{Tidy cancer code}
\usage{
tidy_cancer(x, label_type = "full", lang = "zh", as_factor = TRUE)
}
\arguments{
\item{x}{Cancer codes that output by classify_icd10.}

\item{label_type}{Type of labels used, options are 'full' or 'abbr'.}

\item{lang}{Language used for labels, options area 'en' or 'cn'.}

\item{as_factor}{Logical value, output factor or not.}
}
\value{
Factors of cancer code label.
}
\description{
Tidy cancer code
}
