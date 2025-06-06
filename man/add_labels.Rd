% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_deal.R
\name{add_labels}
\alias{add_labels}
\title{Add variable labels for data set.}
\usage{
add_labels(x, label_type = "full", lang = "zh", as_factor = TRUE)
}
\arguments{
\item{x}{A data frame or tibble to be labeled.}

\item{label_type}{Type of the label used ("full" or "abbr").}

\item{lang}{Character, specify the output language, options are 'cn',
or 'en', default is 'cn'.}

\item{as_factor}{Logical, indicate whether output value as factor.}
}
\value{
A data frame or tibble with labeled variables.
}
\description{
This function labels variables such as \code{sex} and \code{cancer} in the dataset
based on the specified language and label type.
}
