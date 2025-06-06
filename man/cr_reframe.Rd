% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cr_reframe.R
\name{cr_reframe}
\alias{cr_reframe}
\alias{cr_reframe.canregs}
\alias{cr_reframe.fbswicds}
\title{Reframe data of class canregs or fbswicds}
\usage{
cr_reframe(x, strat = "registry")

\method{cr_reframe}{canregs}(x, strat = "registry")

\method{cr_reframe}{fbswicds}(x, strat = "registry")
}
\arguments{
\item{x}{Object with class of canregs or fbswicds.}

\item{strat}{Stratification variables used to reframe 'canregs' or
'fbswicds'.}
}
\value{
Reframed canregs or fbswicds.
}
\description{
Reframe data of class canregs or fbswicds
}
