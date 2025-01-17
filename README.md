
<!-- README.md is generated from README.Rmd. Please edit that file -->

## canregtools <img src="man/figures/logo.png" align="right" height="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Canregtools is an R package designed to streamline data analysis,
visualization, and reporting in Population-Based Cancer Registration
(PBCR).

It includes five sets of R functions that cover data reading,
processing, statistical calculations, visualization, and reporting. The
data processing functions carried out tasks such as categorizing age
into different groups, classifying ICD-10 (or ICD-O-3) codes into
categories using various methods, and tidying variables into a clean
format. The statistical calculating functions calculate statistics such
as age-standardized rates (ASR), truncated rates, cumulative rates,
cumulative risk, lifetime risk, life tables, and other quality
indicators. Additionally, it can generate a summary sheet containing
some of these statistics.

## Install

Currently, the package is not available on CRAN, but you can install the
development version of canregtools from GitHub. The installation method
is as follows:

``` r
## Install the remotes package
install.packages("remotes")

library(remotes)
install_github("gigu003/canregtools")
```
