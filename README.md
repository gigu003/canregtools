
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canregtools <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/canregtools)](https://CRAN.R-project.org/package=canregtools)
[![R-CMD-check](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/gigu003/canregtools/branch/main/graph/badge.svg)](https://codecov.io/gh/gigu003/canregtools)
<!-- badges: end -->

## Overview

canregtools is an R package designed to streamline data analysis,
visualization, and reporting in Population-Based Cancer Registration
(PBCR). It includes five sets of R functions that cover data reading,
processing, statistical calculations, visualization, and reporting.

- `read_canreg()` read data formats required by the National Cancer
  Center in China to object with class of `canreg` or `canregs`. The raw
  data is stored in an Excel file containing three sheets named FB, SW,
  and POP, where FB represents incidence data, SW represents mortality
  data, and POP represents population data.
- `count_canreg()` convert object with class of `canreg` or `canregs` to
  object with class of `fbswicd` or `fbswicds`.
- `create_asr()` calculate age-standardized rates (ASR), truncated rates
  and cumulative rates, `create_quality()` calculate quality indicators
  such as crude incidence or mortality, mortality to incidence ratio
  (M:I), percent of pathological verified cases (MV%), and etc. ,
  `create_age_rate()` calculate age specific rate.
- `draw_pyramid()` draw population pyramid, `draw_linechart()` draw line
  chart, `draw_barchart()` draw grouped bar chart.
- `create_report()` generate cancer registry report based on data of
  class “canreg” or “canregs”, the output file type can be Word, HTML,
  or PDF.

## Installation

Currently, the package is not available on CRAN, but you can install the
development version of canregtools from GitHub. The installation method
is as follows:

``` r
## Install the remotes package
install.packages("remotes")

library(remotes)
install_github("gigu003/canregtools")
```

## Usage

``` r
# Load sample data
library(canregtools)
data("canregs")
data <- canregs[[1]]

# Convert `canreg` to `fbswicd`
fbsw <- count_canreg(data, cancer_type = "big")

# 1. Age-standardized rate (ASR)
create_asr(fbsw, year, sex, cancer) |>
  add_labels(lang = "en")
#> # A tibble: 84 × 14
#>    year sex   cancer site              icd10 no_cases    cr asr_cn2000 asr_wld85
#>   <int> <fct> <chr>  <fct>             <chr>    <dbl> <dbl>      <dbl>     <dbl>
#> 1  2021 Total 101    Oral Cavity & Ph… C00-…       38  5.53       3.74      3.79
#> 2  2021 Total 102    Nasopharynx       C11          5  0.73       0.71      0.71
#> 3  2021 Total 103    Esophagus         C15         59  8.58       4.53      4.85
#> 4  2021 Total 104    Stomach           C16        123 17.9       11.1      11.2 
#> 5  2021 Total 105    Conlon, Rectum &… C18-…      236 34.3       20.6      20.2 
#> # ℹ 79 more rows
#> # ℹ 5 more variables: truncr_cn2000 <dbl>, truncr_wld85 <dbl>, cumur <dbl>,
#> #   prop <dbl>, rank <int>

# 2. Quality indicators
create_quality(fbsw, cancer) |>
  add_labels(label_type = "abbr", lang = "en")
#> # A tibble: 28 × 16
#>    year sex   cancer site       icd10    rks   fbs  inci   sws  mort    mi    mv
#>   <int> <fct> <chr>  <fct>      <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  9000 Total 101    Oral Cavi… C00-… 687356    38  5.53    13  1.89  0.34  73.7
#> 2  9000 Total 102    Nasophary… C11   687356     5  0.73     0  0     0     40  
#> 3  9000 Total 103    Esophagus  C15   687356    59  8.58    50  7.27  0.85  74.6
#> 4  9000 Total 104    Stomach    C16   687356   123 17.9     82 11.9   0.67  71.5
#> 5  9000 Total 105    Colorectum C18-… 687356   236 34.3    103 15.0   0.44  77.5
#> # ℹ 23 more rows
#> # ℹ 4 more variables: dco <dbl>, ub <dbl>, sub <dbl>, m8000 <dbl>

# 3. Age-specific rates
create_age_rate(fbsw) |>
  add_labels(lang = "en")
#> # A tibble: 19 × 8
#>    year sex   cancer site        icd10 agegrp   cases  rate
#>   <int> <fct> <chr>  <fct>       <chr> <fct>    <dbl> <dbl>
#> 1  9000 Total 60     All Cancers ALL   0 岁         3  44.3
#> 2  9000 Total 60     All Cancers ALL   1-4 岁      12  31.4
#> 3  9000 Total 60     All Cancers ALL   5-9 岁       9  16.4
#> 4  9000 Total 60     All Cancers ALL   10-14 岁    13  32.5
#> 5  9000 Total 60     All Cancers ALL   15-19 岁     8  26.2
#> # ℹ 14 more rows
```
