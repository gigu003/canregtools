
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canregtools <img src="man/figures/logo.png" align="right" height="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml)
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
  `create_age_rate()` calculate age specific rate, `create_sheet()`
  create a summary sheet containing some of these statistics.
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
library(canregtools)
data("canregs")
data <- canregs[[1]]

# count `canreg` data to `fbswicd`
fbsw <- data |>
  count_canreg(cancer_type = "big")

# calculate age standard rate (ASR) based on `fbswicd`
fbsw |> 
  create_asr(year, sex, cancer) |> 
  add_labels(lang = "en")
#> # A tibble: 84 × 14
#>    year sex   cancer site              icd10 no_cases    cr asr_cn2000 asr_wld85
#>   <int> <fct> <chr>  <fct>             <chr>    <dbl> <dbl>      <dbl>     <dbl>
#> 1  2021 Total 101    Oral Cavity & Ph… C00-…       27  3.95       3.23      3.32
#> 2  2021 Total 102    Nasopharynx       C11          7  1.02       0.78      0.83
#> 3  2021 Total 103    Esophagus         C15         65  9.52       6.88      6.65
#> 4  2021 Total 104    Stomach           C16        101 14.8       11.2      11.2 
#> 5  2021 Total 105    Conlon, Rectum &… C18-…      180 26.4       19.8      19.9 
#> # ℹ 79 more rows
#> # ℹ 5 more variables: truncr_cn2000 <dbl>, truncr_wld85 <dbl>, cumur <dbl>,
#> #   prop <dbl>, rank <int>

# calculate quality indicators based on `fbswicd`
fbsw |> 
  create_quality(cancer) |> 
  add_labels(label_type = "abbr", lang="en")
#> # A tibble: 28 × 16
#>    year sex   cancer site       icd10    rks   fbs  inci   sws  mort    mi    mv
#>   <int> <fct> <chr>  <fct>      <chr>  <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  9000 Total 101    Oral Cavi… C00-… 683110    27  3.95    13  1.9   0.48  55.6
#> 2  9000 Total 102    Nasophary… C11   683110     7  1.02     5  0.73  0.71  57.1
#> 3  9000 Total 103    Esophagus  C15   683110    65  9.52    46  6.73  0.71  86.2
#> 4  9000 Total 104    Stomach    C16   683110   101 14.8     79 11.6   0.78  71.3
#> 5  9000 Total 105    Colorectum C18-… 683110   180 26.4    103 15.1   0.57  81.7
#> # ℹ 23 more rows
#> # ℹ 4 more variables: dco <dbl>, ub <dbl>, sub <dbl>, m8000 <dbl>

# calculate age specific rate based on `fbswicd`
fbsw |> 
  create_age_rate() |> 
  add_labels(lang = "en")
#> # A tibble: 19 × 8
#>    year sex   cancer site        icd10 agegrp   cases  rate
#>   <int> <fct> <chr>  <fct>       <chr> <fct>    <int> <dbl>
#> 1  9000 Total 60     All Cancers ALL   0 岁         6  79.1
#> 2  9000 Total 60     All Cancers ALL   1-4 岁      12  42.0
#> 3  9000 Total 60     All Cancers ALL   5-9 岁      14  46.5
#> 4  9000 Total 60     All Cancers ALL   10-14 岁     6  18.5
#> 5  9000 Total 60     All Cancers ALL   15-19 岁     6  16.1
#> # ℹ 14 more rows
```
