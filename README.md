
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canregtools <img src="man/figures/logo.png" align="right" height="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/canregtools)](https://CRAN.R-project.org/package=canregtools)
<!-- badges: end -->

肿瘤登记是恶性肿瘤监测的一项常规工作，它在人群中搜集、整理和统计分析肿瘤发病、死亡及生存资料，从而为肿瘤防控提供基础数据。**canregtools**包致力于为肿瘤登记工作提供数据逻辑核查、数据分析、可视化、和肿瘤登记报告等常用的功能和函数。

## 安装

目前该包还没有放到CRAN上，但是可以通过[GitHub](https://github.com/)来安装开发版本的canregtools，具体方法如下：

``` r
## 安装devtools包
install.packages("devtools")

## 利用devtools包安装存放在github上的canregtools
devtools::install_github("gigu003/canregtools")
```

或者

``` r
## 安装remotes包
install.packages("remotes")

library(remotes)
install_github("gigu003/canregtools")
```
