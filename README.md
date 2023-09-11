
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canregtools <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/gigu003/canregtools/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gigu003/canregtools?branch=main)
[![R-CMD-check](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gigu003/canregtools/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

canregtools包的主要目的是为中国肿瘤登记工作者提供数据核查、数据准备、数据分析和可视化常用的功能和函数。

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

## 主要函数列表

[主要函数列表](https://gigu003.github.io/canregtools/reference/index.html)

## 应用实践

1.  [icd10编码分类](https://gigu003.github.io/canregtools/articles/classify_icd10.html)

## 开发计划

1.  登记处数据分类合并统计
2.  自动报告
3.  ICDO3编码转换
4.  儿童肿瘤分类
