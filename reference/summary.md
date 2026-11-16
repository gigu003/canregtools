# Summary object of class 'canreg'

Summary object of class 'canreg'

## Usage

``` r
# S3 method for class 'canreg'
summary(object, collapse = FALSE, ...)

# S3 method for class 'canregs'
summary(object, collapse = TRUE, ...)
```

## Arguments

- object:

  Object data with class of 'canreg', 'canregs'

- collapse:

  Collapse data or not.

- ...:

  Other filter expressions

## Value

A data frame contains summary statistics of canreg data.

## Examples

``` r
data("canregs")
data <- canregs[[1]]
summary(data)
#> $regicode
#> [1] "410103"
#> 
#> $rks
#> [1] 687356
#> 
#> $fbs
#> [1] 2800
#> 
#> $inci
#> [1] 407.36
#> 
#> $sws
#> [1] 1087
#> 
#> $mort
#> [1] 158.14
#> 
#> $mi
#> [1] 0.39
#> 
#> $mv
#> [1] 74.79
#> 
#> $dco
#> [1] 0.86
#> 
#> $death
#> [1] 0
#> 
#> $rks_year
#> [1] "2021"
#> 
#> $inci_vars
#> [1] "regi_num" "year"     "sex"      "agegrp"   "basi"     "icd10"    "cancer"  
#> [8] "morp"     "topo"    
#> 
#> $miss_r_vars_inci
#> [1] "birthda" "inciden" "deathda"
#> 
#> $mort_vars
#> [1] "regi_num" "year"     "sex"      "cancer"   "agegrp"   "icd10"   
#> 
#> $miss_r_vars_mort
#> [1] "birthda" "inciden" "deathda" "basi"   
#> 
#> attr(,"class")
#> [1] "summary" "list"   


summary(canregs)
#>   regicode    rks  fbs   inci  sws   mort   mi    mv  dco death rks_year
#> 1   410103 687356 2800 407.36 1087 158.14 0.39 74.79 0.86     0     2021
#> 2   410307 491325 1480 301.23  718 146.14 0.49 72.43 0.88     0     2021
#> 3   411721 882017 2200 249.43 1470 166.66 0.67 70.55 2.27     0     2016
#>                                                      inci_vars
#> 1 regi_num, year, sex, agegrp, basi, icd10, cancer, morp, topo
#> 2 regi_num, year, sex, agegrp, basi, icd10, cancer, morp, topo
#> 3 regi_num, year, sex, agegrp, basi, icd10, cancer, morp, topo
#>            miss_r_vars_inci                                  mort_vars
#> 1 birthda, inciden, deathda regi_num, year, sex, cancer, agegrp, icd10
#> 2 birthda, inciden, deathda regi_num, year, sex, cancer, agegrp, icd10
#> 3 birthda, inciden, deathda regi_num, year, sex, cancer, agegrp, icd10
#>                  miss_r_vars_mort
#> 1 birthda, inciden, deathda, basi
#> 2 birthda, inciden, deathda, basi
#> 3 birthda, inciden, deathda, basi
```
