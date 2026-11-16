# Example Population-Based Cancer Registry Data (`canregs`)

This dataset provides example data from population-based cancer
registries (PBCRs), structured using the `canregs` class. It can be used
for demonstration, testing, or development purposes within the
`canregtools` package.

## Usage

``` r
canregs
```

## Format

A list of PBCR datasets with class `canregs`.

## Source

Henan Province Cancer Registry, China.

## Examples

``` r

data("canregs")
summary(canregs[[1]])
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
```
