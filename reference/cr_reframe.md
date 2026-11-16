# Reframe data of class canregs or fbswicds

Reframe data of class canregs or fbswicds

## Usage

``` r
cr_reframe(x, strat = "registry")

# S3 method for class 'canregs'
cr_reframe(x, strat = "registry")

# S3 method for class 'fbswicds'
cr_reframe(x, strat = "registry")
```

## Arguments

- x:

  Object with class of canregs or fbswicds.

- strat:

  Stratification variables used to reframe 'canregs' or 'fbswicds'. Use
  [`cr_map()`](https://gigu003.github.io/canregtools/reference/cr_map.md)
  to configure registry, area-type, or custom grouping mappings.

## Value

Reframed canregs or fbswicds.

## Examples

``` r
# list reframe vars that could be used in `strat` parameter
ls_vars("reframe")
#> # A tibble: 6 × 5
#>   code      cname        ename                                   abbr_cn abbr_en
#>   <chr>     <chr>        <chr>                                   <chr>   <chr>  
#> 1 regicode  登记处代码   Cancer registry identifier              登记处  Regist…
#> 2 registry  登记处代码   Cancer Registry codes expressed as adm… 登记处  Regist…
#> 3 province  省份代码     Province expressed as administrative d… 省份    Provin…
#> 4 city      城市代码     City expressed as administrative divis… 城市    City   
#> 5 area_type 地区类型代码 Area type expressed as 6 digits code 9… 地区类型…… Area T…
#> 6 region    中国区域代码 Region in China express as digits code  区域    Region 
data("canregs")
# Reframe the `canregs` data according to `area_type` attribute
city <- cr_reframe(canregs, strat = "area_type")

# Reframe object with class of `canregs`
# Reframe the `canregs` according to the `province` attribute
province <- cr_reframe(canregs, strat = "province")
class(province)
#> [1] "canreg" "list"  
names(province)
#> [1] "regicode"         "FBcases"          "SWcases"          "POP"             
#> [5] "source_regicodes"

# Reframe object with class of `fbswicds`
# Convert object with class of `canregs` into object with class of `fbswicds`
fbsw <- count_canreg(canregs, cancer_type = "small")
# Reframe the `fbswicds` according to the `city` attribute
city <- cr_reframe(fbsw, strat = "city")
```
