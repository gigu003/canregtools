# List variable names and their descriptions by category

This function returns a `tibble` containing variable names and their
descriptions based on the specified category. For `type = "reframe"`,
valid custom grouping dictionaries from the active dictionary directory
are appended to the built-in attributes, using their names as default
labels. Registry label files and backup files are excluded; invalid
grouping files are skipped with a warning.

## Usage

``` r
ls_vars(type = "std")
```

## Arguments

- type:

  A character string specifying the type of variables to list. Options
  are `"std"` for standard population variables, `"summary"` for summary
  variables, and `"reframe"` for variables used in
  [`cr_reframe()`](https://gigu003.github.io/canregtools/reference/cr_reframe.md).

## Value

A tibble with five columns:

- `code`: The name of the variable.

- `cname`: A detailed description of the variable in Chinese.

- `ename`: A detailed description of the variable in English.

- `abbr_cn`: An abbreviated description of the code label in Chinese.

- `abbr_en`: An abbreviated description of the code label in English.

## See also

[`cr_reframe()`](https://gigu003.github.io/canregtools/reference/cr_reframe.md),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`create_asr()`](https://gigu003.github.io/canregtools/reference/create_asr.md)

## Examples

``` r
ls_vars("std")
#> # A tibble: 8 × 5
#>   code    cname              ename                               abbr_cn abbr_en
#>   <chr>   <chr>              <chr>                               <chr>   <chr>  
#> 1 cn64    1964年中国标准人口 Standard population in Chinese in … 中国64  CN64   
#> 2 cn82    1982年中国标准人口 Standard population in Chinese in … 中国82  CN82   
#> 3 cn2000  2000年中国标准人口 Standard population in Chinese in … 中国2000… CN2000 
#> 4 wld85   Segi世界标准人口   Segi's world standard population    世界Segi… WLD85  
#> 5 wld2000 2000年世界标准人口 World standard population in 2000   世界2000… WLD2000
#> 6 eur76   1976年欧洲标准人口 European Standard Population in 19… 欧洲76  EUR76  
#> 7 eur2013 2013年欧洲标准人口 European Standard Population in 20… 欧洲2013… EUR2013
#> 8 usa2000 2000年美国标准人口 U.S. Standard Population in 2000    美国2000… USA2000
ls_vars("summary")
#> # A tibble: 14 × 5
#>    code      cname                  ename                        abbr_cn abbr_en
#>    <chr>     <chr>                  <chr>                        <chr>   <chr>  
#>  1 rks       人口数                 Population                   人口    Pop    
#>  2 fbs       发病数                 Number of Incidence          发病数  No.Inc…
#>  3 sws       死亡数                 Number of Deaths             发病数  No.Dea…
#>  4 inci      发病率                 Incidence                    发病率  Incide…
#>  5 mort      死亡率                 Mortality                    死亡率  Mortal…
#>  6 mi        死亡发病比             Mortality Incidence Ratio    死亡发病比…… M:I    
#>  7 mv        病理诊断比例(%)        Proportion of Morphology Ve… 病理诊断比例… MV     
#>  8 dco       只有死亡医学证明书比例 Proportion of Death Certifi… 死亡医学证明… DCO    
#>  9 ub        部位不明病例比例       Proportion of Primary Site … 部位不明比例… UB     
#> 10 sub       亚部位不明病例比例     Proportion of Primary Subsi… 亚部位不明…… SUB    
#> 11 death     全死因死亡数           All cause mortality          全因死亡数…… death  
#> 12 rks_year  人口数年份             Years that have population   人口年份…… Years  
#> 13 inci_vars 发病库变量名           Vars in incidence            发病库变量名… Vars i…
#> 14 mort_vars 死亡库变量名           Vars in mortality            死亡库变量名… Vars i…
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
```
