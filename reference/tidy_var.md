# Reformat variable values for Cancer Registration in China

Standardizes and labels values of a specified variable according to the
national cancer registration standard of China: T/CHIA 18-2021.

## Usage

``` r
tidy_var(
  x,
  var_name = "occu",
  label_type = "full",
  lang = "code",
  sep = "",
  suffix = FALSE,
  as_factor = FALSE
)
```

## Arguments

- x:

  A character vector containing raw values of a variable used in cancer
  registry data.

- var_name:

  A character string indicating the name of the variable to reformat
  (e.g., `"occu"` for occupation). Must be one of the variable names
  defined in `tidy_var_maps`.

- label_type:

  Type of the label used ("full" or "abbr").

- lang:

  Character, specify the output language, options are 'cn', or 'en',
  default is 'cn'.

- sep:

  A character string to separate the label.

- suffix:

  Logical. If `TRUE`, add a language-specific suffix to cancer labels
  when `var_name = "cancer"`: `"\u764c"` for Chinese labels and
  `" cancer"` for English labels. Some cancer categories such as
  sarcoma, lymphoma, leukemia, melanoma, and related groups do not
  receive a suffix.

- as_factor:

  Logical, indicate whether output value as factor.

## Value

A character or factor vector of reformatted values. The output depends
on the settings for `label_type`, `lang`, and `as_factor`:

- If `as_factor = FALSE`, returns a character vector.

- If `as_factor = TRUE`, returns a factor with sorted unique levels.

- The labels used depend on `lang` (`"cn"`, `"en"`, `"code"`, or
  `"icd10"`) and `label_type` (`"full"` or `"abbr"`).

## Details

`tidy_var()` converts raw character inputs into standardized labels,
codes, or abbreviations based on reference mappings defined for each
variable (e.g., occupation, basis of diagnosis, etc.). It supports both
Chinese and English outputs and can return values as factors with
labeled levels.

## Examples

``` r
occu <- c("11", "13", "17", "21", "24", "27", "31", "37", "51", "80", "90")
tidy_var(occu, var_name = "occu", lang = "cn")
#>  [1] "国家公务员"     "专业技术人员"   "职员"           "企业管理人员"  
#>  [5] "工人"           "农民"           "学生"           "现役军人"      
#>  [9] "自由职业者"     "退（离）休人员" "其他"          
tidy_var(occu, var_name = "occu", lang = "en")
#>  [1] "Civil Servant"                    "Professional/Technical Personnel"
#>  [3] "Clerk"                            "Enterprise Manager"              
#>  [5] "Worker"                           "Farmer"                          
#>  [7] "Student"                          "Active Duty Military Personnel"  
#>  [9] "Self-employed"                    "Retired"                         
#> [11] "Others"                          
tidy_var(occu, var_name = "occu", lang = "cn", label_type = "abbr")
#>  [1] "公务员" "技术"   "职员"   "管理"   "工人"   "农民"   "学生"   "军人"  
#>  [9] "自由"   "退休"   "其他"  
tidy_var(occu, var_name = "occu", lang = "en", label_type = "abbr")
#>  [1] "CS"  "PT"  "CL"  "EM"  "WK"  "FR"  "ST"  "ML"  "SE"  "RT"  "OTH"
```
