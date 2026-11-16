# Classify ICD10 codes to cancer categories

Classify ICD10 codes into Cancer Categories according to the specified
category type and language.

## Usage

``` r
classify_icd10(
  x,
  cancer_type = "big",
  lang = "code",
  label_type = "abbr",
  as_factor = FALSE
)
```

## Arguments

- x:

  The ICD10 codes of cancer part ('C00-C98 and D00-D48') which used in
  by the Population-Based Cancer Registration ('PBCR').

- cancer_type:

  A character string specifying the classification method used to
  categorize ICD-10 codes. This determines how ICD-10 codes are
  classified. Options include `"big"` (classify ICD-10 codes into 26
  cancer categories), `"small"` (classify ICD-10 codes into 59 cancer
  categories, more specific categories), `"system"` (classify ICD-10
  codes into organ system), and `"gco"` (classify ICD-10 code into
  cancer categories same as classification published by the Global
  Cancer Observatory), and `"ci5"` (classify ICD-10 codes using the
  63-category CI5-XII classification, with shared aggregate codes `60`
  and `61`). This parameter is only available when the input data is a
  vector of ICD-10 codes, or object with class of `'canreg'` or
  `'canregs'`.

- lang:

  Character, specify the output language, options are 'cn', or 'en',
  default is 'cn'.

- label_type:

  Type of the label used ("full" or "abbr").

- as_factor:

  Logical, indicate whether output value as factor.

## Value

Cancer code.

## Examples

``` r
icd10 <- c("C15.2", "C33.4", "C80.9", "C26.2", "C16.3")
classify_icd10(icd10, cancer_type = "big")
#> [1] "103" "110" "126" "126" "104"
classify_icd10(icd10, cancer_type = "small")
#> [1] "10" "21" "59" "59" "11"
classify_icd10(icd10, cancer_type = "system")
#> [1] "202" "203" "211" "202" "202"
classify_icd10(icd10, cancer_type = "gco")
#> [1] "306" "315" "337" "338" "307"
classify_icd10(icd10, cancer_type = "ci5")
#> [1] "410" "421" "461" "461" "411"
```
