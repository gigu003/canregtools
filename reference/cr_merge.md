# Merge elements from objects of class `canregs`, `fbswicds`, or `asrs`

Merge elements from object with class of `canregs`, `fbswicds`, `asrs`,
`qualities`, `age_rates`, or `summaries` into object with class of
`canreg`, `fbswicd`, `asr`, `quality`, `age_rate`, or `summary`.

## Usage

``` r
cr_merge(data)

# S3 method for class 'canregs'
cr_merge(data)

# S3 method for class 'fbswicds'
cr_merge(data)

# S3 method for class 'asrs'
cr_merge(data)

# S3 method for class 'qualities'
cr_merge(data)

# S3 method for class 'age_rates'
cr_merge(data)

# S3 method for class 'site_morps'
cr_merge(data)

# S3 method for class 'checks'
cr_merge(data)

# S3 method for class 'summaries'
cr_merge(data)
```

## Arguments

- data:

  An object with class of `canregs`, `fbswicds`, `asrs`, `qualities`,
  `age_rates`, or `summaries`.

## Value

An object with merged elements.

## Details

Merged `canreg` objects store original registry identifiers in
`source_regicodes`. For multiple sources, the scalar `regicode` is
`merged:` followed by URL-encoded source codes joined with `+`. A single
source retains its identifier. Case-level source identifiers are
retained.

## Examples

``` r
data("canregs")
canreg <- cr_merge(canregs)
class(canreg)
#> [1] "canreg" "list"  

# Merge obejct with class of `fbswicds` into obejct with class of `fbswicd`
fbsws <- count_canreg(canregs)
fbsw <- cr_merge(fbsws)

# Merge obejct with class of `asrs` into object with class of `asr`
asrs <- create_asr(canregs, year, sex, cancer, collapse = FALSE)
asr <- cr_merge(asrs)

# Merge obejct with class of `qualities` into object with class of `quality`
quas <- create_quality(canregs, year, sex, cancer, collapse = FALSE)
qua <- cr_merge(quas)

# Merge obejct with class of `age_rates` into object with class of `age_rate`
agerates <- create_age_rate(canregs, year, sex, cancer, collapse = FALSE)
agerate <- cr_merge(agerates)

# Merge obejct with class of `summaries` into object with class of `summary`
summs <- summary(canregs, collapse = FALSE)
summ <- cr_merge(summs)
```
