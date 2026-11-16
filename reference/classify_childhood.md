# Classify childhood cancer according to the ICCC3 standards

`classify_childhood()`classifies childhood cancer based on ICD-O-3
codes, which include topography, morphology, and behavior codes, using
the International Classification of Childhood Cancer, Third Edition
(ICCC3).

## Usage

``` r
classify_childhood(topo, morp, beha, type = "sub", version = "v2005")
```

## Arguments

- topo:

  A character vector of ICD-O-3 topography codes (e.g., `"C15.6"` or
  `"C156"`).

- morp:

  A character vector of ICD-O-3 morphology codes (e.g., `"8000"` or
  `"M8140"`).

- beha:

  A numeric or character vector representing ICD-O-3 behavior codes.

- type:

  A string specifying the type of classification to return: `"main"` for
  main groups or `"sub"` for subgroups. Defaults to `"main"`.

- version:

  A string specifying the version of the ICCC-3 rules to use: either
  `"v2005"` or `"v2017"`. Defaults to `"v2005"`.

## Value

A numeric vector of ICCC-3 classification codes. If `type = "sub"`,
returns subgroup codes; if `type = "main"`, returns main group codes.

## References

Steliarova-Foucher, E., Stiller, C., Lacour, B. and Kaatsch, P. (2005),
International Classification of Childhood Cancer, third edition†‡.
Cancer, 103: 1457-1467.
[doi:10.1002/cncr.20910](https://doi.org/10.1002/cncr.20910)

## Examples

``` r
topo <- c("C15.2", "C16.2", "C34.2")
morp <- c("8000", "8040", "8170")
beha <- c("3", "3", "3")
child_code <- classify_childhood(topo, morp, beha, type = "main")
```
