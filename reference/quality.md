# Quality Indicators from the China Cancer Registry Annual Report

This dataset contains key quality indicators of population-based cancer
registry (PBCR) data, as published in the China Cancer Registry Annual
Report by the National Cancer Center of China.

## Usage

``` r
quality
```

## Format

A data frame with quality indicators for various cancer types across
different years and area types.

- `year`:

  Calendar year of the PBCR data.

- `area_type`:

  Area classification code: `910000` for urban areas, `920000` for rural
  areas.

- `cancer`:

  Cancer type code.

- `mv`:

  Proportion of morphologically verified (MV) cases.

- `dco`:

  Proportion of cases identified through sdeath certificates only (DCO).

- `mi`:

  Mortality-to-incidence (MI) ratio.

## Source

China Cancer Registry Annual Report, National Cancer Center, China.

## Examples

``` r

data("quality")
```
