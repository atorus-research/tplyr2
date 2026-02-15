# Convert tplyr_build output to Analysis Results Data (ARD) format

Transforms the numeric data attached to a
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
result into a long-format data frame with one row per statistic per
group combination. This is compatible with the CDISC Analysis Results
Data standard.

## Usage

``` r
tplyr_to_ard(result)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)

## Value

A data.frame in long format with columns:

- analysis_id:

  Integer layer index

- stat_name:

  Character name of the statistic

- stat_value:

  Numeric value of the statistic

- ...:

  Grouping columns from the original data
