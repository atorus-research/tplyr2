# Retrieve raw statistic values from a tplyr_build result

Filters the raw numeric data for a specific layer and statistic.

## Usage

``` r
tplyr_stats_data(result, layer, statistic)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)

- layer:

  Integer layer index (1-based)

- statistic:

  Character string naming the statistic column to extract (e.g., "n",
  "pct", "mean", "sd")

## Value

A data.frame with the grouping columns and the requested statistic.
Returns NULL if not available.
