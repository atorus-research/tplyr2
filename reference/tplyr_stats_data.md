# Retrieve raw statistic values from a tplyr_build result

Filters the raw numeric data for a specific layer and statistic. Use
[`tplyr_numeric_data()`](https://atorus-research.github.io/tplyr2/reference/tplyr_numeric_data.md)
to get every statistic for the layer.

## Usage

``` r
tplyr_stats_data(result, layer, statistic)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)

- layer:

  Integer layer index (1-based)

- statistic:

  Character string naming the statistic column to extract (e.g., "n",
  "pct", "mean", "sd")

## Value

A data.frame with the layer's grouping columns and the requested
statistic. Returns NULL if the layer has no numeric data or does not
compute the statistic.

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_desc("AGE"))
)
built <- tplyr_build(spec, tplyr_adsl)
tplyr_stats_data(built, 1, "mean")
#>                 TRT01P     mean
#> 1              Placebo 75.20930
#> 2 Xanomeline High Dose 74.38095
#> 3  Xanomeline Low Dose 75.66667
```
