# Retrieve raw numeric data from a tplyr_build result

Returns the unformatted numeric data that was computed during the build
process, before formatting and pivoting to wide format.

## Usage

``` r
tplyr_numeric_data(result, layer = NULL)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)

- layer:

  Integer layer index (1-based), or NULL for all layers

## Value

If `layer` is specified, a data.frame of raw statistics for that layer.
If `layer` is NULL, a named list of data.frames keyed by layer index.
Returns NULL if numeric data was not retained.

## See also

[`tplyr_stats_data()`](https://atorus-research.github.io/tplyr2/reference/tplyr_stats_data.md)
for a single statistic with its grouping columns.

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("AGEGR1"),
    group_desc("AGE")
  )
)
built <- tplyr_build(spec, tplyr_adsl)

# One data.frame per layer, keyed by layer index
names(tplyr_numeric_data(built))
#> [1] "1" "2"

# The counts behind the formatted cells, before rounding and padding
head(tplyr_numeric_data(built, 1))
#>                 TRT01P AGEGR1  n      pct total
#> 1              Placebo  65-80 42 48.83721    86
#> 2              Placebo    <65 14 16.27907    86
#> 3              Placebo    >80 30 34.88372    86
#> 4 Xanomeline High Dose  65-80 55 65.47619    84
#> 5 Xanomeline High Dose    <65 11 13.09524    84
#> 6 Xanomeline High Dose    >80 18 21.42857    84

# Every statistic the desc layer computed, including unused ones
names(tplyr_numeric_data(built, 2))
#>  [1] "TRT01P"    "n"         "n_records" "mean"      "sd"        "median"   
#>  [7] "var"       "min"       "max"       "iqr"       "q1"        "q3"       
#> [13] "missing"   "total"     "pct"      
```
