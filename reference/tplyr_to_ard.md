# Convert tplyr_build output to Analysis Results Data (ARD) format

Transforms the numeric data attached to a
[`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)
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
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)

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

## See also

[`tplyr_from_ard()`](https://atorus-research.github.io/tplyr2/reference/tplyr_from_ard.md)
to rebuild a formatted table from an ARD.

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

ard <- tplyr_to_ard(built)
head(ard)
#>   analysis_id               TRT01P AGEGR1 stat_name stat_value
#> 1           1              Placebo  65-80         n         42
#> 2           1              Placebo    <65         n         14
#> 3           1              Placebo    >80         n         30
#> 4           1 Xanomeline High Dose  65-80         n         55
#> 5           1 Xanomeline High Dose    <65         n         11
#> 6           1 Xanomeline High Dose    >80         n         18

# One row per statistic per group; analysis_id identifies the layer
table(ard$analysis_id)
#> 
#>  1  2 
#> 27 42 
unique(ard$stat_name[ard$analysis_id == 2])
#>  [1] "n"         "n_records" "mean"      "sd"        "median"    "var"      
#>  [7] "min"       "max"       "iqr"       "q1"        "q3"        "missing"  
#> [13] "total"     "pct"      
```
