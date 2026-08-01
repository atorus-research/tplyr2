# Reconstruct a formatted table from ARD and a spec

Takes Analysis Results Data (long format) and a `tplyr_spec`, then
applies the spec's formatting rules to produce a formatted output table.

## Usage

``` r
tplyr_from_ard(ard, spec)
```

## Arguments

- ard:

  A data.frame in ARD format (as produced by
  [`tplyr_to_ard()`](https://atorus-research.github.io/tplyr2/reference/tplyr_to_ard.md))

- spec:

  A `tplyr_spec` object defining the table structure

## Value

A data.frame with the same structure as
[`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)
output

## See also

[`tplyr_to_ard()`](https://atorus-research.github.io/tplyr2/reference/tplyr_to_ard.md)
to produce the ARD.

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_desc("AGE"))
)
built <- tplyr_build(spec, tplyr_adsl)

# Round-tripping through the ARD reproduces the formatted cells, so a table
# can be rebuilt from stored results without the subject-level data
ard <- tplyr_to_ard(built)
recon <- tplyr_from_ard(ard, spec)
recon[, c("rowlabel1", "res1")]
#>   rowlabel1         res1
#> 1         n           86
#> 2 Mean (SD) 75.2 ( 8.59)
#> 3    Median         76.0
#> 4    Q1, Q3   69.2, 81.8
#> 5  Min, Max       52, 89
#> 6   Missing            0
identical(as.vector(recon$res1), as.vector(built$res1))
#> [1] TRUE

# Changing only the spec's formats re-renders the same numbers differently
respec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(
      format_strings = list("Mean (SD)" = f_str("xx.xx (xx.xxx)", "mean", "sd"))))
  )
)
tplyr_from_ard(ard, respec)[, c("rowlabel1", "res1")]
#>   rowlabel1           res1
#> 1 Mean (SD) 75.21 ( 8.590)
```
