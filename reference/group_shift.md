# Create a shift layer

Create a shift layer

## Usage

``` r
group_shift(target_var, by = NULL, where = NULL, settings = layer_settings())
```

## Arguments

- target_var:

  Named character vector with `row` and `column` elements

- by:

  Character string or vector for row grouping

- where:

  Expression for filtering data for this layer

- settings:

  A layer_settings object

## Value

A tplyr_shift_layer object

## Examples

``` r
# Baseline (rows) against post-baseline (columns) reference ranges
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_shift(c(row = "BNRIND", column = "ANRIND"))
  )
)
head(tplyr_build(spec, tplyr_adlb))
#>   rowlabel1        res1        res2        res3 ord_layer_1 ord_layer_index
#> 1         N 75 (100.0%) 78 (100.0%) 47 (100.0%)           1               1

# Percentages relative to each baseline (column) group rather than the arm
by_col <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_shift(c(row = "BNRIND", column = "ANRIND"),
                settings = layer_settings(shift_denom = "column"))
  )
)
head(tplyr_build(by_col, tplyr_adlb))
#>   rowlabel1        res1        res2        res3 ord_layer_1 ord_layer_index
#> 1         N 75 (100.0%) 78 (100.0%) 47 (100.0%)           1               1
```
