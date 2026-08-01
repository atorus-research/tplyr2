# Apply row masks to blank repeated row labels

Walks each `rowlabel*` column top-to-bottom and blanks values that are
identical to the previous row, respecting layer boundaries
(`ord_layer_index`).

## Usage

``` r
apply_row_masks(result, row_breaks = FALSE)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)

- row_breaks:

  Logical. If TRUE, insert a blank row between layers.

## Value

A data.frame with repeated labels blanked

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("AGEGR1", by = "SEX"))
)
built <- tplyr_build(spec, tplyr_adsl)

# SEX repeats on every age-group row
built[, c("rowlabel1", "rowlabel2")]
#>   rowlabel1 rowlabel2
#> 1         F       <65
#> 2         F     65-80
#> 3         F       >80
#> 4         M       <65
#> 5         M     65-80
#> 6         M       >80

# Masked: each value prints once, at the top of its block
apply_row_masks(built)[, c("rowlabel1", "rowlabel2")]
#>   rowlabel1 rowlabel2
#> 1         F       <65
#> 2               65-80
#> 3                 >80
#> 4         M       <65
#> 5               65-80
#> 6                 >80

# row_breaks inserts a blank row between layers
two <- tplyr_build(
  tplyr_spec(cols = "TRT01P",
             layers = tplyr_layers(group_count("SEX"), group_count("AGEGR1"))),
  tplyr_adsl
)
apply_row_masks(two, row_breaks = TRUE)[, c("rowlabel1", "res1")]
#>   rowlabel1       res1
#> 1         F 53 (61.6%)
#> 2         M 33 (38.4%)
#> 3                     
#> 4       <65 14 (16.3%)
#> 5     65-80 42 (48.8%)
#> 6       >80 30 (34.9%)
```
