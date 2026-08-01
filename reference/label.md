# Create a text label for use in by parameters

Explicitly marks a string as a text label (not a data variable name).
Useful when a label string might coincidentally match a column name.

## Usage

``` r
label(x)
```

## Arguments

- x:

  Character string to use as a label

## Value

A tplyr_label object

## Examples

``` r
# A `by` string that matches no column is already treated as a label, but
# label() is explicit -- and necessary when the text matches a column name.
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("AGEGR1", by = label("Age Group (y)"))
  )
)
head(tplyr_build(spec, tplyr_adsl))
#>       rowlabel1 rowlabel2       res1       res2       res3 ord_layer_1
#> 1 Age Group (y)       <65 14 (16.3%) 11 (13.1%)  8 ( 9.5%)           1
#> 2 Age Group (y)     65-80 42 (48.8%) 55 (65.5%) 47 (56.0%)           2
#> 3 Age Group (y)       >80 30 (34.9%) 18 (21.4%) 29 (34.5%)           3
#>   ord_layer_index
#> 1               1
#> 2               1
#> 3               1
```
