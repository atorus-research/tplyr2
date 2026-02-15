# Classify an output row as normal, total, missing, or missing_subjects

Classify an output row as normal, total, missing, or missing_subjects

## Usage

``` r
classify_row_type(output, row_idx, layer, var_to_rl)
```

## Arguments

- output:

  The output data.frame

- row_idx:

  Row index in the output

- layer:

  A tplyr_layer object

- var_to_rl:

  Named list mapping data variables to rowlabel columns

## Value

Character string: "normal", "total", "missing", or "missing_subjects"
