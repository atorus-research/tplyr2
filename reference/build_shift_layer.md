# Process a shift layer

Builds a cross-tabulation (row variable × column variable) within each
treatment arm. The shift column variable becomes an additional column
dimension, producing result columns like "Placebo_H", "Placebo_N", etc.

## Usage

``` r
build_shift_layer(dt, layer, cols, layer_index, col_n = NULL, pop_dt = NULL)
```

## Arguments

- dt:

  data.table with the (filtered) input data

- layer:

  A tplyr_shift_layer object

- cols:

  Character vector of column variable names from the spec

- layer_index:

  Integer index of this layer

## Value

A data.table with rowlabel\*, res\*, and ord\* columns
