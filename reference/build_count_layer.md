# Process a count layer

Process a count layer

## Usage

``` r
build_count_layer(dt, layer, cols, layer_index, col_n = NULL, pop_dt = NULL)
```

## Arguments

- dt:

  data.table with the (filtered) input data

- layer:

  A tplyr_count_layer object

- cols:

  Character vector of column variable names from the spec

- layer_index:

  Integer index of this layer

## Value

A data.table with rowlabel\*, res\*, and ord\* columns
