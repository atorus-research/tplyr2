# Process a descriptive statistics layer

Process a descriptive statistics layer

## Usage

``` r
build_desc_layer(dt, layer, cols, layer_index, col_n = NULL, pop_dt = NULL)
```

## Arguments

- dt:

  data.table with the (filtered) input data

- layer:

  A tplyr_desc_layer object

- cols:

  Character vector of column variable names from the spec

- layer_index:

  Integer index of this layer

## Value

A data.table with rowlabel\*, res\*, and ord\* columns
