# Process a custom analysis layer

Calls the user-provided `analyze_fn` for each group combination and
formats results using either `format_strings` or a pre-formatted column.

## Usage

``` r
build_analyze_layer(dt, layer, cols, layer_index, col_n = NULL, pop_dt = NULL)
```

## Arguments

- dt:

  data.table with the (filtered) input data

- layer:

  A tplyr_analyze_layer object

- cols:

  Character vector of column variable names from the spec

- layer_index:

  Integer index of this layer

- col_n:

  data.table with column counts (or NULL)

- pop_dt:

  data.table with population data (or NULL)

## Value

A data.table with rowlabel\*, res\*, and ord\* columns
