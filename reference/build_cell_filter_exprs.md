# Build filter expressions for a single output cell

Constructs the complete set of filter expressions for one cell in the
output table by inspecting the row's label values, the column label, and
the layer/spec configuration.

## Usage

``` r
build_cell_filter_exprs(
  output,
  row_idx,
  rc,
  layer,
  layer_idx,
  cols,
  col_level_map,
  var_to_rl,
  by_data_vars,
  spec,
  pop_col_map = NULL
)
```

## Arguments

- output:

  The output data.frame

- row_idx:

  Row index in the output

- rc:

  Result column name (e.g., "res1")

- layer:

  A tplyr_layer object

- layer_idx:

  Integer layer index

- cols:

  Character vector of spec-level column variables

- col_level_map:

  Named list: res column -\> column variable level string

- var_to_rl:

  Named list mapping data variables to rowlabel columns

- by_data_vars:

  Character vector of by-variable data column names

- spec:

  The tplyr_spec object

## Value

A tplyr_meta object
