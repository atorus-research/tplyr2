# Build cell-level metadata for the full output table

For each output row x result column combination, constructs filter
expressions that describe the source data subset for that cell. The
expressions can be evaluated at query time against the original data.

## Usage

``` r
build_cell_metadata(output, spec, col_names, pop_col_map = NULL)
```

## Arguments

- output:

  data.frame output from tplyr_build (with rowlabel/res/ord cols)

- spec:

  tplyr_spec object

- col_names:

  Character vector of original data column names

## Value

Named list of tplyr_meta objects, keyed by "row_id\|\|column"
