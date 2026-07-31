# Combine pinned column levels with a layer's own

The spec-level level set (every column-variable value in the table's
data) takes precedence so a layer whose `where` empties a column group
still emits that column. Any additional variable the layer knows about
(a shift layer's own column variable) is carried through.

## Usage

``` r
merge_col_levels(pinned, layer_levels)
```

## Arguments

- pinned:

  Named list of levels captured before layer filtering (or NULL)

- layer_levels:

  Named list from
  [`get_col_levels()`](https://github.com/mstackhouse/tplyr2/reference/get_col_levels.md)
  on the layer data

## Value

Named list of levels
