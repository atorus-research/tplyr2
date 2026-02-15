# Metadata object for a tplyr output cell

Contains filter expressions that, when evaluated against the original
data, reproduce the subset of rows that contributed to a specific cell
in the output table.

## Usage

``` r
tplyr_meta(
  names = character(0),
  filters = list(),
  layer_index = integer(0),
  anti_join = NULL
)
```

## Arguments

- names:

  Character vector of variable names relevant to this cell

- filters:

  List of R language objects (call expressions) representing filter
  conditions

- layer_index:

  Integer layer index (1-based)

- anti_join:

  NULL or a `tplyr_meta_anti_join` object for missing subjects rows

## Value

A tplyr_meta object
