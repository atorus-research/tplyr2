# Write rowlabel columns onto a long-format layer table

Writes one `rowlabel<n>` column per `by` label constant, then one per
`by` data variable (as character), then the value variable (as
character).

## Usage

``` r
build_row_labels_long(dt, by_labels, by_data_vars, value_var)
```

## Arguments

- dt:

  data.table to modify by reference

- by_labels:

  Character vector of constant `by` labels

- by_data_vars:

  Character vector of `by` data variable names

- value_var:

  Name of the variable supplying the final rowlabel column. ARD
  reconstruction can encounter a target variable absent from the stats
  table; the column is skipped (but still named) in that case.

## Value

Character vector of the rowlabel column names
