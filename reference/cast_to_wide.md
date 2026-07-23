# Cast long data to wide output format

When `stat_labels` is provided (stat_columns mode), the long data
carries one `formatted_<i>` column per statistic and each column group
spreads into one res column per statistic, interleaved
column-group-major. Column labels follow the pattern
`"<column group> (N=n) | <stat label>"` so renderers can span the column
group over its stat sub-columns.

## Usage

``` r
cast_to_wide(
  dt,
  row_label_cols,
  cols,
  layer_index,
  col_n = NULL,
  stat_labels = NULL
)
```

## Arguments

- stat_labels:

  Character vector of stat column labels (the names of the
  `stat_columns` setting), or NULL for the standard single-format cast
