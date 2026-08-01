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
  stat_labels = NULL,
  col_levels = NULL,
  row_order_col = NULL
)
```

## Arguments

- stat_labels:

  Character vector of stat column labels (the names of the
  `stat_columns` setting), or NULL for the standard single-format cast

- col_levels:

  Named list mapping factor column variables to their level order (from
  [`get_col_levels()`](https://github.com/mstackhouse/tplyr2/reference/get_col_levels.md));
  orders the resulting `res*` columns by factor levels instead of
  alphabetically. NULL leaves dcast's default alphabetical column order.

- row_order_col:

  Name of a numeric column in `dt` giving the intended row order. dcast
  sorts its LHS alphabetically, so a caller whose row labels are not
  alphabetical (e.g. format-string names) must carry the order through
  the cast; the column joins the LHS, sorts the result, and is then
  dropped.
