# Warn when a nonzero count has a missing or zero denominator

Denominators are attached with a left join, so a group present in the
analysis data but absent from the denominator source comes back with
`total = NA` and renders a blank-width percent (`" 5 ( )"`) — and
`total == 0` with `n > 0` used to display an affirmatively wrong `0.0%`.
Both mean the denominator setup is wrong, so say so.

## Usage

``` r
check_denominator_integrity(counts, n_col, total_col, group_cols, layer_index)
```

## Arguments

- counts:

  data.table holding the counts and their denominators

- n_col:

  Name of the count column (`"n"` or `"distinct_n"`)

- total_col:

  Name of the denominator column

- group_cols:

  Columns identifying a row, used to name offending groups

- layer_index:

  Integer layer index

## Value

Invisible TRUE
