# Attach pairwise association-test columns to a nested wide layer result

Places each comparison's display string on every matching output row by
an exact join on the `rowlabel*` columns (which uniquely identify a wide
row across all nesting levels). Rows with no computed value – special
rows such as Missing, or a Total row when `total_row = FALSE` – stay
blank.

## Usage

``` r
merge_pairwise_assoc_nested(
  wide,
  assoc_data,
  config,
  row_label_cols,
  reference
)
```

## Arguments

- wide:

  data.table in wide format (after
  [`cast_to_wide()`](https://atorus-research.github.io/tplyr2/reference/cast_to_wide.md)).

- assoc_data:

  data.table from
  [`compute_pairwise_assoc_nested()`](https://atorus-research.github.io/tplyr2/reference/compute_pairwise_assoc_nested.md).

- config:

  A `tplyr_assoc_test` object (pairwise mode).

- row_label_cols:

  Character vector of the `rowlabel*` column names.

- reference:

  Character(1) resolved reference arm level.
