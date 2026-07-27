# Compute pairwise per-level association-test p-values for a nested layer

Like
[`compute_pairwise_assoc()`](https://github.com/mstackhouse/tplyr2/reference/compute_pairwise_assoc.md)
but keyed directly by the assembled `rowlabel*` columns rather than a
single target variable, so it works at every nesting level at once: each
inner (e.g. preferred-term) row and each outer (e.g. system-organ-class
subtotal) row is one `rowlabel` tuple, and its 2x2 is built from that
row's own reference/comparison counts and population denominators. The
same helper computes the grand-total row's p-value when passed the
total-row table (a single `rowlabel` tuple).

## Usage

``` r
compute_pairwise_assoc_nested(
  long,
  cols,
  row_label_cols,
  distinct_by,
  config,
  reference,
  arm_n = NULL
)
```

## Arguments

- long:

  data.table holding the column variable, the assembled `rowlabel*`
  columns, and the raw `n`/`total` (or `distinct_n`/`distinct_total`)
  statistics – the nested `combined` table (category rows) or a
  total-row table.

- cols:

  Character vector of column variable names from the spec.

- row_label_cols:

  Character vector of the `rowlabel*` column names that jointly identify
  an output row.

- distinct_by:

  Distinct-by variable name (or NULL); selects the distinct
  counts/denominators when non-NULL.

- config:

  A `tplyr_assoc_test` object (pairwise mode).

- reference:

  Character(1) resolved reference arm level.

- arm_n:

  Named numeric of population arm sizes (arm level -\> N), used to
  back-fill the 2x2 denominator for an arm that has no events on a row
  (or no events at all). Without it, a zero-event reference or
  comparison arm would have a missing denominator and blank the test;
  with it, an empty arm still yields a valid `0-vs-k` test (issue \#49,
  sparse-table fix).

## Value

A data.table with the `row_label_cols` (as character), `.comp_idx`, and
the display string `.disp`; one row per output row per comparison.
