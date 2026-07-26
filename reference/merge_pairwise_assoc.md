# Attach pairwise per-level association-test columns to a wide layer result

Appends one `pval<k>` column per comparison to the wide-format output,
each carrying the formatted p-value on *every* target-level row (blank
for special rows such as Total/Missing), with the per-comparison label
as the column's `label` attribute.

## Usage

``` r
merge_pairwise_assoc(
  wide,
  assoc_data,
  config,
  tv,
  by_data_vars,
  by_labels,
  reference
)
```

## Arguments

- wide:

  data.table in wide format (after
  [`cast_to_wide()`](https://github.com/mstackhouse/tplyr2/reference/cast_to_wide.md)).

- assoc_data:

  data.table from
  [`compute_pairwise_assoc()`](https://github.com/mstackhouse/tplyr2/reference/compute_pairwise_assoc.md).

- config:

  A `tplyr_assoc_test` object (pairwise mode).

- tv:

  Character string naming the target variable.

- by_data_vars:

  Character vector of by-variable names.

- by_labels:

  Character vector of by string-labels (non-data by entries).

- reference:

  Character(1) resolved reference arm level.
