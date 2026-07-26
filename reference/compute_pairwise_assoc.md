# Compute pairwise per-level association-test p-values from a counts table

For each comparison arm and each target-variable level, builds a 2x2
contingency matrix from the assembled cell counts and population
denominators and calls `config$fn` to obtain a scalar p-value. This
mirrors
[`compute_risk_diff()`](https://github.com/mstackhouse/tplyr2/reference/compute_risk_diff.md)
in placement (a value per target level per comparison) but delegates the
test to the caller-supplied function.

## Usage

``` r
compute_pairwise_assoc(
  counts_long,
  cols,
  tv,
  by_data_vars,
  distinct_by,
  config,
  reference
)
```

## Arguments

- counts_long:

  data.table (pre-formatting) with the column variable, any `by`
  variables, the target variable, and `n`/`total` (plus
  `distinct_n`/`distinct_total` when distinct counting).

- cols:

  Character vector of column variable names from the spec.

- tv:

  Character string naming the target variable.

- by_data_vars:

  Character vector of by-variable names.

- distinct_by:

  Distinct-by variable name (or NULL); selects the distinct
  counts/denominators when non-NULL.

- config:

  A `tplyr_assoc_test` object (pairwise mode).

- reference:

  Character(1) resolved reference arm level.

## Value

A data.table with one row per target level per comparison, holding the
row variables, `.comp_idx`, and the scalar p-value `p`.
