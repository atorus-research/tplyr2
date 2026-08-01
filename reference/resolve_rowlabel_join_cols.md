# Locate the rowlabel columns to join summary statistics back on

The inverse of
[`build_row_labels_long()`](https://github.com/mstackhouse/tplyr2/reference/build_row_labels_long.md)'s
layout: one rowlabel column per constant `by` label first, then one per
`by` data variable, then the target variable last. Callers that merge
per-group statistics (risk difference, pairwise p-values) onto the
assembled table need the data variable columns; assuming they start at
`rowlabel1` keys the join against a constant-label column whenever `by`
leads with a string label, matching nothing and leaving every cell
blank.

## Usage

``` r
resolve_rowlabel_join_cols(wide, by_labels, by_data_vars)
```

## Arguments

- wide:

  Assembled wide table

- by_labels:

  Character vector of constant `by` labels

- by_data_vars:

  Character vector of `by` data variable names

## Value

List with `tv_col` (the target variable's rowlabel column) and `by_cols`
(the by data variables' rowlabel columns, in `by_data_vars` order), or
NULL when `wide` has no rowlabel columns
