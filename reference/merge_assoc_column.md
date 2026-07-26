# Attach an association-test result column to a wide layer result

Adds a `pval1` column carrying the formatted per-by-group result, placed
on the first output row of each by group (blank elsewhere), with
`config$label` as its `label` attribute.

## Usage

``` r
merge_assoc_column(wide, assoc, by_rl_cols, by_data_vars, config)
```

## Arguments

- wide:

  data.table layer result (with rowlabel/res/ord columns).

- assoc:

  data.table from
  [`compute_assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/compute_assoc_test.md).

- by_rl_cols:

  Character vector of the rowlabel columns holding the by variable
  values (in by-variable order); empty when the layer has no by.

- by_data_vars:

  Character vector of by data-variable names.

- config:

  A `tplyr_assoc_test` object.
