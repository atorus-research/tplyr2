# Compute the association-test result per by-group

Runs `config$fn` once per `by` group over the source-data subset for
that group, and returns the formatted scalar keyed by the by variables.

## Usage

``` r
compute_assoc_test(source_dt, by_data_vars, config)
```

## Arguments

- source_dt:

  data.table of source rows for the layer (after any layer `where`),
  holding the `cols`, `by`, and target/row variables.

- by_data_vars:

  Character vector of by data-variable names (may be empty).

- config:

  A `tplyr_assoc_test` object.

## Value

A data.table with the `by_data_vars` columns (as character) plus a
formatted character column `.assoc_p`. When `by_data_vars` is empty, a
single-row table with only `.assoc_p`.
