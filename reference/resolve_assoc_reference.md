# Resolve the reference arm level for a pairwise association test

Returns `config$reference` when supplied, otherwise the first level of
the first `cols` variable at build time (factor level order when the
variable is a factor, else first value in appearance order).

## Usage

``` r
resolve_assoc_reference(config, dt, cols)
```

## Arguments

- config:

  A `tplyr_assoc_test` object.

- dt:

  data.table of source rows for the layer.

- cols:

  Character vector of column variable names from the spec.

## Value

Character(1) reference level.
