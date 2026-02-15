# Build a tplyr2 table from a spec and data

Executes the table specification against the provided data, producing a
formatted output data frame.

## Usage

``` r
tplyr_build(spec, data, pop_data = NULL, metadata = FALSE, ...)
```

## Arguments

- spec:

  A tplyr_spec object (or path to a JSON/YAML spec file)

- data:

  A data.frame to process

- pop_data:

  Optional population data.frame (overrides spec pop_data)

- metadata:

  If TRUE, attach cell-level metadata enabling traceability back to
  source data rows via
  [`tplyr_meta_result()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta_result.md)
  and
  [`tplyr_meta_subset()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta_subset.md).

- ...:

  Additional named arguments to override spec-level parameters

## Value

A data.frame with rowlabel, res, and ord columns
