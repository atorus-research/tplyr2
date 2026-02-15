# Reconstruct a formatted table from ARD and a spec

Takes Analysis Results Data (long format) and a `tplyr_spec`, then
applies the spec's formatting rules to produce a formatted output table.

## Usage

``` r
tplyr_from_ard(ard, spec)
```

## Arguments

- ard:

  A data.frame in ARD format (as produced by
  [`tplyr_to_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_to_ard.md))

- spec:

  A `tplyr_spec` object defining the table structure

## Value

A data.frame with the same structure as
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
output
