# Retrieve raw numeric data from a tplyr_build result

Returns the unformatted numeric data that was computed during the build
process, before formatting and pivoting to wide format.

## Usage

``` r
tplyr_numeric_data(result, layer = NULL)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)

- layer:

  Integer layer index (1-based), or NULL for all layers

## Value

If `layer` is specified, a data.frame of raw statistics for that layer.
If `layer` is NULL, a named list of data.frames keyed by layer index.
Returns NULL if numeric data was not retained.
