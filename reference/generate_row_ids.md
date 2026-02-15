# Generate unique row IDs for output rows

Creates a character ID for each row by combining the layer index and row
label values. These IDs can be used with
[`tplyr_meta_result()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta_result.md)
and
[`tplyr_meta_subset()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta_subset.md)
to look up cell metadata.

## Usage

``` r
generate_row_ids(result)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)

## Value

Character vector of row IDs (same length as `nrow(result)`)
