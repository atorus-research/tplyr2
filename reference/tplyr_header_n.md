# Extract header N from a tplyr2 build result

Returns the population-based header N values that were computed during
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md).
Only available when population data was provided.

## Usage

``` r
tplyr_header_n(result)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)

## Value

A data.frame with column variable levels and their N values, or NULL if
no population data was used.
