# Apply row masks to blank repeated row labels

Walks each `rowlabel*` column top-to-bottom and blanks values that are
identical to the previous row, respecting layer boundaries
(`ord_layer_index`).

## Usage

``` r
apply_row_masks(result, row_breaks = FALSE)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)

- row_breaks:

  Logical. If TRUE, insert a blank row between layers.

## Value

A data.frame with repeated labels blanked
