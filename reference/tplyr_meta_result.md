# Get metadata for a specific output cell

Returns a `tplyr_meta` object containing the filter expressions that
describe the source data for the specified cell.

## Usage

``` r
tplyr_meta_result(result, row_id, column)
```

## Arguments

- result:

  A data.frame from
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
  built with `metadata = TRUE`

- row_id:

  Character row ID (from `result$row_id` or
  [`generate_row_ids()`](https://github.com/mstackhouse/tplyr2/reference/generate_row_ids.md))

- column:

  Character column name (e.g., `"res1"`)

## Value

A `tplyr_meta` object, or NULL if no metadata for that cell
