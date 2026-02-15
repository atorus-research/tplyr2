# Get source data rows for a specific output cell

Evaluates the stored filter expressions against the original data to
return the rows that contributed to the specified output cell.

## Usage

``` r
tplyr_meta_subset(result, row_id, column, data, pop_data = NULL)
```

## Arguments

- result:

  A data.frame from
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
  built with `metadata = TRUE`

- row_id:

  Character row ID

- column:

  Character column name (e.g., `"res1"`)

- data:

  The original data.frame that was passed to
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)

- pop_data:

  Optional population data.frame, required when the cell represents a
  missing subjects row (anti-join)

## Value

A data.frame subset of the original data, or NULL if no metadata
