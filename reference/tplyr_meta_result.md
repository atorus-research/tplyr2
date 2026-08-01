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
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)
  built with `metadata = TRUE`

- row_id:

  Character row ID (from `result$row_id` or
  [`generate_row_ids()`](https://atorus-research.github.io/tplyr2/reference/generate_row_ids.md))

- column:

  Character column name (e.g., `"res1"`)

## Value

A `tplyr_meta` object, or NULL if no metadata for that cell

## See also

[`tplyr_meta_subset()`](https://atorus-research.github.io/tplyr2/reference/tplyr_meta_subset.md)
to fetch the source rows themselves.

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("AGEGR1"))
)
built <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
built[, c("row_id", "rowlabel1", "res1")]
#>    row_id rowlabel1       res1
#> 1   1_<65       <65 14 (16.3%)
#> 2 1_65-80     65-80 42 (48.8%)
#> 3   1_>80       >80 30 (34.9%)

# The filters behind the Placebo / 65-80 cell
tplyr_meta_result(built, built$row_id[1], "res1")
#> tplyr_meta [layer 1]
#>   Names: TRT01P, AGEGR1
#>   Filters:
#>     TRT01P == "Placebo"
#>     AGEGR1 == "<65"
```
