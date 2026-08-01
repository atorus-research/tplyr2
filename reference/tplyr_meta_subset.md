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
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)
  built with `metadata = TRUE`

- row_id:

  Character row ID

- column:

  Character column name (e.g., `"res1"`)

- data:

  The original data.frame that was passed to
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)

- pop_data:

  Optional population data.frame, required when the cell represents a
  missing subjects row (anti-join)

## Value

A data.frame subset of the original data, or NULL if no metadata

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

# Trace the first cell back to the subjects it counted
src <- tplyr_meta_subset(built, built$row_id[1], "res1", tplyr_adsl)
nrow(src)
#> [1] 14
head(src[, c("USUBJID", "TRT01P", "AGEGR1")])
#>       USUBJID  TRT01P AGEGR1
#> 1 01-701-1015 Placebo    <65
#> 2 01-701-1023 Placebo    <65
#> 3 01-701-1118 Placebo    <65
#> 4 01-701-1345 Placebo    <65
#> 5 01-703-1042 Placebo    <65
#> 6 01-706-1041 Placebo    <65

# The row count matches the number displayed in the cell
built$res1[1]
#> [1] "14 (16.3%)"
```
