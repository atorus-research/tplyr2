# Metadata object for a tplyr output cell

Contains filter expressions that, when evaluated against the original
data, reproduce the subset of rows that contributed to a specific cell
in the output table.

## Usage

``` r
tplyr_meta(
  names = character(0),
  filters = list(),
  layer_index = integer(0),
  anti_join = NULL,
  statistic = NULL
)
```

## Arguments

- names:

  Character vector of variable names relevant to this cell

- filters:

  List of R language objects (call expressions) representing filter
  conditions

- layer_index:

  Integer layer index (1-based)

- anti_join:

  NULL or a `tplyr_meta_anti_join` object for missing subjects rows

- statistic:

  NULL or a character string naming which statistic the cell displays
  (set for `stat_columns` layers, where the stat sub-columns of a column
  group share the same source-data filters)

## Value

A tplyr_meta object

## See also

[`tplyr_meta_result()`](https://atorus-research.github.io/tplyr2/reference/tplyr_meta_result.md)
to retrieve one from a build.

## Examples

``` r
# Usually obtained from a build rather than constructed by hand
m <- tplyr_meta(
  names = c("TRT01P", "AGEGR1"),
  filters = list(quote(TRT01P == "Placebo"), quote(AGEGR1 == "65-80")),
  layer_index = 1L
)
m
#> tplyr_meta [layer 1]
#>   Names: TRT01P, AGEGR1
#>   Filters:
#>     TRT01P == "Placebo"
#>     AGEGR1 == "65-80"

# The filters are ordinary language objects, so they can be applied directly
subset(tplyr_adsl, TRT01P == "Placebo" & AGEGR1 == "65-80")[1:3, c("USUBJID", "AGEGR1")]
#> # A tibble: 3 × 2
#>   USUBJID     AGEGR1
#>   <chr>       <chr> 
#> 1 01-701-1153 65-80 
#> 2 01-701-1234 65-80 
#> 3 01-701-1392 65-80 
```
