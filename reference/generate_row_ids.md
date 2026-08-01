# Generate unique row IDs for output rows

Creates a character ID for each row by combining the layer index and row
label values. These IDs can be used with
[`tplyr_meta_result()`](https://atorus-research.github.io/tplyr2/reference/tplyr_meta_result.md)
and
[`tplyr_meta_subset()`](https://atorus-research.github.io/tplyr2/reference/tplyr_meta_subset.md)
to look up cell metadata.

## Usage

``` r
generate_row_ids(result)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)

## Value

Character vector of row IDs (same length as `nrow(result)`)

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("AGEGR1"))
)
built <- tplyr_build(spec, tplyr_adsl)
generate_row_ids(built)
#> [1] "1_<65"   "1_65-80" "1_>80"  

# IDs are derived from the row labels, so generate them from an unmodified
# build. tplyr_build(metadata = TRUE) attaches a row_id column that survives
# post-processing, which is the safer route.
with_meta <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
with_meta$row_id
#> [1] "1_<65"   "1_65-80" "1_>80"  
```
