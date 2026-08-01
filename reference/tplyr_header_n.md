# Extract header N from a tplyr2 build result

Returns the population-based header N values that were computed during
[`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md).
Only available when population data was provided.

## Usage

``` r
tplyr_header_n(result)
```

## Arguments

- result:

  A data.frame produced by
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)

## Value

A data.frame with column variable levels and their N values, or NULL if
no population data was used.

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
  layers = tplyr_layers(group_count("AEBODSYS"))
)
built <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
tplyr_header_n(built)
#>                   TRTA .n
#> 1              Placebo 86
#> 2 Xanomeline High Dose 84
#> 3  Xanomeline Low Dose 84

# NULL when the build had no population data to draw an N from
no_pop <- tplyr_build(
  tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("SEX"))),
  tplyr_adsl
)
tplyr_header_n(no_pop)
#> NULL
```
