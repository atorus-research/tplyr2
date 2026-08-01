# Build a tplyr2 table from a spec and data

Executes the table specification against the provided data, producing a
formatted output data frame.

## Usage

``` r
tplyr_build(spec, data, pop_data = NULL, metadata = FALSE, ...)
```

## Arguments

- spec:

  A tplyr_spec object (or path to a JSON/YAML spec file)

- data:

  A data.frame to process

- pop_data:

  Optional population data.frame (overrides spec pop_data)

- metadata:

  If TRUE, attach cell-level metadata enabling traceability back to
  source data rows via
  [`tplyr_meta_result()`](https://atorus-research.github.io/tplyr2/reference/tplyr_meta_result.md)
  and
  [`tplyr_meta_subset()`](https://atorus-research.github.io/tplyr2/reference/tplyr_meta_subset.md).

- ...:

  Additional named arguments overriding spec-level parameters. Names
  must match a field of `spec` (or `where`/`pop_data`); an unrecognized
  name is an error rather than a silent no-op. Because `...` is
  evaluated eagerly, a `where` override must be a character string or a
  quoted expression, not the bare expression
  [`tplyr_spec()`](https://atorus-research.github.io/tplyr2/reference/tplyr_spec.md)
  accepts.

## Value

A data.frame with rowlabel, res, and ord columns

## See also

[`tplyr_spec()`](https://atorus-research.github.io/tplyr2/reference/tplyr_spec.md)
to build the specification, and
[`tplyr_numeric_data()`](https://atorus-research.github.io/tplyr2/reference/tplyr_numeric_data.md)
for the unformatted values behind the cells.

## Examples

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("AGEGR1"))
)
tplyr_build(spec, tplyr_adsl)
#>   rowlabel1       res1       res2       res3 ord_layer_1 ord_layer_index
#> 1       <65 14 (16.3%) 11 (13.1%)  8 ( 9.5%)           1               1
#> 2     65-80 42 (48.8%) 55 (65.5%) 47 (56.0%)           2               1
#> 3       >80 30 (34.9%) 18 (21.4%) 29 (34.5%)           3               1

# Override spec fields at build time without editing the spec. Overrides go
# through `...`, which evaluates eagerly, so a `where` must be a string or
# quoted -- not the bare expression tplyr_spec() accepts.
tplyr_build(spec, tplyr_adsl, where = "SEX == 'F'")
#>   rowlabel1       res1       res2       res3 ord_layer_1 ord_layer_index
#> 1       <65  9 (17.0%)  5 (12.5%)  5 (10.0%)           1               1
#> 2     65-80 22 (41.5%) 28 (70.0%) 28 (56.0%)           2               1
#> 3       >80 22 (41.5%)  7 (17.5%) 17 (34.0%)           3               1
tplyr_build(spec, tplyr_adsl, where = quote(SEX == "F"))
#>   rowlabel1       res1       res2       res3 ord_layer_1 ord_layer_index
#> 1       <65  9 (17.0%)  5 (12.5%)  5 (10.0%)           1               1
#> 2     65-80 22 (41.5%) 28 (70.0%) 28 (56.0%)           2               1
#> 3       >80 22 (41.5%)  7 (17.5%) 17 (34.0%)           3               1

# Population data supplies the denominators and the header N
pop_spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
  layers = tplyr_layers(
    group_count("AEBODSYS",
                settings = layer_settings(distinct_by = "USUBJID"))
  )
)
head(tplyr_build(pop_spec, tplyr_adae, pop_data = tplyr_adsl))
#>                                              rowlabel1       res1       res2
#> 1                                    CARDIAC DISORDERS  5 ( 5.8%)  6 ( 7.1%)
#> 2           CONGENITAL, FAMILIAL AND GENETIC DISORDERS  0 ( 0.0%)  1 ( 1.2%)
#> 3                           GASTROINTESTINAL DISORDERS  6 ( 7.0%)  6 ( 7.1%)
#> 4 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS 11 (12.8%) 21 (25.0%)
#> 5                              IMMUNE SYSTEM DISORDERS  0 ( 0.0%)  0 ( 0.0%)
#> 6                          INFECTIONS AND INFESTATIONS  5 ( 5.8%)  4 ( 4.8%)
#>         res3 ord_layer_1 ord_layer_index
#> 1  6 ( 7.1%)           1               1
#> 2  0 ( 0.0%)           2               1
#> 3  3 ( 3.6%)           3               1
#> 4 21 (25.0%)           4               1
#> 5  1 ( 1.2%)           5               1
#> 6  3 ( 3.6%)           6               1
```
