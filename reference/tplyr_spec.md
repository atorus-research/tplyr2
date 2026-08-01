# Create a tplyr2 table specification

The spec is a pure configuration object describing what to compute. No
data processing occurs until
[`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)
is called.

## Usage

``` r
tplyr_spec(
  cols,
  where = NULL,
  pop_data = NULL,
  total_groups = NULL,
  custom_groups = NULL,
  layers = tplyr_layers(),
  settings = NULL
)
```

## Arguments

- cols:

  Character vector of column variable names

- where:

  Expression for global data filter (optional)

- pop_data:

  A pop_data() object for population-based features (optional)

- total_groups:

  List of total_group() objects (optional)

- custom_groups:

  List of custom_group() objects (optional)

- layers:

  A list of layer objects from tplyr_layers()

- settings:

  Additional spec-level settings (optional)

## Value

A tplyr_spec object

## See also

[`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)
to execute a spec,
[`tplyr_layers()`](https://atorus-research.github.io/tplyr2/reference/tplyr_layers.md)
to assemble layers, and
[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md)
for per-layer configuration.

## Examples

``` r
# A spec is inert configuration -- nothing is computed until tplyr_build()
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("AGEGR1"),
    group_desc("AGE")
  )
)
spec
#> tplyr2 table specification
#> Column variables: TRT01PLayers: 2[1] count: AGEGR1 (Layer 1)[2] desc: AGE (Layer 2)

tplyr_build(spec, tplyr_adsl)
#>   rowlabel1         res1         res2         res3 ord_layer_1 ord_layer_index
#> 1       <65   14 (16.3%)   11 (13.1%)    8 ( 9.5%)           1               1
#> 2     65-80   42 (48.8%)   55 (65.5%)   47 (56.0%)           2               1
#> 3       >80   30 (34.9%)   18 (21.4%)   29 (34.5%)           3               1
#> 4         n           86           84           84           1               2
#> 5 Mean (SD) 75.2 ( 8.59) 74.4 ( 7.89) 75.7 ( 8.29)           2               2
#> 6    Median         76.0         76.0         77.5           3               2
#> 7    Q1, Q3   69.2, 81.8   70.8, 80.0   71.0, 82.0           4               2
#> 8  Min, Max       52, 89       56, 88       51, 88           5               2
#> 9   Missing            0            0            0           6               2

# A global `where` filter applies to every layer
safety <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(group_count("SEX"))
)
tplyr_build(safety, tplyr_adsl)
#>   rowlabel1       res1       res2       res3 ord_layer_1 ord_layer_index
#> 1         F 53 (61.6%) 40 (47.6%) 50 (59.5%)           1               1
#> 2         M 33 (38.4%) 44 (52.4%) 34 (40.5%)           2               1
```
