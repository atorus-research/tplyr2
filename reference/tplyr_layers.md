# Create a list of layers

Wraps one or more layer objects into a validated list for use in
tplyr_spec().

## Usage

``` r
tplyr_layers(...)
```

## Arguments

- ...:

  Layer objects created by group_count(), group_desc(), group_shift(),
  or group_analyze()

## Value

A list of tplyr_layer objects

## Examples

``` r
# Layers stack in the order given, and may mix types freely
layers <- tplyr_layers(
  group_desc("AGE"),
  group_count("SEX"),
  group_count("AGEGR1")
)
length(layers)
#> [1] 3

spec <- tplyr_spec(cols = "TRT01P", layers = layers)
tplyr_build(spec, tplyr_adsl)
#>    rowlabel1         res1         res2         res3 ord_layer_1 ord_layer_index
#> 1          n           86           84           84           1               1
#> 2  Mean (SD) 75.2 ( 8.59) 74.4 ( 7.89) 75.7 ( 8.29)           2               1
#> 3     Median         76.0         76.0         77.5           3               1
#> 4     Q1, Q3   69.2, 81.8   70.8, 80.0   71.0, 82.0           4               1
#> 5   Min, Max       52, 89       56, 88       51, 88           5               1
#> 6    Missing            0            0            0           6               1
#> 7          F   53 (61.6%)   40 (47.6%)   50 (59.5%)           1               2
#> 8          M   33 (38.4%)   44 (52.4%)   34 (40.5%)           2               2
#> 9        <65   14 (16.3%)   11 (13.1%)    8 ( 9.5%)           1               3
#> 10     65-80   42 (48.8%)   55 (65.5%)   47 (56.0%)           2               3
#> 11       >80   30 (34.9%)   18 (21.4%)   29 (34.5%)           3               3
```
