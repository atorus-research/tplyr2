# Create a descriptive statistics layer

Create a descriptive statistics layer

## Usage

``` r
group_desc(target_var, by = NULL, where = NULL, settings = layer_settings())
```

## Arguments

- target_var:

  Character string or vector naming the target variable(s)

- by:

  Character string or vector for row grouping

- where:

  Expression for filtering data for this layer

- settings:

  A layer_settings object

## Value

A tplyr_desc_layer object

## See also

[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md)
for auto-precision and custom summaries.

## Examples

``` r
# Default summary: n, Mean (SD), Median, Q1/Q3, Min/Max, Missing
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_desc("AGE"))
)
tplyr_build(spec, tplyr_adsl)
#>   rowlabel1         res1         res2         res3 ord_layer_1 ord_layer_index
#> 1         n           86           84           84           1               1
#> 2 Mean (SD) 75.2 ( 8.59) 74.4 ( 7.89) 75.7 ( 8.29)           2               1
#> 3    Median         76.0         76.0         77.5           3               1
#> 4    Q1, Q3   69.2, 81.8   70.8, 80.0   71.0, 82.0           4               1
#> 5  Min, Max       52, 89       56, 88       51, 88           5               1
#> 6   Missing            0            0            0           6               1

# Choose the statistics and their formats
custom <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(
      format_strings = list(
        "n"         = f_str("xx", "n"),
        "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
      )))
  )
)
tplyr_build(custom, tplyr_adsl)
#>   rowlabel1         res1         res2         res3 ord_layer_1 ord_layer_index
#> 1         n           86           84           84           1               1
#> 2 Mean (SD) 75.2 ( 8.59) 74.4 ( 7.89) 75.7 ( 8.29)           2               1

# Several target variables in one layer, grouped by visit
multi <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(group_desc(c("AVAL", "CHG"), by = "AVISIT"))
)
head(tplyr_build(multi, tplyr_adlb))
#>   rowlabel1 rowlabel2 rowlabel3 res1 res2 res3 ord_layer_1 ord_layer_2
#> 1  Baseline      AVAL         n    8   10    7         101           1
#> 2    Week 2      AVAL         n    8    9    7         101           2
#> 3    Week 4      AVAL         n    8    8    6         101           3
#> 4    Week 6      AVAL         n    6    8    5         101           4
#> 5    Week 8      AVAL         n    6    6    4         101           5
#> 6   Week 12      AVAL         n    6    6    2         101           6
#>   ord_layer_index
#> 1               1
#> 2               1
#> 3               1
#> 4               1
#> 5               1
#> 6               1
```
