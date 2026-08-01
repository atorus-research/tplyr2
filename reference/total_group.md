# Create a total group configuration

Specifies that a synthetic "Total" column level should be added by
duplicating all rows with the specified column variable set to the
label.

## Usage

``` r
total_group(col_var, label = "Total")
```

## Arguments

- col_var:

  Character string naming the column variable to totalize

- label:

  Character string for the total group label (default: "Total")

## Value

A tplyr_total_group object

## Examples

``` r
# Adds a "Total" column spanning every arm, alongside the individual arms
spec <- tplyr_spec(
  cols = "TRT01P",
  total_groups = list(total_group("TRT01P")),
  layers = tplyr_layers(group_count("AGEGR1"))
)
tplyr_build(spec, tplyr_adsl)
#>   rowlabel1       res1        res2       res3       res4 ord_layer_1
#> 1       <65 14 (16.3%)  33 (13.0%) 11 (13.1%)  8 ( 9.5%)           1
#> 2     65-80 42 (48.8%) 144 (56.7%) 55 (65.5%) 47 (56.0%)           2
#> 3       >80 30 (34.9%)  77 (30.3%) 18 (21.4%) 29 (34.5%)           3
#>   ord_layer_index
#> 1               1
#> 2               1
#> 3               1

# Rename the total column
all_pts <- tplyr_spec(
  cols = "TRT01P",
  total_groups = list(total_group("TRT01P", label = "All Patients")),
  layers = tplyr_layers(group_count("SEX"))
)
tplyr_build(all_pts, tplyr_adsl)
#>   rowlabel1        res1       res2       res3       res4 ord_layer_1
#> 1         F 143 (56.3%) 53 (61.6%) 40 (47.6%) 50 (59.5%)           1
#> 2         M 111 (43.7%) 33 (38.4%) 44 (52.4%) 34 (40.5%)           2
#>   ord_layer_index
#> 1               1
#> 2               1
```
