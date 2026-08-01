# Create a custom column group configuration

Combines existing column levels into a custom group. Rows matching any
of the source levels are duplicated with the column variable set to the
group name.

## Usage

``` r
custom_group(col_var, ...)
```

## Arguments

- col_var:

  Character string naming the column variable

- ...:

  Named arguments where names are group labels and values are character
  vectors of source levels to combine. Example:
  `"High Dose" = c("Dose 1", "Dose 2")`

## Value

A tplyr_custom_group object

## Examples

``` r
# Pool the two dose arms into one "Xanomeline (All)" column, kept alongside
# the arms it is built from
spec <- tplyr_spec(
  cols = "TRT01P",
  custom_groups = list(custom_group(
    "TRT01P",
    "Xanomeline (All)" = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )),
  layers = tplyr_layers(group_count("AGEGR1"))
)
tplyr_build(spec, tplyr_adsl)
#>   rowlabel1       res1        res2       res3       res4 ord_layer_1
#> 1       <65 14 (16.3%)  19 (11.3%) 11 (13.1%)  8 ( 9.5%)           1
#> 2     65-80 42 (48.8%) 102 (60.7%) 55 (65.5%) 47 (56.0%)           2
#> 3       >80 30 (34.9%)  47 (28.0%) 18 (21.4%) 29 (34.5%)           3
#>   ord_layer_index
#> 1               1
#> 2               1
#> 3               1

# Several groups at once
custom_group(
  "TRT01P",
  "Active"  = c("Xanomeline High Dose", "Xanomeline Low Dose"),
  "Control" = "Placebo"
)
#> tplyr2 custom group on TRT01P"Active" = [Xanomeline High Dose, Xanomeline Low Dose]"Control" = [Placebo]
```
