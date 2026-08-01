# Create a count layer

Create a count layer

## Usage

``` r
group_count(target_var, by = NULL, where = NULL, settings = layer_settings())
```

## Arguments

- target_var:

  Character string or vector naming the target variable(s). Multiple
  variables create nested/hierarchical counts.

- by:

  Character string or vector for row grouping. Strings that don't match
  column names are treated as text labels. Use
  [`label()`](https://atorus-research.github.io/tplyr2/reference/label.md)
  for explicit disambiguation.

- where:

  Expression for filtering data for this layer

- settings:

  A layer_settings object

## Value

A tplyr_count_layer object

## See also

[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md)
for denominators, sorting, and special rows.

## Examples

``` r
# Counts of a categorical variable within each column group
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("AGEGR1"))
)
tplyr_build(spec, tplyr_adsl)
#>   rowlabel1       res1       res2       res3 ord_layer_1 ord_layer_index
#> 1       <65 14 (16.3%) 11 (13.1%)  8 ( 9.5%)           1               1
#> 2     65-80 42 (48.8%) 55 (65.5%) 47 (56.0%)           2               1
#> 3       >80 30 (34.9%) 18 (21.4%) 29 (34.5%)           3               1

# Distinct subject counts with a total row, filtered to serious events
ae <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEBODSYS",
      where = AESER == "Y",
      settings = layer_settings(distinct_by = "USUBJID", total_row = TRUE))
  )
)
head(tplyr_build(ae, tplyr_adae))
#> [1] rowlabel1       ord_layer_1     ord_layer_index
#> <0 rows> (or 0-length row.names)

# Two target variables nest: preferred term within body system
nested <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(group_count(c("AEBODSYS", "AEDECOD")))
)
head(tplyr_build(nested, tplyr_adae))
#>           rowlabel1                  rowlabel2       res1       res2       res3
#> 1 CARDIAC DISORDERS                             5 (10.6%)  6 ( 7.8%)  6 ( 7.9%)
#> 2 CARDIAC DISORDERS        ATRIAL FIBRILLATION  0 ( 0.0%)  0 ( 0.0%)  1 ( 1.3%)
#> 3 CARDIAC DISORDERS             ATRIAL FLUTTER  0 ( 0.0%)  1 ( 1.3%)  0 ( 0.0%)
#> 4 CARDIAC DISORDERS         ATRIAL HYPERTROPHY  1 ( 2.1%)  0 ( 0.0%)  0 ( 0.0%)
#> 5 CARDIAC DISORDERS  BUNDLE BRANCH BLOCK RIGHT  1 ( 2.1%)  0 ( 0.0%)  0 ( 0.0%)
#> 6 CARDIAC DISORDERS CARDIAC FAILURE CONGESTIVE  1 ( 2.1%)  0 ( 0.0%)  0 ( 0.0%)
#>   ord_layer_1 ord_layer_2 ord_layer_index
#> 1           1           1               1
#> 2           2           2               1
#> 3           3           2               1
#> 4           4           2               1
#> 5           5           2               1
#> 6           6           2               1
```
