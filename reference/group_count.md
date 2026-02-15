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
  [`label()`](https://github.com/mstackhouse/tplyr2/reference/label.md)
  for explicit disambiguation.

- where:

  Expression for filtering data for this layer

- settings:

  A layer_settings object

## Value

A tplyr_count_layer object
