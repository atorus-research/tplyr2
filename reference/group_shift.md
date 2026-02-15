# Create a shift layer

Create a shift layer

## Usage

``` r
group_shift(target_var, by = NULL, where = NULL, settings = layer_settings())
```

## Arguments

- target_var:

  Named character vector with `row` and `column` elements

- by:

  Character string or vector for row grouping

- where:

  Expression for filtering data for this layer

- settings:

  A layer_settings object

## Value

A tplyr_shift_layer object
