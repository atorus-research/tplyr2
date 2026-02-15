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
