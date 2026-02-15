# Create a tplyr2 table specification

The spec is a pure configuration object describing what to compute. No
data processing occurs until
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
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
