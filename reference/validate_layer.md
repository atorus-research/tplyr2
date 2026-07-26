# Validate a single layer

Validate a single layer

## Usage

``` r
validate_layer(layer, index, cols = NULL)
```

## Arguments

- layer:

  A tplyr_layer object

- index:

  Integer layer index (for error messages)

- cols:

  Character vector of spec column variables (for cross-checks such as
  pairwise assoc_test); may be NULL when validating a layer in
  isolation.

## Value

Invisible TRUE if valid
