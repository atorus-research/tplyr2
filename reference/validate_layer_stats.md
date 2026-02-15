# Validate that format string vars are valid stats for the layer type

Issues warnings (not errors) for unrecognized statistic names, since
custom summaries can add arbitrary stat names.

## Usage

``` r
validate_layer_stats(layer, index)
```

## Arguments

- layer:

  A tplyr_layer object

- index:

  Integer layer index

## Value

Invisible TRUE
