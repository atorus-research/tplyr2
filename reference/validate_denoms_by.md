# Validate denoms_by against the layer's grouping variables

Most denominator merges join on `intersect(denom_group, names(x))`. A
`denoms_by` naming a variable that is not one of the layer's grouping
columns silently shrinks the join-key set, leaving the denominator table
with several rows per remaining key — so the merge either multiplies
table rows or attaches another group's denominator, with no error.
Pinning the invariant here makes every one of those intersects provably
a no-op.

## Usage

``` r
validate_denoms_by(layer, index, cols, dt_names)
```

## Arguments

- layer:

  A tplyr_layer object

- index:

  Integer layer index

- cols:

  Character vector of spec-level column variables

- dt_names:

  Column names of the build data

## Value

Invisible TRUE
