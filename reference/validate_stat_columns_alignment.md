# Validate stat_columns consistency across layers

Layers using stat_columns emit one res column per statistic per column
group, while other layers emit one per column group.
harmonize_and_bind() aligns layers positionally by res column name, so
mixing the two shapes in one spec would silently place results under the
wrong column labels.

## Usage

``` r
validate_stat_columns_alignment(layers)
```

## Arguments

- layers:

  List of tplyr_layer objects

## Value

Invisible TRUE if valid
