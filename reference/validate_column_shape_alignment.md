# Validate that every layer produces the same result-column shape

Result columns are aligned positionally by name across layers. A shift
layer emits one column per `cols` level *crossed with* its shift-column
variable, and a `stats_as_columns` desc layer emits one per level
crossed with each statistic. Either alongside a layer with the plain
one-column-per- level shape leaves the combined table's `res` columns
meaning different things in different row blocks, with only the first
layer's column labels retained — so the values appear under the wrong
treatment arm.

## Usage

``` r
validate_column_shape_alignment(layers)
```

## Arguments

- layers:

  List of tplyr_layer objects

## Value

Invisible TRUE if valid
