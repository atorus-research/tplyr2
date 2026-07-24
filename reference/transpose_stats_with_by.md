# Transpose desc stats to columns while keeping `by` groups as rows

Produces one row per by-group combination and one result column per
treatment x statistic, ordered treatment-major then statistic. Result
columns carry a `"<treatment label> | <stat name>"` label attribute
following the same grammar as count-layer `stat_columns`.

## Usage

``` r
transpose_stats_with_by(wide, res_cols, trt_labels, by_cols, stat_col)
```
