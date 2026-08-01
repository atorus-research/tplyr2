# Tag a numeric-data snapshot with its grouping columns

Records which columns identify a row rather than hold a statistic, so
[`tplyr_stats_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_stats_data.md)
can subset to grouping columns plus one statistic without guessing from
column types (a grouping variable can be numeric).

## Usage

``` r
tag_numeric_group_cols(snapshot, group_cols)
```

## Arguments

- snapshot:

  data.table snapshot, modified by reference

- group_cols:

  Character vector of candidate grouping column names

## Value

`snapshot`, invisibly
