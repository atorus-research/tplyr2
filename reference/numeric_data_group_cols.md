# Grouping columns of a numeric-data snapshot

Layer builders tag each snapshot with the columns that identify a row
(see
[`tag_numeric_group_cols()`](https://github.com/mstackhouse/tplyr2/reference/tag_numeric_group_cols.md)).
Snapshots from an older build lack the attribute, so fall back to
treating the non-numeric columns as grouping.

## Usage

``` r
numeric_data_group_cols(nd)
```

## Arguments

- nd:

  A numeric-data snapshot data.frame

## Value

Character vector of grouping column names
