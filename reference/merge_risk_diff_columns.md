# Merge risk difference columns onto wide result

Appends formatted risk difference columns to the wide-format output from
[`cast_to_wide()`](https://github.com/mstackhouse/tplyr2/reference/cast_to_wide.md).

## Usage

``` r
merge_risk_diff_columns(
  wide,
  rd_data,
  risk_diff_config,
  row_label_cols,
  tv,
  by_data_vars
)
```

## Arguments

- wide:

  data.table in wide format (after cast_to_wide)

- rd_data:

  data.table with computed risk differences

- risk_diff_config:

  List with comparisons, ci, format

- row_label_cols:

  Character vector of row label column names

- tv:

  Character string naming the target variable

- by_data_vars:

  Character vector of by-variable names

## Value

Modified wide data.table with rdiff columns appended
