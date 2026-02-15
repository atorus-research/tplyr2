# Complete shift count data

Ensures all row_var × col_var × cols combinations exist, filling missing
combinations with zero counts.

## Usage

``` r
complete_shift_counts(
  counts,
  dt,
  all_cols,
  by_data_vars,
  row_var,
  denom_group = NULL
)
```
