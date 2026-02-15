# Compute sort keys for count layer rows

Computes ordering columns for a count layer's rows, based on the
by-variable and target variable values.

## Usage

``` r
compute_count_sort_keys(counts, dt, cols, by_data_vars, tv, settings)
```

## Arguments

- counts:

  data.table with count data (long format, before cast)

- dt:

  data.table with the original input data (for VARN lookup)

- cols:

  Character vector of spec column variables

- by_data_vars:

  Character vector of by data variable names

- tv:

  Character string, target variable name

- settings:

  Layer settings object

## Value

The counts data.table with `.ord_by_*` and `.ord_tv` columns added
