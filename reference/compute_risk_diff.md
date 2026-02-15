# Compute risk differences for count layer data

For each comparison pair and each target variable level, computes the
difference in proportions with a confidence interval using
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html).

## Usage

``` r
compute_risk_diff(counts_long, cols, tv, by_data_vars, risk_diff_config)
```

## Arguments

- counts_long:

  data.table in long format with n, total columns

- cols:

  Character vector of column variable names from the spec

- tv:

  Character string naming the target variable

- by_data_vars:

  Character vector of by-variable names

- risk_diff_config:

  List with comparisons, ci, and format

## Value

A data.table with one row per target_var level per comparison,
containing rdiff, lower, upper, p_value columns
