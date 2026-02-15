# Compute missing subjects row

Counts subjects present in pop_data but absent from target data. Uses
`distinct_by` to identify subjects; if NULL, uses row-level counts.

## Usage

``` r
compute_missing_subjects(
  dt,
  pop_dt,
  cols,
  by_data_vars,
  tv,
  distinct_by,
  missing_label,
  denom_group,
  denom_dt,
  fmt
)
```

## Arguments

- dt:

  data.table with target data (after layer where filter)

- pop_dt:

  data.table with population data

- cols:

  Character vector of spec column variable names

- by_data_vars:

  Character vector of by-variable names from data

- tv:

  Character string, target variable name

- distinct_by:

  Character string naming the subject identifier (or NULL)

- missing_label:

  Character string for the row label

- denom_group:

  Character vector of denominator grouping variables

- denom_dt:

  data.table for denominator computation

- fmt:

  f_str object for formatting

## Value

A data.table for the missing subjects row, or NULL
