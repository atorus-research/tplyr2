# Format values with auto-precision

Handles the split-apply-combine when precision_by creates multiple
precision groups. For a single precision group (no precision_by),
resolves precision once and formats all rows.

## Usage

``` r
format_with_precision(
  fmt,
  var_names,
  stats,
  group_vars,
  precision_table,
  precision_by
)
```

## Arguments

- fmt:

  An f_str object

- var_names:

  Character vector of variable names to format

- stats:

  data.table with computed statistics

- group_vars:

  Character vector of grouping column names

- precision_table:

  data.table from collect_precision()

- precision_by:

  Character vector of precision grouping variables (or NULL)

## Value

Character vector of formatted values
