# Create layer settings

Configuration object for all layer options. Unused parameters default to
NULL and are ignored during build. Type-specific validation happens at
build time.

## Usage

``` r
layer_settings(
  format_strings = NULL,
  denoms_by = NULL,
  denom_where = NULL,
  denom_ignore = NULL,
  distinct_by = NULL,
  indentation = "  ",
  total_row = FALSE,
  total_row_label = "Total",
  total_row_count_missings = TRUE,
  missing_count = NULL,
  missing_subjects = FALSE,
  missing_subjects_label = "Missing",
  keep_levels = NULL,
  limit_data_by = NULL,
  custom_summaries = NULL,
  stats_as_columns = FALSE,
  precision_by = NULL,
  precision_on = NULL,
  precision_data = NULL,
  precision_cap = NULL,
  order_count_method = NULL,
  ordering_cols = NULL,
  result_order_var = NULL,
  outer_sort_position = NULL,
  risk_diff = NULL,
  name = NULL
)
```

## Arguments

- format_strings:

  Named list of f_str objects

- denoms_by:

  Character vector of variable names for denominator grouping

- denom_where:

  Expression for separate denominator filter

- denom_ignore:

  Character vector of values to exclude from denominators

- distinct_by:

  Character string naming the variable for distinct counting

- indentation:

  Character string for nested count indentation

- total_row:

  Logical, whether to add a total row

- total_row_label:

  Character string for total row label

- total_row_count_missings:

  Logical, include missing in total

- missing_count:

  List with missing count configuration

- missing_subjects:

  Logical, add missing subjects row

- missing_subjects_label:

  Character string for missing subjects label

- keep_levels:

  Character vector of levels to keep

- limit_data_by:

  Character vector for data limiting

- custom_summaries:

  Named list of expressions for custom summaries

- stats_as_columns:

  Logical, transpose stats to columns

- precision_by:

  Character vector for precision grouping

- precision_on:

  Character string for precision variable

- precision_data:

  Data frame with external precision values

- precision_cap:

  Named numeric vector c(int=, dec=)

- order_count_method:

  Character, ordering method

- ordering_cols:

  Character, which column drives ordering

- result_order_var:

  Character, which result variable for ordering

- outer_sort_position:

  Character, outer sort direction

- risk_diff:

  List with risk difference configuration

- name:

  Character string, layer name for identification

## Value

A tplyr_layer_settings object
