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

## Settings by Layer Type

Not all settings apply to every layer type. The table below shows which
settings are applicable for each of the four layer types:

|                            |       |      |       |         |
|----------------------------|-------|------|-------|---------|
| Setting                    | Count | Desc | Shift | Analyze |
| `format_strings`           | X     | X    | X     | X       |
| `denoms_by`                | X     | X    | X     |         |
| `denom_where`              | X     | X    | X     |         |
| `denom_ignore`             | X     |      | X     |         |
| `distinct_by`              | X     |      | X     |         |
| `total_row`                | X     |      |       |         |
| `total_row_label`          | X     |      |       |         |
| `total_row_count_missings` | X     |      |       |         |
| `missing_count`            | X     |      |       |         |
| `missing_subjects`         | X     |      |       |         |
| `missing_subjects_label`   | X     |      |       |         |
| `keep_levels`              | X     |      |       |         |
| `limit_data_by`            | X     |      |       |         |
| `custom_summaries`         |       | X    |       |         |
| `stats_as_columns`         |       | X    |       |         |
| `precision_by`             |       | X    |       |         |
| `precision_on`             |       | X    |       |         |
| `precision_data`           |       | X    |       |         |
| `precision_cap`            |       | X    |       |         |
| `order_count_method`       | X     |      |       |         |
| `ordering_cols`            | X     |      |       |         |
| `result_order_var`         | X     |      |       |         |
| `outer_sort_position`      | X     |      |       |         |
| `risk_diff`                | X     |      |       |         |
| `name`                     | X     | X    | X     | X       |

Settings provided for an inapplicable layer type are silently ignored.
