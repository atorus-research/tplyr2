# Create layer settings

Configuration object for all layer options. Unused parameters default to
NULL and are ignored during build. Type-specific validation happens at
build time.

## Usage

``` r
layer_settings(
  format_strings = NULL,
  stat_columns = NULL,
  denoms_by = NULL,
  shift_denom = "total",
  denom_row = FALSE,
  denom_row_label = "n",
  denom_row_format = NULL,
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
  ci_method = c("clopper_pearson", "wilson", "wald", "agresti_coull", "jeffreys"),
  ci_level = 0.95,
  assoc_test = NULL,
  pct_lt = NULL,
  pct_gt = NULL,
  zero_count_display = "full",
  name = NULL
)
```

## Arguments

- format_strings:

  Named list of f_str objects

- stat_columns:

  Named list of f_str objects for count layers. Each entry produces its
  own result column per column group (e.g. one "n (\\ name used as the
  column sub-label. Column label attributes follow the pattern
  `"<column group> (N=n) | <stat name>"`. When set, it takes precedence
  over `format_strings`. Names may not contain `" | "` or `"(N="`, which
  are reserved by the label grammar.

- denoms_by:

  Character vector of variable names for denominator grouping. This
  **replaces** (does not augment) the default denominator grouping,
  which is the column (`cols`) variable(s). To get per-column
  denominators that also break down by a `by` variable, you must list
  the `cols` variable(s) explicitly alongside the `by` variable(s) —
  e.g. `denoms_by = c("TRT", "SEX")`, not `denoms_by = "SEX"`. Passing
  only the `by` variable collapses the denominator across the columns.

- shift_denom:

  Denominator basis for shift layers. `"total"` (default) computes
  percentages out of the column (`cols`) total — i.e. the treatment arm.
  `"column"` computes them column-wise, out of each shift *column* group
  (the "from"/baseline group) within the arm, which is the standard "%
  within the from group" shift display; the header `(N=)` labels then
  reflect the per-column-group denominators. Ignored when `denoms_by` is
  set (which specifies the grouping explicitly).

- denom_row:

  Logical, shift layers only. When `TRUE`, emit the per-column-group
  denominator (the same `total` used for the percentages) as an integer
  row above the shift-to rows — the "n" row of a threshold/normal-range
  shift table. Pairs naturally with `shift_denom = "column"`. Defaults
  to `FALSE`.

- denom_row_label:

  Character string, the row label for the `denom_row` row. Defaults to
  `"n"`.

- denom_row_format:

  An
  [`f_str`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
  object formatting the `denom_row` cells, shift layers only. Lets the
  denominator row carry its own width independent of the `n_counts`
  format (e.g. `f_str("xx", "n")` for a plain narrow integer). The f_str
  must reference a single variable (the denominator count is passed
  positionally). `NULL` (default) pads the integer to the width of the
  shift cells. An absent baseline group renders as `0` either way.

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

  List configuring the Missing row. Recognized keys:

  `missing_values`

  :   Character vector of target values to fold into the Missing row
      alongside `NA`.

  `label`

  :   Row label; defaults to `"Missing"`.

  `sort_value`

  :   Numeric sort key placing the row; defaults to `Inf` (last).

  `format`

  :   An
      [`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
      overriding the layer's count format for this row.

  `denom_exclude`

  :   Logical. When `TRUE`, the rows counted as missing leave the
      layer's percentage denominator, so percentages are of the
      non-missing population. This applies to every row in the layer,
      including the Missing row itself and any total row. Defaults to
      `FALSE`.

  Any other key is an error.

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

- ci_method:

  Method for the single-proportion confidence interval exposed through
  the `ci_lower`/`ci_upper` (and
  `distinct_ci_lower`/`distinct_ci_upper`) count-layer format keywords.
  One of `"clopper_pearson"` (default, exact / SAS `PROC FREQ EXACT`
  parity), `"wilson"` (score, matching
  `stats::prop.test(correct = FALSE)`), `"wald"`, `"agresti_coull"`, or
  `"jeffreys"`. See
  [`proportion_ci`](https://atorus-research.github.io/tplyr2/reference/proportion_ci.md).

- ci_level:

  Numeric coverage probability for the single-proportion confidence
  interval keywords. Defaults to `0.95`.

- assoc_test:

  A
  [`assoc_test`](https://atorus-research.github.io/tplyr2/reference/assoc_test.md)
  object attaching an association-test p-value column. Omnibus mode
  works on count, shift, and desc layers (a desc layer's continuous
  comparison, e.g. ANOVA/Kruskal); pairwise/per-level mode is count
  layers only.

- pct_lt:

  Numeric less-than threshold for count-layer percents. A cell with a
  nonzero count whose percent would display below this value renders the
  percent as `"<"` followed by the threshold (e.g. `pct_lt = 1` shows
  `1 ( <1%)` instead of `1 ( 0%)`). NULL disables.

- pct_gt:

  Numeric greater-than threshold for count-layer percents. A cell whose
  percent is below 100 but would display above this value renders the
  percent as `">"` followed by the threshold (e.g. `pct_gt = 99` shows
  `>99` for `99.6%`). NULL disables.

- zero_count_display:

  How to display count-layer cells whose count is zero: `"full"`
  (default) keeps the usual `"0 ( 0%)"`; `"count_only"` shows only the
  count field (e.g. `" 0"`); `"blank"` shows an empty string.

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
| `stat_columns`             | X     |      |       |         |
| `denoms_by`                | X     | X    | X     |         |
| `shift_denom`              |       |      | X     |         |
| `denom_row`                |       |      | X     |         |
| `denom_row_label`          |       |      | X     |         |
| `denom_row_format`         |       |      | X     |         |
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
| `ci_method`                | X     |      |       |         |
| `ci_level`                 | X     |      |       |         |
| `assoc_test`               | X     | X    | X     |         |
| `pct_lt`                   | X     |      |       |         |
| `pct_gt`                   | X     |      |       |         |
| `zero_count_display`       | X     |      |       |         |
| `name`                     | X     | X    | X     | X       |

Settings provided for an inapplicable layer type are silently ignored.

## Examples

``` r
# Formats and special rows on a count layer
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("AGEGR1", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct")),
      total_row = TRUE,
      total_row_label = "Total subjects"
    ))
  )
)
tplyr_build(spec, tplyr_adsl)
#>        rowlabel1        res1        res2        res3 ord_layer_1
#> 1            <65  14 (16.3%)  11 (13.1%)   8 ( 9.5%)           1
#> 2          65-80  42 (48.8%)  55 (65.5%)  47 (56.0%)           2
#> 3            >80  30 (34.9%)  18 (21.4%)  29 (34.5%)           3
#> 4 Total subjects 86 (100.0%) 84 (100.0%) 84 (100.0%)           4
#>   ord_layer_index
#> 1               1
#> 2               1
#> 3               1
#> 4               1

# Denominators: percentages within each arm-by-sex cell rather than the arm
denom <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("AGEGR1", by = "SEX",
                settings = layer_settings(denoms_by = c("TRT01P", "SEX")))
  )
)
tplyr_build(denom, tplyr_adsl)
#>   rowlabel1 rowlabel2       res1       res2       res3 ord_layer_1
#> 1         F       <65  9 (17.0%)  5 (12.5%)  5 (10.0%)           1
#> 2         F     65-80 22 (41.5%) 28 (70.0%) 28 (56.0%)           2
#> 3         F       >80 22 (41.5%)  7 (17.5%) 17 (34.0%)           3
#> 4         M       <65  5 (15.2%)  6 (13.6%)  3 ( 8.8%)           4
#> 5         M     65-80 20 (60.6%) 27 (61.4%) 19 (55.9%)           5
#> 6         M       >80  8 (24.2%) 11 (25.0%) 12 (35.3%)           6
#>   ord_layer_index
#> 1               1
#> 2               1
#> 3               1
#> 4               1
#> 5               1
#> 6               1

# Auto-precision on a desc layer: 'a' takes decimals from the data, and
# precision_cap bounds them.
prec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL", by = "PARAMCD", settings = layer_settings(
      format_strings = list("Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd")),
      precision_by = "PARAMCD",
      precision_cap = c(int = 3, dec = 2)
    ))
  )
)
head(tplyr_build(prec, tplyr_adlb))
#>   rowlabel1 rowlabel2               res1               res2               res3
#> 1     URATE Mean (SD) 322.223 ( 64.9688) 298.849 ( 55.5429) 287.149 ( 76.8221)
#>   ord_layer_1 ord_layer_2 ord_layer_index
#> 1           1           1               1

# A custom summary adds a statistic the built-ins do not provide
cv <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(
      custom_summaries = list(cv = quote(sd(.var) / mean(.var) * 100)),
      format_strings = list("CV%" = f_str("xx.x", "cv"))
    ))
  )
)
tplyr_build(cv, tplyr_adsl)
#>   rowlabel1 res1 res2 res3 ord_layer_1 ord_layer_index
#> 1       CV% 11.4 10.6 11.0           1               1
```
