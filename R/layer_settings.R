#' Create layer settings
#'
#' Configuration object for all layer options. Unused parameters default to NULL
#' and are ignored during build. Type-specific validation happens at build time.
#'
#' @section Settings by Layer Type:
#'
#' Not all settings apply to every layer type. The table below shows which
#' settings are applicable for each of the four layer types:
#'
#' | Setting | Count | Desc | Shift | Analyze |
#' | --- | --- | --- | --- | --- |
#' | `format_strings` | X | X | X | X |
#' | `stat_columns` | X | | | |
#' | `denoms_by` | X | X | X | |
#' | `shift_denom` | | | X | |
#' | `denom_row` | | | X | |
#' | `denom_row_label` | | | X | |
#' | `denom_where` | X | X | X | |
#' | `denom_ignore` | X | | X | |
#' | `distinct_by` | X | | X | |
#' | `total_row` | X | | | |
#' | `total_row_label` | X | | | |
#' | `total_row_count_missings` | X | | | |
#' | `missing_count` | X | | | |
#' | `missing_subjects` | X | | | |
#' | `missing_subjects_label` | X | | | |
#' | `keep_levels` | X | | | |
#' | `limit_data_by` | X | | | |
#' | `custom_summaries` | | X | | |
#' | `stats_as_columns` | | X | | |
#' | `precision_by` | | X | | |
#' | `precision_on` | | X | | |
#' | `precision_data` | | X | | |
#' | `precision_cap` | | X | | |
#' | `order_count_method` | X | | | |
#' | `ordering_cols` | X | | | |
#' | `result_order_var` | X | | | |
#' | `outer_sort_position` | X | | | |
#' | `risk_diff` | X | | | |
#' | `ci_method` | X | | | |
#' | `ci_level` | X | | | |
#' | `assoc_test` | X | | X | |
#' | `pct_lt` | X | | | |
#' | `pct_gt` | X | | | |
#' | `zero_count_display` | X | | | |
#' | `name` | X | X | X | X |
#'
#' Settings provided for an inapplicable layer type are silently ignored.
#'
#' @param format_strings Named list of f_str objects
#' @param stat_columns Named list of f_str objects for count layers. Each
#'   entry produces its own result column per column group (e.g. one
#'   "n (\%)" column and one "E" column per treatment arm), with the entry
#'   name used as the column sub-label. Column label attributes follow the
#'   pattern \code{"<column group> (N=n) | <stat name>"}. When set, it takes
#'   precedence over \code{format_strings}. Names may not contain
#'   \code{" | "} or \code{"(N="}, which are reserved by the label grammar.
#' @param denoms_by Character vector of variable names for denominator grouping.
#'   This **replaces** (does not augment) the default denominator grouping,
#'   which is the column (\code{cols}) variable(s). To get per-column
#'   denominators that also break down by a \code{by} variable, you must list
#'   the \code{cols} variable(s) explicitly alongside the \code{by}
#'   variable(s) — e.g. \code{denoms_by = c("TRT", "SEX")}, not
#'   \code{denoms_by = "SEX"}. Passing only the \code{by} variable collapses
#'   the denominator across the columns.
#' @param shift_denom Denominator basis for shift layers. \code{"total"}
#'   (default) computes percentages out of the column (\code{cols}) total —
#'   i.e. the treatment arm. \code{"column"} computes them column-wise, out of
#'   each shift *column* group (the "from"/baseline group) within the arm,
#'   which is the standard "% within the from group" shift display; the header
#'   \code{(N=)} labels then reflect the per-column-group denominators. Ignored
#'   when \code{denoms_by} is set (which specifies the grouping explicitly).
#' @param denom_row Logical, shift layers only. When \code{TRUE}, emit the
#'   per-column-group denominator (the same \code{total} used for the
#'   percentages) as an integer row above the shift-to rows — the "n" row of a
#'   threshold/normal-range shift table. Pairs naturally with
#'   \code{shift_denom = "column"}. Defaults to \code{FALSE}.
#' @param denom_row_label Character string, the row label for the \code{denom_row}
#'   row. Defaults to \code{"n"}.
#' @param denom_where Expression for separate denominator filter
#' @param denom_ignore Character vector of values to exclude from denominators
#' @param distinct_by Character string naming the variable for distinct counting
#' @param total_row Logical, whether to add a total row
#' @param total_row_label Character string for total row label
#' @param total_row_count_missings Logical, include missing in total
#' @param missing_count List with missing count configuration
#' @param missing_subjects Logical, add missing subjects row
#' @param missing_subjects_label Character string for missing subjects label
#' @param keep_levels Character vector of levels to keep
#' @param limit_data_by Character vector for data limiting
#' @param custom_summaries Named list of expressions for custom summaries
#' @param stats_as_columns Logical, transpose stats to columns
#' @param precision_by Character vector for precision grouping
#' @param precision_on Character string for precision variable
#' @param precision_data Data frame with external precision values
#' @param precision_cap Named numeric vector c(int=, dec=)
#' @param order_count_method Character, ordering method
#' @param ordering_cols Character, which column drives ordering
#' @param result_order_var Character, which result variable for ordering
#' @param outer_sort_position Character, outer sort direction
#' @param risk_diff List with risk difference configuration
#' @param ci_method Method for the single-proportion confidence interval exposed
#'   through the \code{ci_lower}/\code{ci_upper} (and
#'   \code{distinct_ci_lower}/\code{distinct_ci_upper}) count-layer format
#'   keywords. One of \code{"clopper_pearson"} (default, exact / SAS
#'   \code{PROC FREQ EXACT} parity), \code{"wilson"} (score, matching
#'   \code{stats::prop.test(correct = FALSE)}), \code{"wald"},
#'   \code{"agresti_coull"}, or \code{"jeffreys"}. See \code{\link{proportion_ci}}.
#' @param ci_level Numeric coverage probability for the single-proportion
#'   confidence interval keywords. Defaults to \code{0.95}.
#' @param assoc_test A \code{\link{assoc_test}} object attaching an omnibus
#'   association-test p-value column (count and shift layers).
#' @param pct_lt Numeric less-than threshold for count-layer percents. A cell
#'   with a nonzero count whose percent would display below this value renders
#'   the percent as \code{"<"} followed by the threshold (e.g. \code{pct_lt = 1}
#'   shows \code{1 ( <1\%)} instead of \code{1 (  0\%)}). NULL disables.
#' @param pct_gt Numeric greater-than threshold for count-layer percents. A cell
#'   whose percent is below 100 but would display above this value renders the
#'   percent as \code{">"} followed by the threshold (e.g. \code{pct_gt = 99}
#'   shows \code{>99} for \code{99.6\%}). NULL disables.
#' @param zero_count_display How to display count-layer cells whose count is
#'   zero: \code{"full"} (default) keeps the usual \code{"0 (  0\%)"};
#'   \code{"count_only"} shows only the count field (e.g. \code{" 0"});
#'   \code{"blank"} shows an empty string.
#' @param name Character string, layer name for identification
#'
#' @return A tplyr_layer_settings object
#' @export
layer_settings <- function(
    format_strings = NULL,
    stat_columns = NULL,
    denoms_by = NULL,
    shift_denom = "total",
    denom_row = FALSE,
    denom_row_label = "n",
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
    ci_method = c("clopper_pearson", "wilson", "wald", "agresti_coull",
                  "jeffreys"),
    ci_level = 0.95,
    assoc_test = NULL,
    pct_lt = NULL,
    pct_gt = NULL,
    zero_count_display = "full",
    name = NULL
) {
  zero_count_display <- match.arg(zero_count_display,
                                  c("full", "count_only", "blank"))
  shift_denom <- match.arg(shift_denom, c("total", "column"))
  ci_method <- match.arg(ci_method)
  structure(
    list(
      format_strings = format_strings,
      stat_columns = stat_columns,
      denoms_by = denoms_by,
      shift_denom = shift_denom,
      denom_row = denom_row,
      denom_row_label = denom_row_label,
      denom_where = denom_where,
      denom_ignore = denom_ignore,
      distinct_by = distinct_by,
      total_row = total_row,
      total_row_label = total_row_label,
      total_row_count_missings = total_row_count_missings,
      missing_count = missing_count,
      missing_subjects = missing_subjects,
      missing_subjects_label = missing_subjects_label,
      keep_levels = keep_levels,
      limit_data_by = limit_data_by,
      custom_summaries = custom_summaries,
      stats_as_columns = stats_as_columns,
      precision_by = precision_by,
      precision_on = precision_on,
      precision_data = precision_data,
      precision_cap = precision_cap,
      order_count_method = order_count_method,
      ordering_cols = ordering_cols,
      result_order_var = result_order_var,
      outer_sort_position = outer_sort_position,
      risk_diff = risk_diff,
      ci_method = ci_method,
      ci_level = ci_level,
      assoc_test = assoc_test,
      pct_lt = pct_lt,
      pct_gt = pct_gt,
      zero_count_display = zero_count_display,
      name = name
    ),
    class = "tplyr_layer_settings"
  )
}
