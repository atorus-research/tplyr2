#' @keywords internal
"_PACKAGE"

#' @importFrom stats IQR median quantile sd setNames var as.formula
#' @importFrom utils head tail
NULL

# Suppress R CMD check NOTEs for data.table column references
utils::globalVariables(c(
  ".", ".agg_cnt", ".by_ord", ".col_combo", ".comp_idx", ".disp", ".idx",
  ".is_special", ".join_key", ".missing_sort",
  ".ic", ".nest_level", ".nest_row_order", ".ord_tv", ".row_ord", ".row_order",
  ".row_seq", ".sort_inner", ".sort_key",
  ".sort_outer", ".total_sort", ".tplyr_synthetic", ".var_name",
  "analysis_id", "ci_lower", "ci_upper", "distinct_ci_lower",
  "distinct_ci_upper", "distinct_n", "distinct_pct", "distinct_total",
  "formatted", "formatted_rd", "i..agg_cnt", "i..disp", "i..ic",
  "i..sort_inner", "i.formatted_rd",
  "id", "max_dec", "max_int", "median", "n", "og_row", "ord1", "ord2",
  "ordindx", "out", "pct", "pop_n", "pval1", "row_label", "rowlabel1",
  "rowlabel2", "s", "sd", "stat_order", "stub_sort", "target_n",
  "total", "var"
))
