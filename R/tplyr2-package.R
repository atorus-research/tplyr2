#' @keywords internal
"_PACKAGE"

#' @importFrom stats IQR median quantile sd setNames var as.formula
#' @importFrom utils head tail
NULL

# Suppress R CMD check NOTEs for data.table column references
utils::globalVariables(c(
  ".", ".agg_cnt", ".col_combo", ".comp_idx", ".disp", ".idx", ".is_special",
  ".join_key", ".missing_sort",
  ".ic", ".nest_level", ".ord_tv", ".row_order", ".sort_inner", ".sort_key",
  ".sort_outer", ".total_sort", ".tplyr_synthetic", ".var_name",
  "analysis_id", "distinct_n", "distinct_pct", "distinct_total",
  "formatted", "formatted_rd", "i..agg_cnt", "i..ic", "i..sort_inner", "i.formatted_rd",
  "id", "max_dec", "max_int", "median", "n", "og_row", "ord1",
  "ordindx", "out", "pct", "pop_n", "row_label", "rowlabel1",
  "rowlabel2", "s", "sd", "stat_order", "stub_sort", "target_n",
  "total", "var"
))
