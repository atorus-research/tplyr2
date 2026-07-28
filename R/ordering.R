#' Compute sort key for a variable
#'
#' Returns an integer or numeric vector of sort keys for the values of a variable.
#' Priority: factor levels > VARN companion column > alphabetical.
#' The `method` parameter can override this auto-detection.
#'
#' @param values Character or factor vector of values to sort
#' @param var_name Character string naming the variable (for VARN lookup)
#' @param data_dt data.table with the raw data (for VARN lookup)
#' @param method Character: NULL (auto), "byfactor", "byvarn", "bycount", "alphabetical"
#' @param count_values Numeric vector of counts per row (for bycount method)
#'
#' @return Numeric vector of sort keys (lower = earlier)
#' @keywords internal
compute_var_order <- function(values, var_name = NULL, data_dt = NULL,
                               method = NULL, count_values = NULL) {
  # Count-based ordering
  if (!is.null(method) && method == "bycount") {
    if (!is.null(count_values)) {
      return(-count_values)  # Descending: higher count = lower sort key
    }
    # Fall through to alphabetical if no count_values provided
  }

  # Factor ordering
  if (is.null(method) || method == "byfactor") {
    if (is.factor(values)) {
      return(match(values, levels(values)))
    }
    # `values` is often a character vector here (the target column is rebuilt
    # from CJ levels in complete_counts, and row-label columns are stored as
    # character), so recover the factor levels from the source data. Without
    # this, byfactor falls back to alphabetical ordering (issue #16), the
    # row-ordering analogue of the column-ordering fix in issue #13.
    if (!is.null(var_name) && !is.null(data_dt) &&
        var_name %in% names(data_dt) && is.factor(data_dt[[var_name]])) {
      lv <- levels(data_dt[[var_name]])
      keys <- match(as.character(values), lv)
      # Only use factor ordering if the values actually correspond to levels;
      # otherwise (e.g. special rows) fall through to the remaining methods.
      if (!all(is.na(keys))) {
        return(keys)
      }
    }
  }

  # VARN companion column
  if ((is.null(method) || method == "byvarn") && !is.null(var_name) && !is.null(data_dt)) {
    varn_col <- str_c(var_name, "N")
    if (varn_col %in% names(data_dt)) {
      lookup <- unique(data_dt[, c(var_name, varn_col), with = FALSE])
      varn_vals <- lookup[[varn_col]][match(as.character(values), as.character(lookup[[var_name]]))]
      if (!all(is.na(varn_vals))) {
        return(varn_vals)
      }
    }
  }

  # Alphabetical fallback
  data.table::frank(as.character(values), ties.method = "dense")
}

#' Compute sort keys for count layer rows
#'
#' Computes ordering columns for a count layer's rows, based on the
#' by-variable and target variable values.
#'
#' @param counts data.table with count data (long format, before cast)
#' @param dt data.table with the original input data (for VARN lookup)
#' @param cols Character vector of spec column variables
#' @param by_data_vars Character vector of by data variable names
#' @param tv Character string, target variable name
#' @param settings Layer settings object
#'
#' @return The counts data.table with `.ord_by_*` and `.ord_tv` columns added
#' @keywords internal
compute_count_sort_keys <- function(counts, dt, cols, by_data_vars, tv, settings) {
  method <- settings$order_count_method
  ordering_cols_setting <- settings$ordering_cols
  result_order_var <- settings$result_order_var %||% "n"

  # Sort key for each by_data_var. The by columns on `counts` are character
  # (rebuilt from CJ levels in complete_counts), so recover a factor by
  # variable's level order from the source data directly; otherwise fall back
  # to compute_var_order (VARN companion, then alphabetical). Issue #24.
  for (i in seq_along(by_data_vars)) {
    bv <- by_data_vars[i]
    col_name <- str_c(".ord_by_", i)
    if (is.factor(dt[[bv]])) {
      lv <- levels(dt[[bv]])
      counts[, (col_name) := match(as.character(get(bv)), lv)]
    } else {
      counts[, (col_name) := compute_var_order(
        get(bv), var_name = bv, data_dt = dt
      )]
    }
  }

  # Sort key for target variable
  if (!is.null(method) && method == "bycount") {
    # Count-based ordering. The sort key must be constant across the column
    # (cols) dimension for a given target value, so aggregate the ordering
    # statistic per (by-group, target) rather than using the per-column count.
    # `result_order_var` picks the statistic (default "n"); `ordering_cols`
    # restricts the tally to a specific column level (issue #57).
    order_var <- if (result_order_var %in% names(counts)) result_order_var else "n"
    agg_src <- if (".is_special" %in% names(counts)) {
      counts[.is_special == 0]
    } else {
      counts
    }
    if (!is.null(ordering_cols_setting) && length(cols) >= 1 &&
        cols[1] %in% names(counts)) {
      agg_src <- agg_src[as.character(get(cols[1])) %in%
                           as.character(ordering_cols_setting)]
    }
    key_vars <- c(by_data_vars, tv)
    agg <- agg_src[, list(.agg_cnt = sum(get(order_var), na.rm = TRUE)),
                   by = key_vars]
    counts[agg, .agg_cnt := i..agg_cnt, on = key_vars]
    counts[is.na(.agg_cnt), .agg_cnt := 0]
    counts[, .ord_tv := -.agg_cnt]   # descending count -> ascending sort key
    counts[, .agg_cnt := NULL]
  } else {
    counts[, .ord_tv := compute_var_order(
      get(tv), var_name = tv, data_dt = dt, method = method
    )]
  }

  counts
}

#' Rename ordering columns to match DESIGN.md convention
#'
#' Renames `ordindx` to `ord_layer_index` and `ord1`/`ord2`/... to
#' `ord_layer_1`/`ord_layer_2`/...
#'
#' @param result data.table
#' @return Modified data.table (by reference)
#' @keywords internal
rename_ord_columns <- function(result) {
  if ("ordindx" %in% names(result)) {
    data.table::setnames(result, "ordindx", "ord_layer_index")
  }

  ord_cols <- str_subset(names(result), "^ord\\d+$")
  if (length(ord_cols) > 0) {
    new_names <- str_replace(ord_cols, "^ord(\\d+)$", "ord_layer_\\1")
    data.table::setnames(result, ord_cols, new_names)
  }

  invisible(result)
}
