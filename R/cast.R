# =============================================================================
# Column assembly shared across layer types: row label construction,
# long-to-wide casting, and column labeling.
# =============================================================================

#' Write rowlabel columns onto a long-format layer table
#'
#' Writes one `rowlabel<n>` column per `by` label constant, then one per `by`
#' data variable (as character), then the value variable (as character).
#'
#' @param dt data.table to modify by reference
#' @param by_labels Character vector of constant `by` labels
#' @param by_data_vars Character vector of `by` data variable names
#' @param value_var Name of the variable supplying the final rowlabel column.
#'   ARD reconstruction can encounter a target variable absent from the stats
#'   table; the column is skipped (but still named) in that case.
#'
#' @return Character vector of the rowlabel column names
#' @keywords internal
build_row_labels_long <- function(dt, by_labels, by_data_vars, value_var) {
  label_cols <- character(0)
  col_idx <- 1L

  for (lbl in by_labels) {
    col_name <- str_c("rowlabel", col_idx)
    dt[, (col_name) := lbl]
    label_cols <- c(label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  for (bv in by_data_vars) {
    col_name <- str_c("rowlabel", col_idx)
    dt[, (col_name) := as.character(get(bv))]
    label_cols <- c(label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  col_name <- str_c("rowlabel", col_idx)
  if (value_var %in% names(dt)) {
    dt[, (col_name) := as.character(get(value_var))]
  }
  label_cols <- c(label_cols, col_name)

  label_cols
}

#' Classify by values into data variables and labels
#' @keywords internal
classify_by <- function(by, col_names) {
  if (is.null(by)) {
    return(list(data_vars = character(0), labels = character(0)))
  }

  # If the whole vector is a label, treat all elements as labels
  if (is_label(by)) {
    return(list(data_vars = character(0), labels = as.character(by)))
  }

  # Coerce to list to preserve label() classes on individual elements
  if (!is.list(by)) {
    by <- as.list(by)
  }

  data_vars <- character(0)
  labels <- character(0)

  for (b in by) {
    if (is_label(b)) {
      labels <- c(labels, as.character(b))
    } else if (b %in% col_names) {
      data_vars <- c(data_vars, b)
    } else {
      # Not a column name, treat as label
      labels <- c(labels, b)
    }
  }

  list(data_vars = data_vars, labels = labels)
}

#' Ordered factor levels for the column variable(s)
#'
#' Returns a named list mapping each `cols` variable that is a factor in
#' `source_dt` to its level order. Non-factor column variables are omitted.
#' Used to preserve the column variable's factor-level order through the
#' `dcast()` in `cast_to_wide()` (issue #13), so count/shift/desc layers all
#' order their `res*` columns by factor levels rather than alphabetically.
#'
#' @param source_dt data.table with the original (factor-typed) input data
#' @param cols Character vector of column variable names
#' @keywords internal
get_col_levels <- function(source_dt, cols, complete = FALSE) {
  present <- keep(cols, function(col) col %in% names(source_dt))
  wanted <- if (complete) {
    # Every column variable, so the level set can be pinned for all layers even
    # when a variable is a plain character vector.
    present
  } else {
    keep(present, function(col) is.factor(source_dt[[col]]))
  }
  setNames(map(wanted, function(col) {
    v <- source_dt[[col]]
    if (is.factor(v)) levels(v) else sort(unique(as.character(v)))
  }), wanted)
}

#' Combine pinned column levels with a layer's own
#'
#' The spec-level level set (every column-variable value in the table's data)
#' takes precedence so a layer whose `where` empties a column group still emits
#' that column. Any additional variable the layer knows about (a shift layer's
#' own column variable) is carried through.
#'
#' @param pinned Named list of levels captured before layer filtering (or NULL)
#' @param layer_levels Named list from `get_col_levels()` on the layer data
#' @return Named list of levels
#' @keywords internal
merge_col_levels <- function(pinned, layer_levels) {
  if (is.null(pinned) || length(pinned) == 0) return(layer_levels)
  out <- layer_levels
  out[names(pinned)] <- pinned
  out
}

#' Prepare the dcast column variable, respecting factor-level order
#'
#' For a single column variable, converts it to a factor ordered by
#' `col_levels` so `dcast()` spreads columns in level order. For multiple
#' column variables, builds the `" | "`-joined interaction column and, when
#' any component is a factor, orders it by the cross-product of each
#' variable's level order (outermost variable varies slowest). When no
#' component is a factor the interaction is left as a character vector, so
#' `dcast()` falls back to alphabetical order exactly as before.
#'
#' @param dt Long data.table about to be cast (mutated in place)
#' @param cols Character vector of column variable names
#' @param col_levels Named list from `get_col_levels()` (may be NULL/empty)
#' @return The name of the variable to use on the RHS of the dcast formula
#' @keywords internal
prepare_cast_column <- function(dt, cols, col_levels = NULL) {
  if (length(cols) == 1) {
    col <- cols[1]
    lv <- col_levels[[col]]
    if (!is.null(lv)) {
      dt[, (col) := factor(as.character(get(col)), levels = lv)]
    }
    return(col)
  }

  # Multiple column variables: build the interaction column
  dt[, .col_combo := do.call(str_c, c(.SD, sep = " | ")), .SDcols = cols]

  has_levels <- length(intersect(cols, names(col_levels))) > 0
  if (has_levels) {
    # Order each component by factor levels (observed only), else alphabetically
    per_col_order <- map(cols, function(col) {
      observed <- unique(as.character(dt[[col]]))
      lv <- col_levels[[col]]
      if (!is.null(lv)) lv[lv %in% observed] else sort(observed)
    })
    # Cross-product with the first (outermost) variable varying slowest
    grid <- expand.grid(rev(per_col_order), stringsAsFactors = FALSE,
                        KEEP.OUT.ATTRS = FALSE)
    grid <- grid[, rev(seq_along(grid)), drop = FALSE]
    combo_levels <- do.call(str_c, c(grid, sep = " | "))
    dt[, .col_combo := factor(.col_combo, levels = combo_levels)]
  }

  ".col_combo"
}

#' Cast long data to wide output format
#'
#' When `stat_labels` is provided (stat_columns mode), the long data carries
#' one `formatted_<i>` column per statistic and each column group spreads
#' into one res column per statistic, interleaved column-group-major. Column
#' labels follow the pattern `"<column group> (N=n) | <stat label>"` so
#' renderers can span the column group over its stat sub-columns.
#'
#' @param stat_labels Character vector of stat column labels (the names of
#'   the `stat_columns` setting), or NULL for the standard single-format cast
#' @param col_levels Named list mapping factor column variables to their level
#'   order (from `get_col_levels()`); orders the resulting `res*` columns by
#'   factor levels instead of alphabetically. NULL leaves dcast's default
#'   alphabetical column order.
#' @param row_order_col Name of a numeric column in `dt` giving the intended
#'   row order. dcast sorts its LHS alphabetically, so a caller whose row
#'   labels are not alphabetical (e.g. format-string names) must carry the
#'   order through the cast; the column joins the LHS, sorts the result, and is
#'   then dropped.
#' @keywords internal
cast_to_wide <- function(dt, row_label_cols, cols, layer_index, col_n = NULL,
                         stat_labels = NULL, col_levels = NULL,
                         row_order_col = NULL) {
  # Track column value labels for metadata
  col_labels <- NULL
  n_stats <- length(stat_labels)
  value_cols <- if (n_stats > 0) str_c("formatted_", seq_len(n_stats)) else "formatted"

  if (!is.null(row_order_col) && !row_order_col %in% names(dt)) {
    row_order_col <- NULL
  }
  lhs_cols <- c(row_label_cols, row_order_col)

  # Compute sort order before casting
  # Use .missing_sort and .total_sort for special rows, else row position
  if (".missing_sort" %in% names(dt) || ".total_sort" %in% names(dt)) {
    dt[, .sort_key := seq_len(.N)]
    if (".missing_sort" %in% names(dt)) {
      dt[!is.na(.missing_sort), .sort_key := .N + .missing_sort]
    }
    if (".total_sort" %in% names(dt)) {
      dt[!is.na(.total_sort), .sort_key := .N + .total_sort + 1000]
    }
    data.table::setorderv(dt, ".sort_key")
    dt[, .sort_key := NULL]
  }

  if (length(cols) == 0) {
    # No column variables - one result column per stat (no dcast, so the
    # pre-sorted row order is preserved)
    wide <- dt[, c(lhs_cols, value_cols), with = FALSE]
    data.table::setnames(wide, value_cols, str_c("res", seq_along(value_cols)))
    if (n_stats > 0) {
      col_labels <- stat_labels
    }
  } else {
    # Build dcast formula: row_labels ~ cols
    lhs <- str_c(lhs_cols, collapse = " + ")
    rhs <- prepare_cast_column(dt, cols, col_levels)
    formula_str <- str_c(lhs, " ~ ", rhs)
    wide <- data.table::dcast(
      dt,
      as.formula(formula_str),
      value.var = value_cols,
      fill = ""
    )

    if (n_stats > 1) {
      # Multiple value.var columns come back named "formatted_<i>_<combo>",
      # grouped stat-major. Reconstruct the expected names per column group
      # (never parse dcast output) and reorder column-group-major so each
      # group's stat columns sit adjacent.
      combos <- str_replace(str_subset(names(wide), "^formatted_1_"),
                            "^formatted_1_", "")
      val_cols <- unlist(map(combos, function(cmb) {
        str_c("formatted_", seq_len(n_stats), "_", cmb)
      }))
      data.table::setcolorder(wide, c(lhs_cols, val_cols))
    } else {
      # Single value.var: dcast names columns by the group value alone
      combos <- setdiff(names(wide), lhs_cols)
      val_cols <- combos
    }

    if (n_stats > 0) {
      # "(N=n)" attaches to the column-group segment; the stat label follows
      group_labels <- build_col_labels(combos, col_n)
      col_labels <- unlist(map(group_labels, function(gl) {
        str_c(gl, " | ", stat_labels)
      }))
    } else {
      col_labels <- build_col_labels(val_cols, col_n)
    }

    # Rename value columns from dcast names to res1, res2, ...
    data.table::setnames(wide, val_cols, str_c("res", seq_along(val_cols)))

    # Clean up temp column
    if (".col_combo" %in% names(dt)) {
      dt[, .col_combo := NULL]
    }
  }

  # Restore the caller's row order, which dcast's alphabetical LHS sort lost
  if (!is.null(row_order_col)) {
    data.table::setorderv(wide, row_order_col)
    wide[, (row_order_col) := NULL]
  }

  # Add ordering columns
  wide[, ordindx := layer_index]
  wide[, ord1 := seq_len(.N)]

  # Attach label attributes to result columns
  if (!is.null(col_labels)) {
    res_cols <- str_c("res", seq_along(col_labels))
    for (i in seq_along(res_cols)) {
      data.table::setattr(wide[[res_cols[i]]], "label", col_labels[i])
    }
  }

  wide
}

#' Build column labels with header N suffix
#'
#' Takes raw dcast column names and a col_n data.table, returns labels
#' with `"(N=<n>)"` suffix. For shift layers where the label includes both
#' spec-level cols and the shift column variable, only the spec-level portion
#' is used for the N lookup.
#'
#' @param raw_labels Character vector of raw column labels from dcast
#' @param col_n data.table with spec-level column variables and .n column,
#'   or NULL (in which case labels are returned unchanged)
#'
#' @return Character vector of labels with N suffix
#' @keywords internal
build_col_labels <- function(raw_labels, col_n) {
  if (is.null(col_n) || length(raw_labels) == 0) return(raw_labels)

  col_n_vars <- setdiff(names(col_n), ".n")

  # Build named lookup: combo_string -> N
  if (length(col_n_vars) == 1) {
    n_lookup <- setNames(col_n$.n, as.character(col_n[[col_n_vars[1]]]))
  } else {
    combo_strings <- do.call(str_c, c(col_n[, col_n_vars, with = FALSE], sep = " | "))
    n_lookup <- setNames(col_n$.n, combo_strings)
  }

  map_chr(raw_labels, function(lbl) {
    # Split label; use first length(col_n_vars) parts for N lookup
    parts <- str_split(lbl, fixed(" | "))[[1]]
    if (length(parts) > length(col_n_vars)) {
      key <- str_c(parts[seq_len(length(col_n_vars))], collapse = " | ")
    } else {
      key <- lbl
    }
    n_val <- n_lookup[key]
    if (!is.na(n_val)) str_c(lbl, " (N=", n_val, ")") else lbl
  })
}

#' Sort column names by their numeric suffix
#'
#' Lexicographic sorting places "res10" before "res2"; ordering by the
#' numeric suffix keeps columns in build order once a family has more
#' than 9 members.
#' @keywords internal
sort_by_numeric_suffix <- function(x) {
  x[order(as.integer(str_extract(x, "\\d+")))]
}

#' Locate the rowlabel columns to join summary statistics back on
#'
#' The inverse of \code{build_row_labels_long()}'s layout: one rowlabel column
#' per constant `by` label first, then one per `by` data variable, then the
#' target variable last. Callers that merge per-group statistics (risk
#' difference, pairwise p-values) onto the assembled table need the data
#' variable columns; assuming they start at `rowlabel1` keys the join against a
#' constant-label column whenever `by` leads with a string label, matching
#' nothing and leaving every cell blank.
#'
#' @param wide Assembled wide table
#' @param by_labels Character vector of constant `by` labels
#' @param by_data_vars Character vector of `by` data variable names
#'
#' @return List with `tv_col` (the target variable's rowlabel column) and
#'   `by_cols` (the by data variables' rowlabel columns, in `by_data_vars`
#'   order), or NULL when `wide` has no rowlabel columns
#' @keywords internal
resolve_rowlabel_join_cols <- function(wide, by_labels, by_data_vars) {
  all_label_cols <- sort_by_numeric_suffix(
    str_subset(names(wide), "^rowlabel\\d+$")
  )
  if (length(all_label_cols) == 0) return(NULL)

  by_cols <- all_label_cols[length(by_labels) + seq_along(by_data_vars)]

  list(
    tv_col = all_label_cols[length(all_label_cols)],
    by_cols = by_cols[!is.na(by_cols)]
  )
}

#' Harmonize column sets across layers and row-bind
#' @keywords internal
harmonize_and_bind <- function(layer_results) {
  if (length(layer_results) == 0) {
    return(data.table::data.table())
  }

  # Collect all column names across layers
  all_names <- unique(unlist(map(layer_results, names)))

  # Separate by type: rowlabel*, res*, rdiff*, ord*
  label_cols <- sort_by_numeric_suffix(str_subset(all_names, "^rowlabel"))
  res_cols <- sort_by_numeric_suffix(str_subset(all_names, "^res\\d"))
  rdiff_cols <- sort_by_numeric_suffix(str_subset(all_names, "^rdiff"))
  ord_cols <- sort(str_subset(all_names, "^ord"))

  target_cols <- c(label_cols, res_cols, rdiff_cols, ord_cols)

  # Pad each layer result with missing columns
  for (i in seq_along(layer_results)) {
    dt <- layer_results[[i]]
    missing_cols <- setdiff(target_cols, names(dt))
    for (col in missing_cols) {
      if (str_detect(col, "^ord")) {
        dt[, (col) := NA_real_]
      } else {
        dt[, (col) := ""]
      }
    }
    # Reorder columns
    data.table::setcolorder(dt, intersect(target_cols, names(dt)))
    layer_results[[i]] <- dt
  }

  data.table::rbindlist(layer_results, use.names = TRUE, fill = TRUE)
}
