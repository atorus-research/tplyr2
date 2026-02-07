#' Process a count layer
#'
#' @param dt data.table with the (filtered) input data
#' @param layer A tplyr_count_layer object
#' @param cols Character vector of column variable names from the spec
#' @param layer_index Integer index of this layer
#'
#' @return A data.table with rowlabel*, res*, and ord* columns
#' @keywords internal
build_count_layer <- function(dt, layer, cols, layer_index, col_n = NULL, pop_dt = NULL) {
  target_var <- layer$target_var
  by <- layer$by
  settings <- layer$settings

  # Apply layer-level where
  if (!is.null(layer$where) && !identical(layer$where, TRUE)) {
    dt <- dt[eval(layer$where)]
  }

  # Separate by into data variables and labels
  by_info <- classify_by(by, names(dt))
  by_data_vars <- by_info$data_vars
  by_labels <- by_info$labels

  # Dispatch: single vs nested

  if (length(target_var) == 1) {
    build_count_layer_single(dt, target_var[1], cols, by_data_vars, by_labels,
                              settings, layer_index, col_n = col_n, pop_dt = pop_dt)
  } else {
    build_count_layer_nested(dt, target_var, cols, by_data_vars, by_labels,
                              settings, layer_index, col_n = col_n, pop_dt = pop_dt)
  }
}

# =============================================================================
# Single-variable count layer (Phase 1/2 path)
# =============================================================================

#' Process a single-variable count layer
#' @keywords internal
build_count_layer_single <- function(dt, tv, cols, by_data_vars, by_labels,
                                      settings, layer_index, col_n = NULL, pop_dt = NULL) {
  # Extract settings
  distinct_by   <- settings$distinct_by
  denoms_by     <- settings$denoms_by
  denom_where   <- settings$denom_where
  denom_ignore  <- settings$denom_ignore
  total_row     <- settings$total_row %||% FALSE
  total_label   <- settings$total_row_label %||% "Total"
  total_missings <- settings$total_row_count_missings %||% TRUE
  missing_count <- settings$missing_count
  keep_levels   <- settings$keep_levels
  limit_data_by <- settings$limit_data_by

  # Group variables for counting
  group_vars <- c(cols, by_data_vars, tv)

  # --- Count rows ---
  counts <- dt[, list(n = .N), by = group_vars]

  # --- Distinct counting ---
  if (!is.null(distinct_by)) {
    distinct_counts <- dt[, list(distinct_n = uniqueN(get(distinct_by))), by = group_vars]
    counts <- merge(counts, distinct_counts, by = group_vars, all.x = TRUE)
  }

  # --- Prepare denominator dataset ---
  denom_dt <- data.table::copy(pop_dt %||% dt)
  if (!is.null(denom_where) && !identical(denom_where, TRUE)) {
    denom_dt <- denom_dt[eval(denom_where)]
  }
  if (!is.null(denom_ignore)) {
    denom_dt <- denom_dt[!get(tv) %in% denom_ignore]
  }

  # --- Compute denominators ---
  denom_group <- if (!is.null(denoms_by)) denoms_by else {
    if (length(cols) > 0) cols else character(0)
  }

  if (length(denom_group) > 0) {
    denoms <- denom_dt[, list(total = .N), by = denom_group]
    counts <- merge(counts, denoms, by = denom_group, all.x = TRUE)
  } else {
    counts[, total := nrow(denom_dt)]
  }
  counts[, pct := ifelse(total > 0, n / total * 100, 0)]

  # --- Distinct denominators ---
  if (!is.null(distinct_by)) {
    if (length(denom_group) > 0) {
      distinct_denoms <- denom_dt[, list(distinct_total = uniqueN(get(distinct_by))), by = denom_group]
      counts <- merge(counts, distinct_denoms, by = denom_group, all.x = TRUE)
    } else {
      counts[, distinct_total := uniqueN(denom_dt[[distinct_by]])]
    }
    counts[, distinct_pct := ifelse(distinct_total > 0, distinct_n / distinct_total * 100, 0)]
  }

  # --- Data completion ---
  counts <- complete_counts(counts, dt, cols, by_data_vars, tv, limit_data_by, denom_group)

  # --- Apply keep_levels filter ---
  if (!is.null(keep_levels)) {
    counts <- counts[get(tv) %in% keep_levels]
  }

  # --- Missing count handling ---
  if (!is.null(missing_count)) {
    missing_result <- compute_missing_counts(
      dt, counts, cols, by_data_vars, tv, group_vars, denom_group,
      denom_dt, distinct_by, missing_count
    )
    counts <- missing_result$counts
    missing_row <- missing_result$missing_row
  } else {
    missing_row <- NULL
  }

  # --- Missing subjects ---
  missing_subj_label <- settings$missing_subjects_label %||% "Missing"
  missing_subjects_row <- NULL
  if (isTRUE(settings$missing_subjects)) {
    fmt_ms <- get_count_format(settings)
    missing_subjects_row <- compute_missing_subjects(
      dt, pop_dt, cols, by_data_vars, tv, distinct_by,
      missing_subj_label, denom_group, denom_dt, fmt_ms
    )
  }

  # --- Total row ---
  if (total_row) {
    total_result <- compute_total_row(
      counts, dt, cols, by_data_vars, tv, total_label, total_missings,
      distinct_by, missing_count, denom_group, denom_dt
    )
  } else {
    total_result <- NULL
  }

  # --- Capture numeric data before formatting ---
  numeric_snapshot <- data.table::copy(counts)

  # --- Compute risk difference on main counts (before special rows) ---
  rd_data <- NULL
  if (!is.null(settings$risk_diff)) {
    rd_data <- compute_risk_diff(counts, cols, tv, by_data_vars, settings$risk_diff)
  }

  # --- Format ---
  fmt <- get_count_format(settings)
  fmt_args <- lapply(fmt$vars, function(v) counts[[v]])
  counts[, formatted := do.call(apply_formats, c(list(fmt), fmt_args))]

  # Format missing row if present
  if (!is.null(missing_row)) {
    fmt_args_m <- lapply(fmt$vars, function(v) missing_row[[v]])
    missing_row[, formatted := do.call(apply_formats, c(list(fmt), fmt_args_m))]
  }

  # Format missing subjects row if present
  if (!is.null(missing_subjects_row)) {
    fmt_args_ms <- lapply(fmt$vars, function(v) missing_subjects_row[[v]])
    missing_subjects_row[, formatted := do.call(apply_formats, c(list(fmt), fmt_args_ms))]
  }

  # Format total row if present
  if (!is.null(total_result)) {
    fmt_args_t <- lapply(fmt$vars, function(v) total_result[[v]])
    total_result[, formatted := do.call(apply_formats, c(list(fmt), fmt_args_t))]
  }

  # --- Build row label columns ---
  row_labels <- build_row_labels_count(counts, by_labels, by_data_vars, tv)

  if (!is.null(missing_row)) {
    build_row_labels_special(missing_row, by_labels, by_data_vars, tv, row_labels)
  }
  if (!is.null(missing_subjects_row)) {
    build_row_labels_special(missing_subjects_row, by_labels, by_data_vars, tv, row_labels)
  }
  if (!is.null(total_result)) {
    build_row_labels_special(total_result, by_labels, by_data_vars, tv, row_labels)
  }

  # --- Combine main counts + missing + missing_subjects + total ---
  if (!is.null(missing_row)) {
    missing_row <- missing_row[, names(counts), with = FALSE]
    counts <- data.table::rbindlist(list(counts, missing_row), use.names = TRUE, fill = TRUE)
  }
  if (!is.null(missing_subjects_row)) {
    missing_subjects_row <- missing_subjects_row[, names(counts), with = FALSE]
    counts <- data.table::rbindlist(list(counts, missing_subjects_row), use.names = TRUE, fill = TRUE)
  }
  if (!is.null(total_result)) {
    total_result <- total_result[, names(counts), with = FALSE]
    counts <- data.table::rbindlist(list(counts, total_result), use.names = TRUE, fill = TRUE)
  }

  # --- Compute sort keys before cast ---
  compute_count_sort_keys(counts, dt, cols, by_data_vars, tv, settings)

  # --- dcast to wide format ---
  wide <- cast_to_wide(counts, row_labels, cols, layer_index, col_n = col_n)

  # --- Override ord1 with computed sort keys ---
  method <- settings$order_count_method
  if (!is.null(method)) {
    # Find the rowlabel column that holds the target variable values
    tv_label_idx <- length(by_labels) + length(by_data_vars) + 1L
    tv_label_col <- paste0("rowlabel", tv_label_idx)
    if (tv_label_col %in% names(wide)) {
      tv_vals <- wide[[tv_label_col]]
      if (method == "bycount") {
        # For count-based, use the pre-computed ord1 from cast_to_wide
        # (which is seq_len(.N) based on dcast order)
        # We need to sum counts across columns for ordering
        # Use ord1 as-is (already reflects the dcast order which respects .sort_key)
        # No override needed for bycount — handle via pre-sort below
      } else {
        wide[, ord1 := compute_var_order(
          tv_vals, var_name = tv, data_dt = dt, method = method
        )]
      }
    }
  }

  # --- Merge risk difference columns onto wide result ---
  if (!is.null(rd_data)) {
    wide <- merge_risk_diff_columns(
      wide, rd_data, settings$risk_diff,
      row_labels, tv, by_data_vars
    )
  }

  # Attach numeric data snapshot
  data.table::setattr(wide, "numeric_data", as.data.frame(numeric_snapshot))

  wide
}

# =============================================================================
# Nested count layer (Phase 3)
# =============================================================================

#' Process a nested (multi-variable) count layer
#' @keywords internal
build_count_layer_nested <- function(dt, target_vars, cols, by_data_vars, by_labels,
                                      settings, layer_index, col_n = NULL, pop_dt = NULL) {
  n_levels <- length(target_vars)
  indentation <- settings$indentation %||% "  "
  distinct_by <- settings$distinct_by
  denom_where <- settings$denom_where
  denom_ignore <- settings$denom_ignore
  total_row <- settings$total_row %||% FALSE
  total_label <- settings$total_row_label %||% "Total"
  total_missings <- settings$total_row_count_missings %||% TRUE
  missing_count <- settings$missing_count
  keep_levels <- settings$keep_levels
  limit_data_by <- settings$limit_data_by
  fmt <- get_count_format(settings)

  # Prepare denominator dataset (shared across levels)
  denom_dt <- data.table::copy(pop_dt %||% dt)
  if (!is.null(denom_where) && !identical(denom_where, TRUE)) {
    denom_dt <- denom_dt[eval(denom_where)]
  }
  if (!is.null(denom_ignore)) {
    # Ignore values in the outermost target variable
    denom_dt <- denom_dt[!get(target_vars[1]) %in% denom_ignore]
  }

  # Total number of rowlabel columns needed
  n_label_cols <- length(by_labels) + length(by_data_vars) + n_levels

  # Process each nesting level
  level_results <- vector("list", n_levels)

  for (level in seq_len(n_levels)) {
    level_tvs <- target_vars[1:level]
    tv <- target_vars[level]

    # Group by cols + by_data_vars + all target vars up to this level
    group_vars <- c(cols, by_data_vars, level_tvs)

    # Count
    counts <- dt[, list(n = .N), by = group_vars]

    # Distinct counting
    if (!is.null(distinct_by)) {
      distinct_counts <- dt[, list(distinct_n = uniqueN(get(distinct_by))), by = group_vars]
      counts <- merge(counts, distinct_counts, by = group_vars, all.x = TRUE)
    }

    # Denominator for this level
    denom_group <- get_nested_denom_group(settings$denoms_by, level, cols)

    if (length(denom_group) > 0) {
      denoms <- denom_dt[, list(total = .N), by = denom_group]
      counts <- merge(counts, denoms, by = intersect(denom_group, names(counts)), all.x = TRUE)
    } else {
      counts[, total := nrow(denom_dt)]
    }
    counts[, pct := ifelse(total > 0, n / total * 100, 0)]

    # Distinct denominators
    if (!is.null(distinct_by)) {
      if (length(denom_group) > 0) {
        distinct_denoms <- denom_dt[, list(distinct_total = uniqueN(get(distinct_by))), by = denom_group]
        counts <- merge(counts, distinct_denoms, by = intersect(denom_group, names(counts)), all.x = TRUE)
      } else {
        counts[, distinct_total := uniqueN(denom_dt[[distinct_by]])]
      }
      counts[, distinct_pct := ifelse(distinct_total > 0, distinct_n / distinct_total * 100, 0)]
    }

    # Data completion
    counts <- complete_nested_level(counts, dt, cols, by_data_vars, level_tvs,
                                     limit_data_by, denom_group)

    # Format
    fmt_args <- lapply(fmt$vars, function(v) counts[[v]])
    counts[, formatted := do.call(apply_formats, c(list(fmt), fmt_args))]

    # Build row labels for this level
    build_nested_row_labels(counts, by_labels, by_data_vars, target_vars,
                            level, n_levels, indentation)

    # Track nesting level for ordering
    counts[, .nest_level := level]

    level_results[[level]] <- counts
  }

  # Combine all levels
  combined <- data.table::rbindlist(level_results, use.names = TRUE, fill = TRUE)

  # Fill NA rowlabel columns with ""
  for (i in seq_len(n_label_cols)) {
    col_name <- paste0("rowlabel", i)
    if (col_name %in% names(combined)) {
      combined[is.na(get(col_name)), (col_name) := ""]
    }
  }

  # Apply keep_levels (filter on outermost target variable)
  if (!is.null(keep_levels)) {
    combined <- combined[get(target_vars[1]) %in% keep_levels]
  }

  # Sort for correct interleaving: outer value, then level, then inner value
  sort_nested(combined, target_vars, by_data_vars)

  # Handle total row (outermost level only)
  if (total_row) {
    outer_tv <- target_vars[1]
    total_result <- compute_total_row(
      combined[.nest_level == 1], dt, cols, by_data_vars, outer_tv, total_label,
      total_missings, distinct_by, missing_count,
      get_nested_denom_group(settings$denoms_by, 1, cols), denom_dt
    )
    fmt_args_t <- lapply(fmt$vars, function(v) total_result[[v]])
    total_result[, formatted := do.call(apply_formats, c(list(fmt), fmt_args_t))]

    # Build row labels for total row
    build_nested_row_labels_special(total_result, by_labels, by_data_vars,
                                     target_vars, outer_tv, n_label_cols)
    total_result[, .nest_level := 0L]

    # Align columns and append
    shared_cols <- intersect(names(combined), names(total_result))
    total_result <- total_result[, shared_cols, with = FALSE]
    combined <- data.table::rbindlist(list(combined, total_result), use.names = TRUE, fill = TRUE)
  }

  # Handle missing row (outermost level only)
  if (!is.null(missing_count)) {
    outer_tv <- target_vars[1]
    group_vars <- c(cols, by_data_vars, outer_tv)
    missing_result <- compute_missing_counts(
      dt, combined[.nest_level == 1], cols, by_data_vars, outer_tv, group_vars,
      get_nested_denom_group(settings$denoms_by, 1, cols), denom_dt,
      distinct_by, missing_count
    )
    missing_row <- missing_result$missing_row

    fmt_args_m <- lapply(fmt$vars, function(v) missing_row[[v]])
    missing_row[, formatted := do.call(apply_formats, c(list(fmt), fmt_args_m))]

    build_nested_row_labels_special(missing_row, by_labels, by_data_vars,
                                     target_vars, outer_tv, n_label_cols)
    missing_row[, .nest_level := 0L]

    shared_cols <- intersect(names(combined), names(missing_row))
    missing_row <- missing_row[, shared_cols, with = FALSE]
    combined <- data.table::rbindlist(list(combined, missing_row), use.names = TRUE, fill = TRUE)
  }

  # Fill any remaining NA rowlabel columns
  for (i in seq_len(n_label_cols)) {
    col_name <- paste0("rowlabel", i)
    if (col_name %in% names(combined)) {
      combined[is.na(get(col_name)), (col_name) := ""]
    }
  }

  # --- Capture numeric data before casting ---
  numeric_snapshot <- data.table::copy(combined)

  # Cast to wide
  row_label_cols <- paste0("rowlabel", seq_len(n_label_cols))
  wide <- cast_to_wide(combined, row_label_cols, cols, layer_index, col_n = col_n)

  # Add ord2 for nesting depth
  if (".nest_level" %in% names(combined)) {
    # ord2 is already captured in the interleaved sort; add as depth indicator
    # After dcast, reconstruct from the combined data order
    nest_levels <- combined[, .(.nest_level = .nest_level[1]),
                             by = row_label_cols]
    wide <- merge(wide, nest_levels, by = row_label_cols, all.x = TRUE, sort = FALSE)
    data.table::setnames(wide, ".nest_level", "ord2")
  }

  # Attach numeric data snapshot
  data.table::setattr(wide, "numeric_data", as.data.frame(numeric_snapshot))

  wide
}

#' Resolve denominator group for a nesting level
#' @keywords internal
get_nested_denom_group <- function(denoms_by, level, cols) {
  if (is.null(denoms_by)) {
    return(if (length(cols) > 0) cols else character(0))
  }

  # List form: per-level configuration
  if (is.list(denoms_by) && length(denoms_by) > 0 && is.character(denoms_by[[1]])) {
    idx <- min(level, length(denoms_by))
    return(denoms_by[[idx]])
  }

  # Simple character vector: same for all levels
  denoms_by
}

#' Build row labels for a nested count level
#' @keywords internal
build_nested_row_labels <- function(counts, by_labels, by_data_vars, target_vars,
                                     level, n_levels, indentation) {
  col_idx <- 1L

  # by_labels (constant text)
  for (lbl in by_labels) {
    col_name <- paste0("rowlabel", col_idx)
    counts[, (col_name) := lbl]
    col_idx <- col_idx + 1L
  }

  # by_data_vars
  for (bv in by_data_vars) {
    col_name <- paste0("rowlabel", col_idx)
    counts[, (col_name) := as.character(get(bv))]
    col_idx <- col_idx + 1L
  }

  # Target variable columns
  for (tv_idx in seq_along(target_vars)) {
    col_name <- paste0("rowlabel", col_idx)

    if (tv_idx <= level) {
      # This target var is in the grouping — populate with its value
      val <- as.character(counts[[target_vars[tv_idx]]])
      if (tv_idx == level && level > 1) {
        # Apply indentation to the current level (not the outermost)
        indent_str <- strrep(indentation, level - 1)
        val <- paste0(indent_str, val)
      }
      counts[, (col_name) := val]
    } else {
      # Deeper than current level — blank
      counts[, (col_name) := ""]
    }

    col_idx <- col_idx + 1L
  }

  invisible(counts)
}

#' Build row labels for special rows (total/missing) in nested context
#' @keywords internal
build_nested_row_labels_special <- function(dt, by_labels, by_data_vars,
                                             target_vars, tv, n_label_cols) {
  col_idx <- 1L

  # by_labels
  for (lbl in by_labels) {
    col_name <- paste0("rowlabel", col_idx)
    dt[, (col_name) := lbl]
    col_idx <- col_idx + 1L
  }

  # by_data_vars (empty for special rows)
  for (bv in by_data_vars) {
    col_name <- paste0("rowlabel", col_idx)
    dt[, (col_name) := ""]
    col_idx <- col_idx + 1L
  }

  # First target var column gets the label (e.g. "Total")
  col_name <- paste0("rowlabel", col_idx)
  dt[, (col_name) := as.character(get(tv))]
  col_idx <- col_idx + 1L

  # Remaining target var columns are blank
  while (col_idx <= n_label_cols) {
    col_name <- paste0("rowlabel", col_idx)
    dt[, (col_name) := ""]
    col_idx <- col_idx + 1L
  }

  invisible(dt)
}

#' Sort nested count data for correct interleaving
#' @keywords internal
sort_nested <- function(combined, target_vars, by_data_vars) {
  n_levels <- length(target_vars)

  # Compute sort keys for each level
  # Outer rank: alphabetical (or factor) position of outermost target var
  outer_vals <- sort(unique(combined[[target_vars[1]]]))
  combined[, .sort_outer := match(get(target_vars[1]), outer_vals)]

  if (n_levels >= 2) {
    # Inner rank: position within each outer group
    combined[, .sort_inner := 0L]
    inner_rows <- combined[.nest_level >= 2]
    if (nrow(inner_rows) > 0) {
      inner_rows[, .sort_inner := data.table::frank(
        get(target_vars[2]), ties.method = "dense"
      ), by = c(by_data_vars, target_vars[1])]
      # Update combined via join
      join_cols <- intersect(names(combined), names(inner_rows))
      join_cols <- setdiff(join_cols, ".sort_inner")
      combined[inner_rows, .sort_inner := i..sort_inner, on = join_cols]
    }
  }

  # Sort: by outer rank, then nest_level (1 before 2), then inner rank
  sort_cols <- ".sort_outer"
  if (length(by_data_vars) > 0) {
    sort_cols <- c(by_data_vars, sort_cols)
  }
  sort_cols <- c(sort_cols, ".nest_level")
  if (n_levels >= 2) {
    sort_cols <- c(sort_cols, ".sort_inner")
  }
  data.table::setorderv(combined, sort_cols)

  # Clean up sort columns
  combined[, .sort_outer := NULL]
  if (".sort_inner" %in% names(combined)) {
    combined[, .sort_inner := NULL]
  }

  invisible(combined)
}

#' Complete counts for a nested level
#' @keywords internal
complete_nested_level <- function(counts, dt, cols, by_data_vars, level_tvs,
                                   limit_data_by = NULL, denom_group = NULL) {
  # For inner levels (>1 target var), use actual parent-child relationships
  # For outer level (1 target var), use full cross-join (same as complete_counts)
  if (length(level_tvs) == 1) {
    return(complete_counts(counts, dt, cols, by_data_vars, level_tvs[1],
                            limit_data_by, denom_group))
  }

  # Inner level: build grid from actual target var combinations in data
  non_tv_vars <- list()
  for (col in cols) {
    vals <- sort(unique(dt[[col]]))
    if (is.factor(dt[[col]])) vals <- levels(dt[[col]])
    non_tv_vars[[col]] <- vals
  }
  for (bv in by_data_vars) {
    vals <- sort(unique(dt[[bv]]))
    if (is.factor(dt[[bv]])) vals <- levels(dt[[bv]])
    non_tv_vars[[bv]] <- vals
  }

  # Actual parent-child combinations from data
  tv_combos <- unique(dt[, level_tvs, with = FALSE])

  if (length(non_tv_vars) > 0) {
    non_tv_grid <- do.call(data.table::CJ, non_tv_vars)
    # Cross non-tv grid with tv combos
    non_tv_grid[, .join_key := 1L]
    tv_combos[, .join_key := 1L]
    grid <- merge(non_tv_grid, tv_combos, by = ".join_key", allow.cartesian = TRUE)
    grid[, .join_key := NULL]
  } else {
    grid <- tv_combos
  }

  if (nrow(grid) == 0) return(counts)

  result <- merge(grid, counts, by = names(grid), all.x = TRUE)

  # Fill NAs with 0
  for (v in c("n", "pct", "distinct_n", "distinct_pct")) {
    if (v %in% names(result)) {
      data.table::set(result, which(is.na(result[[v]])), v, 0)
    }
  }

  # Fill total and distinct_total from existing data
  denom_vars <- if (!is.null(denom_group) && length(denom_group) > 0) {
    intersect(denom_group, names(result))
  } else if (length(cols) > 0) {
    cols
  } else {
    character(0)
  }
  if ("total" %in% names(result) && length(denom_vars) > 0) {
    denom_dt <- unique(counts[!is.na(total), .SD, .SDcols = c(denom_vars, "total")])
    result[, total := NULL]
    result <- merge(result, denom_dt, by = denom_vars, all.x = TRUE)
  }
  if ("distinct_total" %in% names(result) && length(denom_vars) > 0) {
    dist_denom_dt <- unique(counts[!is.na(distinct_total), .SD, .SDcols = c(denom_vars, "distinct_total")])
    result[, distinct_total := NULL]
    result <- merge(result, dist_denom_dt, by = denom_vars, all.x = TRUE)
  }

  result
}

# =============================================================================
# Shared helpers (used by both single and nested paths)
# =============================================================================

#' Compute missing count row
#' @keywords internal
compute_missing_counts <- function(dt, counts, cols, by_data_vars, tv, group_vars,
                                   denom_group, denom_dt, distinct_by, missing_count) {
  missing_values <- missing_count$missing_values %||% character(0)
  denom_exclude <- missing_count$denom_exclude %||% FALSE
  sort_value <- missing_count$sort_value %||% Inf
  missing_label <- missing_count$label %||% "Missing"

  # Identify missing rows in the original data
  is_missing <- is.na(dt[[tv]])
  if (length(missing_values) > 0) {
    is_missing <- is_missing | dt[[tv]] %in% missing_values
  }

  missing_dt <- dt[is_missing]
  summary_group <- c(cols, by_data_vars)

  if (length(summary_group) > 0) {
    missing_n <- missing_dt[, list(n = .N), by = summary_group]
  } else {
    missing_n <- data.table::data.table(n = nrow(missing_dt))
  }

  # Add target var column with the missing label
  missing_n[, (tv) := missing_label]

  # Merge in denominator totals
  if (length(denom_group) > 0) {
    denoms <- denom_dt[, list(total = .N), by = denom_group]
    missing_n <- merge(missing_n, denoms, by = intersect(denom_group, names(missing_n)), all.x = TRUE)
  } else {
    missing_n[, total := nrow(denom_dt)]
  }
  missing_n[, pct := ifelse(total > 0, n / total * 100, 0)]

  # Distinct counting for missing
  if (!is.null(distinct_by)) {
    if (length(summary_group) > 0) {
      missing_distinct <- missing_dt[, list(distinct_n = uniqueN(get(distinct_by))), by = summary_group]
    } else {
      missing_distinct <- data.table::data.table(distinct_n = uniqueN(missing_dt[[distinct_by]]))
    }
    missing_n <- merge(missing_n, missing_distinct,
                       by = intersect(summary_group, names(missing_distinct)), all.x = TRUE)
    if (length(denom_group) > 0) {
      distinct_denoms <- denom_dt[, list(distinct_total = uniqueN(get(distinct_by))), by = denom_group]
      missing_n <- merge(missing_n, distinct_denoms,
                         by = intersect(denom_group, names(missing_n)), all.x = TRUE)
    } else {
      missing_n[, distinct_total := uniqueN(denom_dt[[distinct_by]])]
    }
    missing_n[, distinct_pct := ifelse(distinct_total > 0, distinct_n / distinct_total * 100, 0)]
  }

  # Fill NAs with 0
  for (v in c("n", "pct", "distinct_n", "distinct_pct")) {
    if (v %in% names(missing_n)) {
      data.table::set(missing_n, which(is.na(missing_n[[v]])), v, 0)
    }
  }

  # Store sort value
  missing_n[, .missing_sort := sort_value]

  list(counts = counts, missing_row = missing_n)
}

#' Compute missing subjects row
#'
#' Counts subjects present in pop_data but absent from target data.
#' Uses `distinct_by` to identify subjects; if NULL, uses row-level counts.
#'
#' @param dt data.table with target data (after layer where filter)
#' @param pop_dt data.table with population data
#' @param cols Character vector of spec column variable names
#' @param by_data_vars Character vector of by-variable names from data
#' @param tv Character string, target variable name
#' @param distinct_by Character string naming the subject identifier (or NULL)
#' @param missing_label Character string for the row label
#' @param denom_group Character vector of denominator grouping variables
#' @param denom_dt data.table for denominator computation
#' @param fmt f_str object for formatting
#'
#' @return A data.table for the missing subjects row, or NULL
#' @keywords internal
compute_missing_subjects <- function(dt, pop_dt, cols, by_data_vars, tv,
                                      distinct_by, missing_label, denom_group,
                                      denom_dt, fmt) {
  if (is.null(pop_dt)) return(NULL)

  summary_group <- c(cols, by_data_vars)
  subj_var <- distinct_by

  if (!is.null(subj_var)) {
    # Subject-level anti-join
    if (length(summary_group) > 0) {
      pop_subjs <- unique(pop_dt[, c(summary_group, subj_var), with = FALSE])
      target_subjs <- unique(dt[, c(summary_group, subj_var), with = FALSE])

      # Anti-join: pop subjects not in target
      missing_subjs <- data.table::fsetdiff(pop_subjs, target_subjs)

      if (nrow(missing_subjs) > 0) {
        missing_n <- missing_subjs[, list(n = .N, distinct_n = .N), by = summary_group]
      } else {
        # Build zero-count rows per group
        if (length(summary_group) > 0) {
          groups <- unique(pop_dt[, summary_group, with = FALSE])
          missing_n <- groups[, list(n = 0L, distinct_n = 0L), by = summary_group]
        } else {
          missing_n <- data.table::data.table(n = 0L, distinct_n = 0L)
        }
      }
    } else {
      pop_ids <- unique(pop_dt[[subj_var]])
      target_ids <- unique(dt[[subj_var]])
      n_missing <- length(setdiff(pop_ids, target_ids))
      missing_n <- data.table::data.table(n = n_missing, distinct_n = n_missing)
    }
  } else {
    # Row-level: count rows in pop not in target (less precise)
    if (length(summary_group) > 0) {
      pop_counts <- pop_dt[, list(pop_n = .N), by = summary_group]
      target_counts <- dt[, list(target_n = .N), by = summary_group]
      merged <- merge(pop_counts, target_counts, by = summary_group, all.x = TRUE)
      merged[is.na(target_n), target_n := 0L]
      merged[, n := pmax(pop_n - target_n, 0L)]
      missing_n <- merged[, c(summary_group, "n"), with = FALSE]
    } else {
      n_missing <- max(nrow(pop_dt) - nrow(dt), 0L)
      missing_n <- data.table::data.table(n = n_missing)
    }
  }

  # Add target var column with missing label
  missing_n[, (tv) := missing_label]

  # Merge in denominator totals
  if (length(denom_group) > 0) {
    denoms <- denom_dt[, list(total = .N), by = denom_group]
    missing_n <- merge(missing_n, denoms,
                       by = intersect(denom_group, names(missing_n)), all.x = TRUE)
  } else {
    missing_n[, total := nrow(denom_dt)]
  }
  missing_n[, pct := ifelse(!is.na(total) & total > 0, n / total * 100, 0)]

  # Distinct denominators if applicable
  if (!is.null(subj_var)) {
    if (length(denom_group) > 0) {
      distinct_denoms <- denom_dt[, list(distinct_total = uniqueN(get(subj_var))),
                                   by = denom_group]
      missing_n <- merge(missing_n, distinct_denoms,
                         by = intersect(denom_group, names(missing_n)), all.x = TRUE)
    } else {
      missing_n[, distinct_total := uniqueN(denom_dt[[subj_var]])]
    }
    missing_n[, distinct_pct := ifelse(!is.na(distinct_total) & distinct_total > 0,
                                        distinct_n / distinct_total * 100, 0)]
  }

  # Fill NAs with 0
  for (v in c("n", "pct", "distinct_n", "distinct_pct")) {
    if (v %in% names(missing_n)) {
      data.table::set(missing_n, which(is.na(missing_n[[v]])), v, 0)
    }
  }

  # Sort value: after missing counts, before total
  missing_n[, .missing_sort := Inf - 1]

  missing_n
}

#' Compute total row
#' @keywords internal
compute_total_row <- function(counts, dt, cols, by_data_vars, tv, total_label,
                              total_missings, distinct_by, missing_count,
                              denom_group, denom_dt) {
  summary_group <- c(cols, by_data_vars)
  src <- counts

  # Exclude missing values from total if requested
  if (!total_missings && !is.null(missing_count)) {
    missing_values <- missing_count$missing_values %||% character(0)
    missing_label <- missing_count$label %||% "Missing"
    src <- src[!get(tv) %in% c(missing_values, missing_label)]
    src <- src[!is.na(get(tv))]
  }

  if (length(summary_group) > 0) {
    total_dt <- src[, list(n = sum(n, na.rm = TRUE), total = total[1]), by = summary_group]
  } else {
    total_dt <- data.table::data.table(
      n = sum(src$n, na.rm = TRUE),
      total = src$total[1]
    )
  }

  # For distinct counts, compute from raw data (summing per-level can double-count)
  if (!is.null(distinct_by)) {
    raw <- dt
    if (!total_missings && !is.null(missing_count)) {
      missing_values <- missing_count$missing_values %||% character(0)
      raw <- raw[!is.na(get(tv)) & !get(tv) %in% missing_values]
    }
    if (length(summary_group) > 0) {
      distinct_totals <- raw[, list(distinct_n = uniqueN(get(distinct_by))), by = summary_group]
      total_dt <- merge(total_dt, distinct_totals, by = summary_group, all.x = TRUE)
    } else {
      total_dt[, distinct_n := uniqueN(raw[[distinct_by]])]
    }
    # Merge distinct denominators
    if (length(denom_group) > 0) {
      distinct_denoms <- denom_dt[, list(distinct_total = uniqueN(get(distinct_by))), by = denom_group]
      merge_by <- intersect(denom_group, names(total_dt))
      total_dt <- merge(total_dt, distinct_denoms, by = merge_by, all.x = TRUE)
    } else {
      total_dt[, distinct_total := uniqueN(denom_dt[[distinct_by]])]
    }
    total_dt[is.na(distinct_n), distinct_n := 0]
    total_dt[, distinct_pct := ifelse(distinct_total > 0, distinct_n / distinct_total * 100, 0)]
  }

  total_dt[, (tv) := total_label]
  total_dt[, pct := ifelse(total > 0, n / total * 100, 0)]

  total_dt[, .total_sort := Inf]

  total_dt
}

#' Build row labels for special rows (missing, total) in single-variable context
#' @keywords internal
build_row_labels_special <- function(dt, by_labels, by_data_vars, tv, existing_label_cols) {
  col_idx <- 1L
  n_label_cols <- length(existing_label_cols)

  # Add label columns (same constant labels as main rows)
  for (lbl in by_labels) {
    col_name <- paste0("rowlabel", col_idx)
    if (!col_name %in% names(dt)) {
      dt[, (col_name) := lbl]
    }
    col_idx <- col_idx + 1L
  }

  # Add by data variable columns (use empty string for special rows if not present)
  for (bv in by_data_vars) {
    col_name <- paste0("rowlabel", col_idx)
    if (!col_name %in% names(dt)) {
      dt[, (col_name) := ""]
    }
    col_idx <- col_idx + 1L
  }

  # The last rowlabel should be the target variable value (e.g., "Total" or "Missing")
  # which is stored in the tv column of the data.table
  if (col_idx <= n_label_cols) {
    col_name <- paste0("rowlabel", col_idx)
    dt[, (col_name) := as.character(get(tv))]
    col_idx <- col_idx + 1L
  }

  # Fill any remaining label columns with empty string
  while (col_idx <= n_label_cols) {
    col_name <- paste0("rowlabel", col_idx)
    if (!col_name %in% names(dt)) {
      dt[, (col_name) := ""]
    }
    col_idx <- col_idx + 1L
  }

  invisible(dt)
}

#' Complete count data to ensure all combinations exist
#' @keywords internal
complete_counts <- function(counts, dt, cols, by_data_vars, tv, limit_data_by = NULL,
                            denom_group = NULL) {
  # Build complete grid of all combinations
  grid_vars <- list()

  for (col in cols) {
    vals <- sort(unique(dt[[col]]))
    if (is.factor(dt[[col]])) vals <- levels(dt[[col]])
    grid_vars[[col]] <- vals
  }

  for (bv in by_data_vars) {
    vals <- sort(unique(dt[[bv]]))
    if (is.factor(dt[[bv]])) vals <- levels(dt[[bv]])
    grid_vars[[bv]] <- vals
  }

  vals <- sort(unique(dt[[tv]]))
  if (is.factor(dt[[tv]])) vals <- levels(dt[[tv]])
  grid_vars[[tv]] <- vals

  if (length(grid_vars) == 0) return(counts)

  grid <- do.call(data.table::CJ, grid_vars)

  # Apply limit_data_by: restrict grid to combinations that exist in data
  if (!is.null(limit_data_by)) {
    limit_cols <- intersect(limit_data_by, names(grid))
    if (length(limit_cols) > 0) {
      existing <- unique(dt[, limit_cols, with = FALSE])
      grid <- merge(grid, existing, by = limit_cols)
    }
  }

  result <- merge(grid, counts, by = names(grid_vars), all.x = TRUE)

  # Fill NAs with 0 for count variables
  for (v in c("n", "pct", "distinct_n", "distinct_pct")) {
    if (v %in% names(result)) {
      data.table::set(result, which(is.na(result[[v]])), v, 0)
    }
  }

  # Fill total and distinct_total from existing data
  # Use denom_group if provided, otherwise fall back to cols
  denom_vars <- if (!is.null(denom_group) && length(denom_group) > 0) {
    intersect(denom_group, names(result))
  } else if (length(cols) > 0) {
    cols
  } else {
    character(0)
  }
  if ("total" %in% names(result) && length(denom_vars) > 0) {
    denom_dt <- unique(counts[!is.na(total), .SD, .SDcols = c(denom_vars, "total")])
    result[, total := NULL]
    result <- merge(result, denom_dt, by = denom_vars, all.x = TRUE)
  }
  if ("distinct_total" %in% names(result) && length(denom_vars) > 0) {
    dist_denom_dt <- unique(counts[!is.na(distinct_total), .SD, .SDcols = c(denom_vars, "distinct_total")])
    result[, distinct_total := NULL]
    result <- merge(result, dist_denom_dt, by = denom_vars, all.x = TRUE)
  }

  result
}

#' Get the count format string, falling back to defaults
#' @keywords internal
get_count_format <- function(settings) {
  if (!is.null(settings$format_strings)) {
    # Return n_counts format, or the first one
    if ("n_counts" %in% names(settings$format_strings)) {
      return(settings$format_strings[["n_counts"]])
    }
    return(settings$format_strings[[1]])
  }

  # Default format
  f_str("xx (xx.x%)", "n", "pct")
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

#' Build row label columns for count layers
#' @keywords internal
build_row_labels_count <- function(counts, by_labels, by_data_vars, tv) {
  label_cols <- list()
  col_idx <- 1L

  # Add label columns
  for (lbl in by_labels) {
    col_name <- paste0("rowlabel", col_idx)
    counts[, (col_name) := lbl]
    label_cols[[col_name]] <- col_name
    col_idx <- col_idx + 1L
  }

  # Add by data variable columns as row labels
  for (bv in by_data_vars) {
    col_name <- paste0("rowlabel", col_idx)
    counts[, (col_name) := as.character(get(bv))]
    label_cols[[col_name]] <- col_name
    col_idx <- col_idx + 1L
  }

  # Add target variable as row label
  col_name <- paste0("rowlabel", col_idx)
  counts[, (col_name) := as.character(get(tv))]
  label_cols[[col_name]] <- col_name

  names(label_cols)
}

#' Cast long data to wide output format
#' @keywords internal
cast_to_wide <- function(dt, row_label_cols, cols, layer_index, col_n = NULL) {
  # Track column value labels for metadata
  col_labels <- NULL

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
    # No column variables - single result column
    wide <- dt[, c(row_label_cols, "formatted"), with = FALSE]
    data.table::setnames(wide, "formatted", "res1")
  } else {
    # Build dcast formula: row_labels ~ cols
    lhs <- paste(row_label_cols, collapse = " + ")

    if (length(cols) == 1) {
      rhs <- cols[1]
    } else {
      # For multiple cols, create interaction column
      dt[, .col_combo := do.call(paste, c(.SD, sep = " | ")), .SDcols = cols]
      rhs <- ".col_combo"
    }

    formula_str <- paste(lhs, "~", rhs)
    wide <- data.table::dcast(
      dt,
      as.formula(formula_str),
      value.var = "formatted",
      fill = ""
    )

    # Rename value columns from dcast names to res1, res2, ...
    val_cols <- setdiff(names(wide), row_label_cols)
    col_labels <- build_col_labels(val_cols, col_n)
    new_names <- paste0("res", seq_along(val_cols))
    data.table::setnames(wide, val_cols, new_names)

    # Clean up temp column
    if (".col_combo" %in% names(dt)) {
      dt[, .col_combo := NULL]
    }
  }

  # Add ordering columns
  wide[, ordindx := layer_index]
  wide[, ord1 := seq_len(.N)]

  # Attach label attributes to result columns
  if (!is.null(col_labels)) {
    res_cols <- paste0("res", seq_along(col_labels))
    for (i in seq_along(res_cols)) {
      data.table::setattr(wide[[res_cols[i]]], "label", col_labels[i])
    }
  }

  wide
}

#' Build column labels with header N suffix
#'
#' Takes raw dcast column names and a col_n data.table, returns labels
#' with "(N=<n>)" suffix. For shift layers where the label includes both
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
    combo_strings <- do.call(paste, c(col_n[, col_n_vars, with = FALSE], sep = " | "))
    n_lookup <- setNames(col_n$.n, combo_strings)
  }

  vapply(raw_labels, function(lbl) {
    # Split label; use first length(col_n_vars) parts for N lookup
    parts <- strsplit(lbl, " | ", fixed = TRUE)[[1]]
    if (length(parts) > length(col_n_vars)) {
      key <- paste(parts[seq_len(length(col_n_vars))], collapse = " | ")
    } else {
      key <- lbl
    }
    n_val <- n_lookup[key]
    if (!is.na(n_val)) paste0(lbl, " (N=", n_val, ")") else lbl
  }, character(1), USE.NAMES = FALSE)
}
