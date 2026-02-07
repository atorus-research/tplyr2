#' Process a descriptive statistics layer
#'
#' @param dt data.table with the (filtered) input data
#' @param layer A tplyr_desc_layer object
#' @param cols Character vector of column variable names from the spec
#' @param layer_index Integer index of this layer
#'
#' @return A data.table with rowlabel*, res*, and ord* columns
#' @keywords internal
build_desc_layer <- function(dt, layer, cols, layer_index, col_n = NULL, pop_dt = NULL) {
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

  if (length(target_var) == 1) {
    # Single target variable
    result <- build_desc_single(
      dt, target_var[1], cols, by_data_vars, by_labels,
      settings, layer_index, col_n, pop_dt = pop_dt
    )
  } else {
    # Multiple target variables
    result <- build_desc_multi(
      dt, target_var, cols, by_data_vars, by_labels,
      settings, layer_index, col_n, pop_dt = pop_dt
    )
  }

  # --- Stats as columns transposition ---
  if (isTRUE(settings$stats_as_columns)) {
    result <- transpose_stats_to_columns(result)
  }

  result
}

#' Build a single-target desc layer
#' @keywords internal
build_desc_single <- function(dt, tv, cols, by_data_vars, by_labels,
                               settings, layer_index, col_n,
                               var_label = NULL, var_index = NULL,
                               pop_dt = NULL) {
  # Group variables
  group_vars <- c(cols, by_data_vars)

  # Compute all built-in statistics
  qtype <- getOption("tplyr2.quantile_type", 7)

  stats <- dt[, {
    v <- get(tv)
    v_clean <- v[!is.na(v)]
    v_finite <- v_clean[is.finite(v_clean)]
    list(
      n       = length(v_clean),
      mean    = if (length(v_clean) > 0) mean(v_clean, na.rm = TRUE) else NA_real_,
      sd      = if (length(v_clean) > 1) sd(v_clean, na.rm = TRUE) else NA_real_,
      median  = if (length(v_clean) > 0) median(v_clean, na.rm = TRUE) else NA_real_,
      var     = if (length(v_clean) > 1) var(v_clean, na.rm = TRUE) else NA_real_,
      min     = if (length(v_finite) > 0) min(v_finite) else NA_real_,
      max     = if (length(v_finite) > 0) max(v_finite) else NA_real_,
      iqr     = if (length(v_clean) > 0) IQR(v_clean, type = qtype) else NA_real_,
      q1      = if (length(v_clean) > 0) unname(quantile(v_clean, 0.25, type = qtype)) else NA_real_,
      q3      = if (length(v_clean) > 0) unname(quantile(v_clean, 0.75, type = qtype)) else NA_real_,
      missing = sum(is.na(v))
    )
  }, by = group_vars]

  # --- Custom summaries ---
  all_custom <- getOption("tplyr2.custom_summaries", list())
  if (!is.null(settings$custom_summaries)) {
    for (nm in names(settings$custom_summaries)) {
      all_custom[[nm]] <- settings$custom_summaries[[nm]]
    }
  }

  if (length(all_custom) > 0) {
    custom_stats <- dt[, {
      .var <- get(tv)
      result <- lapply(all_custom, function(expr) {
        tryCatch(eval(expr), error = function(e) NA_real_)
      })
      result
    }, by = group_vars]

    # Merge custom stats — overwrite built-ins if names collide
    overlap <- intersect(names(custom_stats), setdiff(names(stats), group_vars))
    for (col in overlap) {
      stats[, (col) := custom_stats[[col]]]
    }
    non_overlap <- setdiff(names(custom_stats), c(group_vars, overlap))
    if (length(non_overlap) > 0) {
      stats <- merge(stats, custom_stats[, c(group_vars, non_overlap), with = FALSE],
                     by = group_vars, all.x = TRUE)
    }
  }

  # --- Denominators for desc layer ---
  denom_group <- settings$denoms_by
  if (is.null(denom_group) && length(cols) > 0) {
    denom_group <- cols
  }

  if (length(denom_group) > 0) {
    denom_base <- data.table::copy(pop_dt %||% dt)
    if (!is.null(settings$denom_where) && !identical(settings$denom_where, TRUE)) {
      denom_base <- denom_base[eval(settings$denom_where)]
    }
    denoms <- denom_base[, list(total = .N), by = denom_group]
    stats <- merge(stats, denoms,
                   by = intersect(denom_group, names(stats)), all.x = TRUE)
  } else {
    denom_base <- pop_dt %||% dt
    stats[, total := nrow(denom_base)]
  }
  stats[, pct := ifelse(!is.na(total) & total > 0, n / total * 100, NA_real_)]

  # --- Capture numeric data before formatting ---
  numeric_snapshot <- data.table::copy(stats)

  # Get format strings
  format_strings <- get_desc_formats(settings)

  # --- Auto-precision ---
  needs_precision <- any(vapply(format_strings, function(fmt) {
    any(vapply(fmt$parsed$groups, function(g) {
      g$int$auto || (g$has_decimal && g$dec$auto)
    }, logical(1)))
  }, logical(1)))

  precision_table <- NULL
  if (needs_precision) {
    prec_by <- settings$precision_by %||% character(0)
    prec_on <- settings$precision_on %||% tv
    precision_table <- collect_precision(
      dt, prec_on, prec_by, settings$precision_data, settings$precision_cap
    )
  }

  # For each format string row, create formatted output
  result_rows <- vector("list", length(format_strings))

  for (i in seq_along(format_strings)) {
    row_label_text <- names(format_strings)[i]
    fmt <- format_strings[[i]]
    var_names <- fmt$vars

    has_auto <- any(vapply(fmt$parsed$groups, function(g) {
      g$int$auto || (g$has_decimal && g$dec$auto)
    }, logical(1)))

    if (has_auto && !is.null(precision_table)) {
      formatted <- format_with_precision(
        fmt, var_names, stats, group_vars, precision_table, settings$precision_by
      )
    } else {
      fmt_args <- lapply(var_names, function(v) stats[[v]])
      formatted <- do.call(apply_formats, c(list(fmt), fmt_args))
    }

    row_dt <- stats[, .SD, .SDcols = group_vars]
    row_dt[, row_label := row_label_text]
    row_dt[, formatted := formatted]
    row_dt[, stat_order := i]

    result_rows[[i]] <- row_dt
  }

  long <- data.table::rbindlist(result_rows)

  # Build row label columns
  label_cols <- character(0)
  col_idx <- 1L

  for (lbl in by_labels) {
    col_name <- paste0("rowlabel", col_idx)
    long[, (col_name) := lbl]
    label_cols <- c(label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  for (bv in by_data_vars) {
    col_name <- paste0("rowlabel", col_idx)
    long[, (col_name) := as.character(get(bv))]
    label_cols <- c(label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  # Add variable label for multi-target mode
  if (!is.null(var_label)) {
    col_name <- paste0("rowlabel", col_idx)
    long[, (col_name) := var_label]
    label_cols <- c(label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  # Add stat label as row label
  stat_label_col <- paste0("rowlabel", col_idx)
  long[, (stat_label_col) := row_label]
  label_cols <- c(label_cols, stat_label_col)

  # dcast to wide
  all_label_cols <- label_cols
  col_labels <- NULL

  if (length(cols) == 0) {
    wide <- long[, c(all_label_cols, "formatted", "stat_order"), with = FALSE]
    data.table::setnames(wide, "formatted", "res1")
  } else {
    lhs <- paste(c(all_label_cols, "stat_order"), collapse = " + ")

    if (length(cols) == 1) {
      rhs <- cols[1]
    } else {
      long[, .col_combo := do.call(paste, c(.SD, sep = " | ")), .SDcols = cols]
      rhs <- ".col_combo"
    }

    formula_str <- paste(lhs, "~", rhs)
    wide <- data.table::dcast(
      long,
      as.formula(formula_str),
      value.var = "formatted",
      fill = ""
    )

    val_cols <- setdiff(names(wide), c(all_label_cols, "stat_order"))
    col_labels <- build_col_labels(val_cols, col_n)
    new_names <- paste0("res", seq_along(val_cols))
    data.table::setnames(wide, val_cols, new_names)

    if (".col_combo" %in% names(long)) {
      long[, .col_combo := NULL]
    }
  }

  # Add ordering columns
  wide[, ordindx := layer_index]
  if (!is.null(var_index)) {
    # Multi-target: encode variable index into ord1
    wide[, ord1 := var_index * 100L + stat_order]
  } else {
    wide[, ord1 := stat_order]
  }
  wide[, stat_order := NULL]

  # Attach label attributes to result columns
  if (!is.null(col_labels)) {
    res_cols <- paste0("res", seq_along(col_labels))
    for (i in seq_along(res_cols)) {
      data.table::setattr(wide[[res_cols[i]]], "label", col_labels[i])
    }
  }

  # Attach numeric data snapshot
  data.table::setattr(wide, "numeric_data", as.data.frame(numeric_snapshot))

  wide
}

#' Build a multi-target desc layer
#' @keywords internal
build_desc_multi <- function(dt, target_vars, cols, by_data_vars, by_labels,
                              settings, layer_index, col_n, pop_dt = NULL) {
  var_results <- vector("list", length(target_vars))

  for (vi in seq_along(target_vars)) {
    var_results[[vi]] <- build_desc_single(
      dt, target_vars[vi], cols, by_data_vars, by_labels,
      settings, layer_index, col_n,
      var_label = target_vars[vi], var_index = vi,
      pop_dt = pop_dt
    )
  }

  # Collect numeric data from sub-results
  multi_numeric <- lapply(seq_along(var_results), function(vi) {
    nd <- attr(var_results[[vi]], "numeric_data")
    if (!is.null(nd)) {
      nd$.target_var <- target_vars[vi]
    }
    nd
  })
  multi_numeric <- do.call(rbind, multi_numeric[!vapply(multi_numeric, is.null, logical(1))])

  # Row-bind all variable blocks, preserving label attributes
  result <- harmonize_and_bind(var_results)

  # Re-sort by ordering columns
  all_ord <- grep("^ord", names(result), value = TRUE)
  other_ord <- sort(setdiff(all_ord, "ordindx"))
  data.table::setorderv(result, c("ordindx", other_ord))

  # Attach collected numeric data
  if (!is.null(multi_numeric)) {
    data.table::setattr(result, "numeric_data", multi_numeric)
  }

  result
}

#' Transpose stats-as-columns
#'
#' Transposes the standard wide output so that treatment groups become rows
#' and stat names become columns.
#'
#' @param wide data.table from standard desc processing
#' @return Transposed data.table
#' @keywords internal
transpose_stats_to_columns <- function(wide) {
  res_cols <- grep("^res\\d+$", names(wide), value = TRUE)
  if (length(res_cols) == 0) return(wide)

  # Get treatment group labels
  trt_labels <- vapply(res_cols, function(col) {
    lbl <- attr(wide[[col]], "label")
    if (is.null(lbl)) col else lbl
  }, character(1))

  # Identify rowlabel columns and find the stat label column (the last one)
  label_cols <- grep("^rowlabel\\d+$", names(wide), value = TRUE)
  if (length(label_cols) == 0) return(wide)

  stat_col <- label_cols[length(label_cols)]
  stat_names <- wide[[stat_col]]
  non_stat_labels <- setdiff(label_cols, stat_col)

  # Build transposed result: one row per treatment group
  result_rows <- vector("list", length(res_cols))
  for (i in seq_along(res_cols)) {
    row_dt <- data.table::data.table(rowlabel1 = trt_labels[i])
    for (j in seq_along(stat_names)) {
      data.table::set(row_dt, j = stat_names[j], value = wide[[res_cols[i]]][j])
    }
    row_dt[, ordindx := wide$ordindx[1]]
    row_dt[, ord1 := i]
    result_rows[[i]] <- row_dt
  }

  transposed <- data.table::rbindlist(result_rows, use.names = TRUE, fill = TRUE)

  # If there were non-stat label columns with constant values, add them
  if (length(non_stat_labels) > 0) {
    for (lbl_col in non_stat_labels) {
      vals <- unique(wide[[lbl_col]])
      if (length(vals) == 1) {
        # Shift existing rowlabel columns to make room
        # Actually, rename rowlabel1 and prepend the constant label
        old_rl1 <- transposed$rowlabel1
        transposed[, rowlabel1 := vals]
        transposed[, rowlabel2 := old_rl1]
      }
    }
  }

  transposed
}

#' Format values with auto-precision
#'
#' Handles the split-apply-combine when precision_by creates multiple
#' precision groups. For a single precision group (no precision_by),
#' resolves precision once and formats all rows.
#'
#' @param fmt An f_str object
#' @param var_names Character vector of variable names to format
#' @param stats data.table with computed statistics
#' @param group_vars Character vector of grouping column names
#' @param precision_table data.table from collect_precision()
#' @param precision_by Character vector of precision grouping variables (or NULL)
#' @return Character vector of formatted values
#' @keywords internal
format_with_precision <- function(fmt, var_names, stats, group_vars,
                                   precision_table, precision_by) {
  prec_by <- precision_by %||% character(0)

  if (length(prec_by) == 0 || nrow(precision_table) == 1) {
    # Single precision group — resolve once, format all
    resolved <- lapply(fmt$parsed$groups, function(g) {
      resolve_precision(g, precision_table$max_int[1], precision_table$max_dec[1])
    })
    fmt_args <- lapply(var_names, function(v) stats[[v]])
    return(do.call(apply_formats, c(list(fmt), fmt_args, list(precision = resolved))))
  }

  # Multiple precision groups — split by precision_by, format each group
  formatted <- character(nrow(stats))
  for (pg in seq_len(nrow(precision_table))) {
    prec_row <- precision_table[pg]

    mask <- rep(TRUE, nrow(stats))
    for (pb in prec_by) {
      mask <- mask & as.character(stats[[pb]]) == as.character(prec_row[[pb]])
    }
    if (!any(mask)) next

    resolved <- lapply(fmt$parsed$groups, function(g) {
      resolve_precision(g, prec_row$max_int, prec_row$max_dec)
    })
    sub_args <- lapply(var_names, function(v) stats[[v]][mask])
    formatted[mask] <- do.call(apply_formats, c(list(fmt), sub_args, list(precision = resolved)))
  }

  formatted
}

#' Get descriptive format strings, falling back to defaults
#' @keywords internal
get_desc_formats <- function(settings) {
  if (!is.null(settings$format_strings)) {
    return(settings$format_strings)
  }

  # Default format strings
  list(
    "n"         = f_str("xxx", "n"),
    "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
    "Median"    = f_str("xx.x", "median"),
    "Q1, Q3"    = f_str("xx.x, xx.x", "q1", "q3"),
    "Min, Max"  = f_str("xx, xx", "min", "max"),
    "Missing"   = f_str("xxx", "missing")
  )
}
