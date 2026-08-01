#' Process a descriptive statistics layer
#'
#' @param dt data.table with the (filtered) input data
#' @param layer A tplyr_desc_layer object
#' @param cols Character vector of column variable names from the spec
#' @param layer_index Integer index of this layer
#'
#' @return A data.table with rowlabel*, res*, and ord* columns
#' @keywords internal
build_desc_layer <- function(dt, layer, cols, layer_index, col_n = NULL, pop_dt = NULL,
                             col_levels = NULL) {
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
      settings, layer_index, col_n, pop_dt = pop_dt, col_levels = col_levels
    )
  } else {
    # Multiple target variables
    result <- build_desc_multi(
      dt, target_var, cols, by_data_vars, by_labels,
      settings, layer_index, col_n, pop_dt = pop_dt, col_levels = col_levels
    )
  }

  # --- Stats as columns transposition ---
  if (isTRUE(settings$stats_as_columns)) {
    result <- transpose_stats_to_columns(result)
  }

  # --- Omnibus association-test p-value column (#51) ---
  # Desc layers support the omnibus contract only: `fn` runs once per by-group
  # over that group's raw source-data subset (all `cols` levels) and its scalar
  # (or verbatim character) result lands on the group's first output row; NA
  # renders a blank. This is the same contract used by count/shift omnibus mode
  # and is what a continuous-variable comparison (ANOVA / Kruskal / t-test)
  # needs. Pairwise/per-level mode is count-layer only (rejected in validation).
  if (!is.null(settings$assoc_test) && !isTRUE(settings$assoc_test$pairwise)) {
    assoc <- compute_assoc_test(dt, by_data_vars, settings$assoc_test)
    by_rl_cols <- str_c("rowlabel", length(by_labels) + seq_along(by_data_vars))
    merge_assoc_column(result, assoc, by_rl_cols, by_data_vars,
                       settings$assoc_test)
  }

  result
}

#' Build a single-target desc layer
#' @keywords internal
build_desc_single <- function(dt, tv, cols, by_data_vars, by_labels,
                               settings, layer_index, col_n,
                               var_label = NULL, var_index = NULL,
                               pop_dt = NULL, col_levels = NULL) {
  # Group variables
  group_vars <- c(cols, by_data_vars)

  # Compute all built-in statistics
  qtype <- getOption("tplyr2.quantile_type", 7)

  stats <- dt[, {
    v <- get(tv)
    v_clean <- v[!is.na(v)]
    v_finite <- v_clean[is.finite(v_clean)]
    list(
      n         = length(v_clean),
      n_records = length(v),
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
    all_custom[names(settings$custom_summaries)] <- settings$custom_summaries
  }

  if (length(all_custom) > 0) {
    custom_stats <- dt[, {
      .var <- get(tv)
      grp <- format_group_label(.BY)
      result <- imap(all_custom, function(expr, nm) {
        tryCatch(eval(expr), error = function(e) {
          # Still NA (blank cell), but the reason is reported once per build.
          record_user_fn_error(str_c("custom summary '", nm, "'"), e, grp)
          NA_real_
        })
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
  needs_precision <- any(map_lgl(format_strings, function(fmt) {
    any(map_lgl(fmt$parsed$groups, function(g) {
      g$int$auto || (g$has_decimal && g$dec$auto)
    }))
  }))

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

    has_auto <- any(map_lgl(fmt$parsed$groups, function(g) {
      g$int$auto || (g$has_decimal && g$dec$auto)
    }))

    if (has_auto && !is.null(precision_table)) {
      formatted <- format_with_precision(
        fmt, var_names, stats, group_vars, precision_table, settings$precision_by
      )
    } else {
      fmt_args <- map(var_names, function(v) stats[[v]])
      formatted <- do.call(apply_formats, c(list(fmt), fmt_args))
    }

    row_dt <- stats[, .SD, .SDcols = group_vars]
    row_dt[, row_label := row_label_text]
    row_dt[, formatted := formatted]
    row_dt[, stat_order := i]

    result_rows[[i]] <- row_dt
  }

  long <- data.table::rbindlist(result_rows)

  # Ordering key for by-group rows, respecting the by variables' factor levels
  # (then VARN, then alphabetical). Without this, by groups fall out in the
  # dcast's alphabetical order, e.g. "Week 12" before "Week 2" (issue #20).
  if (length(by_data_vars) > 0) {
    ord_codes <- map(by_data_vars, function(bv) {
      v <- long[[bv]]
      if (is.factor(v)) as.integer(v)
      else compute_var_order(as.character(v), var_name = bv, data_dt = dt)
    })
    key_df <- setNames(as.data.frame(ord_codes, stringsAsFactors = FALSE),
                              str_c("k", seq_along(ord_codes)))
    long[, .by_ord := data.table::frankv(key_df, ties.method = "dense")]
  }

  # Build row label columns
  label_cols <- character(0)
  col_idx <- 1L

  for (lbl in by_labels) {
    col_name <- str_c("rowlabel", col_idx)
    long[, (col_name) := lbl]
    label_cols <- c(label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  for (bv in by_data_vars) {
    col_name <- str_c("rowlabel", col_idx)
    long[, (col_name) := as.character(get(bv))]
    label_cols <- c(label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  # Add variable label for multi-target mode
  if (!is.null(var_label)) {
    col_name <- str_c("rowlabel", col_idx)
    long[, (col_name) := var_label]
    label_cols <- c(label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  # Add stat label as row label
  stat_label_col <- str_c("rowlabel", col_idx)
  long[, (stat_label_col) := row_label]
  label_cols <- c(label_cols, stat_label_col)

  # dcast to wide
  all_label_cols <- label_cols
  col_labels <- NULL
  # Carry the by-group ordering key through the cast (it is constant per by
  # group, so it never splits a dcast cell)
  extra_lhs <- if (".by_ord" %in% names(long)) ".by_ord" else character(0)

  if (length(cols) == 0) {
    wide <- long[, c(all_label_cols, "formatted", "stat_order", extra_lhs), with = FALSE]
    data.table::setnames(wide, "formatted", "res1")
  } else {
    lhs <- str_c(c(all_label_cols, "stat_order", extra_lhs), collapse = " + ")
    rhs <- prepare_cast_column(long, cols, col_levels %||% get_col_levels(dt, cols))
    formula_str <- str_c(lhs, " ~ ", rhs)
    wide <- data.table::dcast(
      long,
      as.formula(formula_str),
      value.var = "formatted",
      fill = ""
    )

    val_cols <- setdiff(names(wide), c(all_label_cols, "stat_order", extra_lhs))
    col_labels <- build_col_labels(val_cols, col_n)
    new_names <- str_c("res", seq_along(val_cols))
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

  # Secondary ordering: by-group factor order within each statistic
  if (".by_ord" %in% names(wide)) {
    wide[, ord2 := .by_ord]
    wide[, .by_ord := NULL]
  }

  # Attach label attributes to result columns
  if (!is.null(col_labels)) {
    res_cols <- str_c("res", seq_along(col_labels))
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
                              settings, layer_index, col_n, pop_dt = NULL,
                              col_levels = NULL) {
  var_results <- map(seq_along(target_vars), function(vi) {
    build_desc_single(
      dt, target_vars[vi], cols, by_data_vars, by_labels,
      settings, layer_index, col_n,
      var_label = target_vars[vi], var_index = vi,
      pop_dt = pop_dt, col_levels = col_levels
    )
  })

  # Collect numeric data from sub-results
  multi_numeric <- map(seq_along(var_results), function(vi) {
    nd <- attr(var_results[[vi]], "numeric_data")
    if (!is.null(nd)) {
      nd$.target_var <- target_vars[vi]
    }
    nd
  })
  multi_numeric <- do.call(rbind, discard(multi_numeric, is.null))

  # Row-bind all variable blocks, preserving label attributes
  result <- harmonize_and_bind(var_results)

  # Re-sort by ordering columns
  sort_by_ord_columns(result)

  # Attach collected numeric data
  if (!is.null(multi_numeric)) {
    data.table::setattr(result, "numeric_data", multi_numeric)
  }

  result
}

#' Transpose stats-as-columns
#'
#' Transposes the standard wide output so that statistics become columns.
#' Without a `by` variable, treatment groups become the rows and stat names
#' become the columns. With a `by` variable, the by groups stay as rows and
#' each column is a treatment x statistic combination (issue #20) — e.g. one
#' "Arm A | Mean" and "Arm A | n" column per treatment — so the by dimension
#' is preserved instead of being collapsed.
#'
#' @param wide data.table from standard desc processing
#' @return Transposed data.table
#' @keywords internal
transpose_stats_to_columns <- function(wide) {
  res_cols <- str_subset(names(wide), "^res\\d+$")
  if (length(res_cols) == 0) return(wide)

  # Treatment (cols) group labels, one per res column
  trt_labels <- map_chr(res_cols, function(col) {
    attr(wide[[col]], "label") %||% col
  })

  # Identify rowlabel columns; the last one holds the stat names
  label_cols <- str_subset(names(wide), "^rowlabel\\d+$")
  if (length(label_cols) == 0) return(wide)

  stat_col <- label_cols[length(label_cols)]
  by_cols <- setdiff(label_cols, stat_col)

  # A real `by` dimension exists when the non-stat row labels take more than
  # one distinct value (a constant `by` label does not count).
  has_by <- length(by_cols) > 0 &&
    nrow(unique(wide[, by_cols, with = FALSE])) > 1

  if (has_by) {
    return(transpose_stats_with_by(wide, res_cols, trt_labels, by_cols, stat_col))
  }

  # --- No by variable: treatment groups become rows, stats become columns ---
  # Take the statistics in layer order (ord1 encodes the format-string order);
  # `wide` is sorted alphabetically by label, which is not the author's order.
  if ("ord1" %in% names(wide)) {
    stat_ord <- unique(wide[, c(stat_col, "ord1"), with = FALSE])
    data.table::setorderv(stat_ord, "ord1")
    stat_names <- stat_ord[[stat_col]]
    data.table::setorderv(wide, "ord1")
  } else {
    stat_names <- wide[[stat_col]]
  }
  non_stat_labels <- setdiff(label_cols, stat_col)

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
        # Prepend the constant label as rowlabel1, shifting the transposed
        # labels to rowlabel2
        old_rl1 <- transposed$rowlabel1
        transposed[, rowlabel1 := vals]
        transposed[, rowlabel2 := old_rl1]
      }
    }
  }

  transposed
}

#' Transpose desc stats to columns while keeping `by` groups as rows
#'
#' Produces one row per by-group combination and one result column per
#' treatment x statistic, ordered treatment-major then statistic. Result
#' columns carry a `"<treatment label> | <stat name>"` label attribute
#' following the same grammar as count-layer `stat_columns`.
#'
#' @keywords internal
transpose_stats_with_by <- function(wide, res_cols, trt_labels, by_cols, stat_col) {
  # Statistics in layer order (ord1 encodes the format-string order)
  if ("ord1" %in% names(wide)) {
    stat_ord <- unique(wide[, c(stat_col, "ord1"), with = FALSE])
    data.table::setorderv(stat_ord, "ord1")
    stat_names <- unique(stat_ord[[stat_col]])
  } else {
    stat_names <- unique(wide[[stat_col]])
  }

  # One row per by-group combination, ordered by the by-group factor order
  # (ord2, set upstream) so e.g. "Week 2" precedes "Week 12" (issue #20).
  if ("ord2" %in% names(wide)) {
    out <- unique(wide[, c(by_cols, "ord2"), with = FALSE])
    data.table::setorderv(out, "ord2")
    out[, ord2 := NULL]
  } else {
    out <- unique(wide[, by_cols, with = FALSE])
  }
  out[, .row_ord := seq_len(.N)]

  col_labels <- character(0)
  k <- 1L
  for (i in seq_along(res_cols)) {          # treatment-major
    for (s in stat_names) {                  # statistic-minor
      sub <- wide[get(stat_col) == s, c(by_cols, res_cols[i]), with = FALSE]
      new_col <- str_c("res", k)
      data.table::setnames(sub, res_cols[i], new_col)
      out <- merge(out, sub, by = by_cols, all.x = TRUE, sort = FALSE)
      col_labels <- c(col_labels, str_c(trt_labels[i], " | ", s))
      k <- k + 1L
    }
  }

  data.table::setorderv(out, ".row_ord")
  out[, ord1 := .row_ord]
  out[, .row_ord := NULL]
  out[, ordindx := wide$ordindx[1]]

  # Attach column label attributes
  new_res <- str_c("res", seq_len(k - 1L))
  for (j in seq_along(new_res)) {
    data.table::setattr(out[[new_res[j]]], "label", col_labels[j])
  }

  out
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
    resolved <- map(fmt$parsed$groups, function(g) {
      resolve_precision(g, precision_table$max_int[1], precision_table$max_dec[1])
    })
    fmt_args <- map(var_names, function(v) stats[[v]])
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

    resolved <- map(fmt$parsed$groups, function(g) {
      resolve_precision(g, prec_row$max_int, prec_row$max_dec)
    })
    sub_args <- map(var_names, function(v) stats[[v]][mask])
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
