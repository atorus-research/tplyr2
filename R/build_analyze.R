#' Process a custom analysis layer
#'
#' Calls the user-provided \code{analyze_fn} for each group combination
#' and formats results using either \code{format_strings} or a pre-formatted
#' column.
#'
#' @param dt data.table with the (filtered) input data
#' @param layer A tplyr_analyze_layer object
#' @param cols Character vector of column variable names from the spec
#' @param layer_index Integer index of this layer
#' @param col_n data.table with column counts (or NULL)
#' @param pop_dt data.table with population data (or NULL)
#'
#' @return A data.table with rowlabel*, res*, and ord* columns
#' @keywords internal
build_analyze_layer <- function(dt, layer, cols, layer_index,
                                 col_n = NULL, pop_dt = NULL) {
  target_var <- layer$target_var
  by <- layer$by
  settings <- layer$settings
  analyze_fn <- layer$analyze_fn

  # Apply layer-level where
  if (!is.null(layer$where) && !identical(layer$where, TRUE)) {
    dt <- dt[eval(layer$where)]
  }

  # Separate by into data variables and labels
  by_info <- classify_by(by, names(dt))
  by_data_vars <- by_info$data_vars
  by_labels <- by_info$labels

  # Group variables for splitting data
  group_vars <- c(cols, by_data_vars)

  # Call analyze_fn per group
  fn_combined <- call_analyze_fn_by_group(dt, analyze_fn, target_var, group_vars)

  # Capture numeric snapshot before formatting
  numeric_snapshot <- data.table::copy(fn_combined)

  has_format_strings <- !is.null(settings$format_strings) &&
    length(settings$format_strings) > 0

  if (has_format_strings) {
    combined <- format_analyze_results(fn_combined, settings$format_strings,
                                        group_vars)
  } else {
    # Expect row_label + formatted columns from analyze_fn
    if (!"formatted" %in% names(fn_combined)) {
      stop("analyze_fn must return a 'formatted' column when no format_strings ",
           "are provided", call. = FALSE)
    }
    if (!"row_label" %in% names(fn_combined)) {
      stop("analyze_fn must return a 'row_label' column when no format_strings ",
           "are provided", call. = FALSE)
    }
    combined <- fn_combined
  }

  # Build row label columns
  row_label_cols <- build_analyze_row_labels(combined, by_labels, by_data_vars)

  # Cast to wide
  wide <- cast_to_wide(combined, row_label_cols, cols, layer_index,
                        col_n = col_n)

  # Attach numeric data snapshot
  data.table::setattr(wide, "numeric_data", as.data.frame(numeric_snapshot))

  wide
}

#' Call analyze_fn for each group combination
#' @keywords internal
call_analyze_fn_by_group <- function(dt, analyze_fn, target_var, group_vars) {
  if (length(group_vars) > 0) {
    groups <- unique(dt[, group_vars, with = FALSE])
    fn_results <- vector("list", nrow(groups))

    for (g in seq_len(nrow(groups))) {
      mask <- rep(TRUE, nrow(dt))
      for (gv in group_vars) {
        mask <- mask & dt[[gv]] == groups[[gv]][g]
      }
      subset_dt <- dt[mask]

      fn_result <- analyze_fn(subset_dt, target_var)
      if (!is.data.frame(fn_result)) {
        stop("analyze_fn must return a data.frame, got ",
             class(fn_result)[1], call. = FALSE)
      }

      fn_dt <- data.table::as.data.table(fn_result)
      for (gv in group_vars) {
        fn_dt[[gv]] <- groups[[gv]][g]
      }

      fn_results[[g]] <- fn_dt
    }

    data.table::rbindlist(fn_results, use.names = TRUE, fill = TRUE)
  } else {
    fn_result <- analyze_fn(dt, target_var)
    if (!is.data.frame(fn_result)) {
      stop("analyze_fn must return a data.frame, got ",
           class(fn_result)[1], call. = FALSE)
    }
    data.table::as.data.table(fn_result)
  }
}

#' Format analyze results using format_strings
#'
#' For each group combination, takes the first row of numeric values from the
#' analyze_fn output and creates one formatted row per format_string entry.
#'
#' @param fn_combined data.table of raw analyze_fn results
#' @param format_strings Named list of f_str objects
#' @param group_vars Character vector of grouping column names
#' @return data.table with row_label and formatted columns
#' @keywords internal
format_analyze_results <- function(fn_combined, format_strings, group_vars) {
  fmt_names <- names(format_strings)

  if (length(group_vars) > 0) {
    groups_dt <- unique(fn_combined[, group_vars, with = FALSE])
  } else {
    groups_dt <- data.table::data.table(.dummy = 1L)
  }

  output_rows <- vector("list", nrow(groups_dt) * length(fmt_names))
  idx <- 1L

  for (g in seq_len(nrow(groups_dt))) {
    if (length(group_vars) > 0) {
      mask <- rep(TRUE, nrow(fn_combined))
      for (gv in group_vars) {
        mask <- mask & fn_combined[[gv]] == groups_dt[[gv]][g]
      }
      group_data <- fn_combined[mask]
    } else {
      group_data <- fn_combined
    }

    # Use first row for stat values (format_strings mode expects one row per group)
    stat_row <- if (nrow(group_data) > 0) group_data[1] else group_data[0]

    for (fi in seq_along(fmt_names)) {
      fname <- fmt_names[fi]
      fmt <- format_strings[[fname]]

      if (nrow(stat_row) > 0) {
        fmt_args <- lapply(fmt$vars, function(v) {
          if (v %in% names(stat_row)) stat_row[[v]] else NA_real_
        })
        formatted_val <- do.call(apply_formats, c(list(fmt), fmt_args))
      } else {
        formatted_val <- ""
      }

      row_dt <- data.table::data.table(
        row_label = fname,
        formatted = formatted_val
      )

      if (length(group_vars) > 0) {
        for (gv in group_vars) {
          row_dt[[gv]] <- groups_dt[[gv]][g]
        }
      }

      output_rows[[idx]] <- row_dt
      idx <- idx + 1L
    }
  }

  data.table::rbindlist(output_rows, use.names = TRUE, fill = TRUE)
}

#' Build row label columns for analyze layer
#' @keywords internal
build_analyze_row_labels <- function(combined, by_labels, by_data_vars) {
  row_label_cols <- character(0)
  col_idx <- 1L

  for (lbl in by_labels) {
    col_name <- paste0("rowlabel", col_idx)
    combined[, (col_name) := lbl]
    row_label_cols <- c(row_label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  for (bv in by_data_vars) {
    col_name <- paste0("rowlabel", col_idx)
    combined[, (col_name) := as.character(get(bv))]
    row_label_cols <- c(row_label_cols, col_name)
    col_idx <- col_idx + 1L
  }

  # row_label as final rowlabel column
  col_name <- paste0("rowlabel", col_idx)
  combined[, (col_name) := as.character(row_label)]
  row_label_cols <- c(row_label_cols, col_name)

  row_label_cols
}
