#' Compute risk differences for count layer data
#'
#' For each comparison pair and each target variable level, computes the
#' difference in proportions with a confidence interval using
#' \code{stats::prop.test()}.
#'
#' @param counts_long data.table in long format with n, total columns
#' @param cols Character vector of column variable names from the spec
#' @param tv Character string naming the target variable
#' @param by_data_vars Character vector of by-variable names
#' @param risk_diff_config List with comparisons, ci, and format
#'
#' @return A data.table with one row per target_var level per comparison,
#'   containing rdiff, lower, upper, p_value columns
#' @keywords internal
compute_risk_diff <- function(counts_long, cols, tv, by_data_vars,
                               risk_diff_config) {
  comparisons <- risk_diff_config$comparisons
  ci_level <- risk_diff_config$ci %||% 0.95

  if (is.null(comparisons) || length(comparisons) == 0) {
    return(NULL)
  }

  # We need the column variable (first element of cols)
  if (length(cols) == 0) {
    stop("Risk difference requires at least one column variable (cols)")
  }
  col_var <- cols[1]

  # Group by: by_data_vars + tv (the row-defining variables)
  row_vars <- c(by_data_vars, tv)

  results <- vector("list", length(comparisons))

  for (ci_idx in seq_along(comparisons)) {
    comp <- comparisons[[ci_idx]]
    if (length(comp) != 2) {
      stop("Each risk_diff comparison must be a 2-element character vector (treatment, reference)")
    }
    trt_level <- comp[1]
    ref_level <- comp[2]

    # Get unique row-variable combinations
    if (length(row_vars) > 0) {
      row_combos <- unique(counts_long[, row_vars, with = FALSE])
    } else {
      row_combos <- data.table::data.table(.dummy = 1L)
    }

    rd_rows <- vector("list", nrow(row_combos))

    for (r in seq_len(nrow(row_combos))) {
      # Build filter for this row
      if (length(row_vars) > 0) {
        mask_trt <- counts_long[[col_var]] == trt_level
        mask_ref <- counts_long[[col_var]] == ref_level
        for (rv in row_vars) {
          mask_trt <- mask_trt & counts_long[[rv]] == row_combos[[rv]][r]
          mask_ref <- mask_ref & counts_long[[rv]] == row_combos[[rv]][r]
        }
      } else {
        mask_trt <- counts_long[[col_var]] == trt_level
        mask_ref <- counts_long[[col_var]] == ref_level
      }

      trt_data <- counts_long[mask_trt]
      ref_data <- counts_long[mask_ref]

      if (nrow(trt_data) == 0 || nrow(ref_data) == 0) {
        rd_rows[[r]] <- data.table::data.table(
          .comp_idx = ci_idx,
          rdiff = NA_real_,
          lower = NA_real_,
          upper = NA_real_,
          p_value = NA_real_
        )
        if (length(row_vars) > 0) {
          for (rv in row_vars) {
            rd_rows[[r]][[rv]] <- row_combos[[rv]][r]
          }
        }
        next
      }

      n_trt <- trt_data$n[1]
      total_trt <- trt_data$total[1]
      n_ref <- ref_data$n[1]
      total_ref <- ref_data$total[1]

      # Compute risk difference via prop.test
      rd_result <- tryCatch({
        pt <- suppressWarnings(stats::prop.test(
          x = c(n_trt, n_ref),
          n = c(total_trt, total_ref),
          conf.level = ci_level,
          correct = FALSE
        ))
        p1 <- n_trt / total_trt
        p2 <- n_ref / total_ref
        list(
          rdiff = (p1 - p2) * 100,  # As percentage
          lower = pt$conf.int[1] * 100,
          upper = pt$conf.int[2] * 100,
          p_value = pt$p.value
        )
      }, error = function(e) {
        list(
          rdiff = NA_real_,
          lower = NA_real_,
          upper = NA_real_,
          p_value = NA_real_
        )
      })

      row_dt <- data.table::data.table(
        .comp_idx = ci_idx,
        rdiff = rd_result$rdiff,
        lower = rd_result$lower,
        upper = rd_result$upper,
        p_value = rd_result$p_value
      )
      if (length(row_vars) > 0) {
        for (rv in row_vars) {
          row_dt[[rv]] <- row_combos[[rv]][r]
        }
      }

      rd_rows[[r]] <- row_dt
    }

    results[[ci_idx]] <- data.table::rbindlist(rd_rows, fill = TRUE)
  }

  data.table::rbindlist(results, fill = TRUE)
}

#' Format risk difference values
#'
#' Applies an f_str format to the computed risk difference data.
#'
#' @param rd_data data.table with rdiff, lower, upper, p_value columns
#' @param fmt An f_str object for formatting
#'
#' @return Character vector of formatted risk difference strings
#' @keywords internal
format_risk_diff <- function(rd_data, fmt) {
  fmt_args <- map(fmt$vars, function(v) rd_data[[v]])
  do.call(apply_formats, c(list(fmt), fmt_args))
}

#' Merge risk difference columns onto wide result
#'
#' Appends formatted risk difference columns to the wide-format output from
#' \code{cast_to_wide()}.
#'
#' @param wide data.table in wide format (after cast_to_wide)
#' @param rd_data data.table with computed risk differences
#' @param risk_diff_config List with comparisons, ci, format
#' @param row_label_cols Character vector of row label column names
#' @param tv Character string naming the target variable
#' @param by_data_vars Character vector of by-variable names
#'
#' @return Modified wide data.table with rdiff columns appended
#' @keywords internal
merge_risk_diff_columns <- function(wide, rd_data, risk_diff_config,
                                     row_label_cols, tv, by_data_vars) {
  if (is.null(rd_data) || nrow(rd_data) == 0) return(wide)

  comparisons <- risk_diff_config$comparisons
  fmt <- risk_diff_config$format

  if (is.null(fmt)) {
    fmt <- f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
  }

  # Determine which rowlabel column holds the target variable
  # (It's the last rowlabel column if no by_data_vars, or after them)
  tv_label_idx <- length(by_data_vars) + 1L
  # Account for by_labels (string labels in `by` that aren't data vars)
  # The TV is in the last rowlabel position
  all_label_cols <- sort(str_subset(names(wide), "^rowlabel\\d+$"))
  if (length(all_label_cols) == 0) return(wide)
  tv_label_col <- all_label_cols[length(all_label_cols)]

  row_vars <- c(by_data_vars, tv)

  for (ci_idx in seq_along(comparisons)) {
    comp <- comparisons[[ci_idx]]
    comp_label <- str_c(comp[1], " vs ", comp[2])

    rd_subset <- rd_data[.comp_idx == ci_idx]

    if (nrow(rd_subset) > 0) {
      # Format the risk difference values
      rd_subset[, .formatted := format_risk_diff(.SD, fmt)]

      # Build column name
      rd_col <- str_c("rdiff", ci_idx)

      # Match rows between wide and rd_subset using the tv column
      wide[, (rd_col) := ""]
      for (r in seq_len(nrow(rd_subset))) {
        tv_val <- as.character(rd_subset[[tv]][r])
        mask <- wide[[tv_label_col]] == tv_val

        # Also match by_data_vars if present
        if (length(by_data_vars) > 0) {
          for (bv_idx in seq_along(by_data_vars)) {
            bv <- by_data_vars[bv_idx]
            bv_col <- all_label_cols[bv_idx]
            if (bv %in% names(rd_subset) && bv_col %in% names(wide)) {
              mask <- mask & wide[[bv_col]] == as.character(rd_subset[[bv]][r])
            }
          }
        }

        wide[mask, (rd_col) := rd_subset$.formatted[r]]
      }

      # Add label attribute
      data.table::setattr(wide[[rd_col]], "label", comp_label)
    }
  }

  wide
}
