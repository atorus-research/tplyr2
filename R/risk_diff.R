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

    # Extract trt and ref data, merge side-by-side on row vars
    trt_dt <- counts_long[get(col_var) == trt_level, c(row_vars, "n", "total"), with = FALSE]
    ref_dt <- counts_long[get(col_var) == ref_level, c(row_vars, "n", "total"), with = FALSE]

    if (length(row_vars) > 0) {
      paired <- merge(trt_dt, ref_dt, by = row_vars, suffixes = c("_trt", "_ref"), all = TRUE)
    } else {
      paired <- data.table::data.table(
        n_trt = trt_dt$n[1], total_trt = trt_dt$total[1],
        n_ref = ref_dt$n[1], total_ref = ref_dt$total[1]
      )
    }

    # Apply prop.test per row (inherently scalar)
    rd_rows <- map(seq_len(nrow(paired)), function(r) {
      n_trt <- paired$n_trt[r]
      total_trt <- paired$total_trt[r]
      n_ref <- paired$n_ref[r]
      total_ref <- paired$total_ref[r]

      if (is.na(n_trt) || is.na(n_ref) || is.na(total_trt) || is.na(total_ref) ||
          total_trt == 0 || total_ref == 0) {
        return(data.table::data.table(
          .comp_idx = ci_idx,
          rdiff = NA_real_, lower = NA_real_,
          upper = NA_real_, p_value = NA_real_
        ))
      }

      # x > n is only reachable through an upstream denominator bug. prop.test
      # errors on it, and the surrounding tryCatch would turn that into a
      # silently blank rdiff row.
      if (n_trt > total_trt || n_ref > total_ref) {
        warning("Risk difference skipped for ", trt_level, " vs ", ref_level,
                ": count exceeds denominator (",
                str_c(str_c(c(trt_level, ref_level), ": ",
                            c(n_trt, n_ref), "/", c(total_trt, total_ref)),
                      collapse = ", "),
                "). Check denoms_by and pop_data.", call. = FALSE)
        return(data.table::data.table(
          .comp_idx = ci_idx,
          rdiff = NA_real_, lower = NA_real_,
          upper = NA_real_, p_value = NA_real_
        ))
      }

      # The plain difference needs no test, so it is computed outside the
      # tryCatch — a CI failure blanks the interval, not the difference itself.
      rdiff <- (n_trt / total_trt - n_ref / total_ref) * 100

      ci_result <- tryCatch({
        pt <- suppressWarnings(stats::prop.test(
          x = c(n_trt, n_ref),
          n = c(total_trt, total_ref),
          conf.level = ci_level,
          correct = FALSE
        ))
        list(
          lower = pt$conf.int[1] * 100,
          upper = pt$conf.int[2] * 100,
          p_value = pt$p.value
        )
      }, error = function(e) {
        record_user_fn_error("risk_diff confidence interval", e,
                             str_c(trt_level, " vs ", ref_level))
        list(lower = NA_real_, upper = NA_real_, p_value = NA_real_)
      })

      data.table::data.table(
        .comp_idx = ci_idx,
        rdiff = rdiff,
        lower = ci_result$lower,
        upper = ci_result$upper,
        p_value = ci_result$p_value
      )
    })

    rd_result_dt <- data.table::rbindlist(rd_rows)

    # Attach row_vars from paired
    if (length(row_vars) > 0) {
      rd_result_dt <- cbind(paired[, row_vars, with = FALSE], rd_result_dt)
    }

    results[[ci_idx]] <- rd_result_dt
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
#' @param by_labels Character vector of constant `by` labels. Needed to find
#'   where the by data variables sit among the rowlabel columns — they follow
#'   the label columns, not `rowlabel1`.
#'
#' @return Modified wide data.table with rdiff columns appended
#' @keywords internal
merge_risk_diff_columns <- function(wide, rd_data, risk_diff_config,
                                     row_label_cols, tv, by_data_vars,
                                     by_labels = character(0)) {
  if (is.null(rd_data) || nrow(rd_data) == 0) return(wide)

  comparisons <- risk_diff_config$comparisons
  fmt <- risk_diff_config$format

  if (is.null(fmt)) {
    fmt <- f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
  }

  join_cols <- resolve_rowlabel_join_cols(wide, by_labels, by_data_vars)
  if (is.null(join_cols)) return(wide)
  tv_label_col <- join_cols$tv_col

  for (ci_idx in seq_along(comparisons)) {
    comp <- comparisons[[ci_idx]]
    comp_label <- str_c(comp[1], " vs ", comp[2])

    rd_subset <- rd_data[.comp_idx == ci_idx]

    if (nrow(rd_subset) > 0) {
      # Format the risk difference values
      rd_subset[, formatted_rd := format_risk_diff(.SD, fmt)]

      # Build column name
      rd_col <- str_c("rdiff", ci_idx)

      # Join rd_subset onto wide using target var + by_data_vars as keys
      wide_join_cols <- tv_label_col
      rd_join_cols <- tv
      if (length(by_data_vars) > 0) {
        bv_rd_cols <- intersect(by_data_vars, names(rd_subset))
        bv_wide_cols <- join_cols$by_cols[match(bv_rd_cols, by_data_vars)]
        wide_join_cols <- c(bv_wide_cols, wide_join_cols)
        rd_join_cols <- c(bv_rd_cols, rd_join_cols)
      }

      # Ensure character types for join compatibility
      for (k in rd_join_cols) {
        set(rd_subset, j = k, value = as.character(rd_subset[[k]]))
      }

      on_clause <- setNames(rd_join_cols, wide_join_cols)
      wide[, (rd_col) := ""]
      wide[rd_subset, (rd_col) := i.formatted_rd, on = on_clause]

      # Add label attribute
      data.table::setattr(wide[[rd_col]], "label", comp_label)
    }
  }

  wide
}
