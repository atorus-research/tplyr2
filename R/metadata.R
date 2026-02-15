#' Metadata object for a tplyr output cell
#'
#' Contains filter expressions that, when evaluated against the original data,
#' reproduce the subset of rows that contributed to a specific cell in the
#' output table.
#'
#' @param names Character vector of variable names relevant to this cell
#' @param filters List of R language objects (call expressions) representing
#'   filter conditions
#' @param layer_index Integer layer index (1-based)
#' @param anti_join NULL or a \code{tplyr_meta_anti_join} object for missing
#'   subjects rows
#'
#' @return A tplyr_meta object
#' @export
tplyr_meta <- function(names = character(0), filters = list(),
                       layer_index = integer(0), anti_join = NULL) {
  structure(
    list(
      names = names,
      filters = filters,
      layer_index = layer_index,
      anti_join = anti_join
    ),
    class = "tplyr_meta"
  )
}

#' Anti-join metadata for missing subjects
#'
#' @param join_meta A \code{tplyr_meta} object with filters for the population data
#' @param on Character vector of join key variable names (e.g., \code{"USUBJID"})
#'
#' @return A tplyr_meta_anti_join object
#' @keywords internal
tplyr_meta_anti_join <- function(join_meta, on) {
  structure(
    list(
      join_meta = join_meta,
      on = on
    ),
    class = "tplyr_meta_anti_join"
  )
}

#' @export
print.tplyr_meta <- function(x, ...) {
  cat(str_c("tplyr_meta [layer ", x$layer_index, "]\n"))
  if (length(x$names) > 0) {
    cat(str_c("  Names: ", str_c(x$names, collapse = ", "), "\n"))
  }
  if (length(x$filters) > 0) {
    cat("  Filters:\n")
    filter_strs <- map_chr(x$filters, deparse1)
    walk(filter_strs, function(f) cat(str_c("    ", f, "\n")))
  }
  if (!is.null(x$anti_join)) {
    cat("  Anti-join:\n")
    cat(str_c("    On: ", str_c(x$anti_join$on, collapse = ", "), "\n"))
    if (length(x$anti_join$join_meta$filters) > 0) {
      cat("    Pop filters:\n")
      aj_strs <- map_chr(x$anti_join$join_meta$filters, deparse1)
      walk(aj_strs, function(f) cat(str_c("      ", f, "\n")))
    }
  }
  invisible(x)
}

#' Generate unique row IDs for output rows
#'
#' Creates a character ID for each row by combining the layer index and
#' row label values. These IDs can be used with \code{tplyr_meta_result()}
#' and \code{tplyr_meta_subset()} to look up cell metadata.
#'
#' @param result A data.frame produced by \code{tplyr_build()}
#'
#' @return Character vector of row IDs (same length as \code{nrow(result)})
#' @export
generate_row_ids <- function(result) {
  rowlabel_cols <- sort(str_subset(names(result), "^rowlabel\\d+$"))
  layer_part <- as.character(result$ord_layer_index)

  if (length(rowlabel_cols) == 0) {
    return(str_c("r", layer_part, "_", seq_len(nrow(result))))
  }

  parts <- map(rowlabel_cols, function(col) as.character(result[[col]]))
  do.call(paste, c(list(layer_part), parts, list(sep = "_")))
}

#' Get metadata for a specific output cell
#'
#' Returns a \code{tplyr_meta} object containing the filter expressions
#' that describe the source data for the specified cell.
#'
#' @param result A data.frame from \code{tplyr_build()} built with
#'   \code{metadata = TRUE}
#' @param row_id Character row ID (from \code{result$row_id} or
#'   \code{generate_row_ids()})
#' @param column Character column name (e.g., \code{"res1"})
#'
#' @return A \code{tplyr_meta} object, or NULL if no metadata for that cell
#' @export
tplyr_meta_result <- function(result, row_id, column) {
  meta <- attr(result, "tplyr_meta")
  if (is.null(meta)) {
    stop("No metadata available. Rebuild with metadata = TRUE", call. = FALSE)
  }
  key <- paste(row_id, column, sep = "||")
  meta[[key]]
}

#' Get source data rows for a specific output cell
#'
#' Evaluates the stored filter expressions against the original data to return
#' the rows that contributed to the specified output cell.
#'
#' @param result A data.frame from \code{tplyr_build()} built with
#'   \code{metadata = TRUE}
#' @param row_id Character row ID
#' @param column Character column name (e.g., \code{"res1"})
#' @param data The original data.frame that was passed to \code{tplyr_build()}
#' @param pop_data Optional population data.frame, required when the cell
#'   represents a missing subjects row (anti-join)
#'
#' @return A data.frame subset of the original data, or NULL if no metadata
#' @export
tplyr_meta_subset <- function(result, row_id, column, data, pop_data = NULL) {
  meta_obj <- tplyr_meta_result(result, row_id, column)
  if (is.null(meta_obj)) return(NULL)

  if (length(meta_obj$filters) == 0 && is.null(meta_obj$anti_join)) {
    return(data[0, , drop = FALSE])
  }

  # Apply filters to data
  if (length(meta_obj$filters) > 0) {
    combined <- Reduce(function(a, b) call("&", a, b), meta_obj$filters)
    dt <- data.table::as.data.table(data)
    out <- as.data.frame(dt[eval(combined)])
  } else {
    out <- data
  }

  # Handle anti-join for missing subjects
  if (!is.null(meta_obj$anti_join)) {
    aj <- meta_obj$anti_join
    if (is.null(pop_data)) {
      warning("pop_data is required for anti-join metadata but was not provided",
              call. = FALSE)
      return(out)
    }
    pop_dt <- data.table::as.data.table(pop_data)
    if (length(aj$join_meta$filters) > 0) {
      pop_filter <- Reduce(function(a, b) call("&", a, b), aj$join_meta$filters)
      pop_dt <- pop_dt[eval(pop_filter)]
    }
    # data.table anti-join: rows in pop that are NOT in target
    target_dt <- data.table::as.data.table(out)
    out <- as.data.frame(pop_dt[!target_dt, on = aj$on])
  }

  out
}

# =============================================================================
# Filter expression builders (internal)
# =============================================================================

#' Build an equality filter expression
#' @keywords internal
make_eq_filter <- function(var_name, value) {
  call("==", as.name(var_name), value)
}

#' Build a \%in\% filter expression
#' @keywords internal
make_in_filter <- function(var_name, values) {
  call("%in%", as.name(var_name), values)
}

#' Build a negated \%in\% filter expression
#' @keywords internal
make_not_in_filter <- function(var_name, values) {
  call("!", call("%in%", as.name(var_name), values))
}

#' Build a !is.na() filter expression
#' @keywords internal
make_not_na_filter <- function(var_name) {
  call("!", call("is.na", as.name(var_name)))
}

#' Build a filter for missing values (is.na OR %in% missing_values)
#' @keywords internal
make_missing_filter <- function(var_name, missing_values = character(0)) {
  sym <- as.name(var_name)
  na_expr <- call("is.na", sym)

  if (length(missing_values) > 0) {
    in_expr <- call("%in%", sym, missing_values)
    call("|", na_expr, in_expr)
  } else {
    na_expr
  }
}

#' Translate a group value to filter expressions
#'
#' When a column value corresponds to a total group or custom group label,
#' translates back to the appropriate filter. Total groups produce no filter
#' (all values pass). Custom groups produce a \code{\%in\%} filter with
#' component values.
#'
#' @param value The column value from the output (e.g., "Total", "Active")
#' @param col_var The column variable name
#' @param total_groups List of tplyr_total_group objects
#' @param custom_groups List of tplyr_custom_group objects
#'
#' @return A list with \code{filters} (list of call expressions) and
#'   \code{is_total} (logical)
#' @keywords internal
translate_group_value <- function(value, col_var, total_groups, custom_groups) {
  # Check total groups
  if (!is.null(total_groups)) {
    for (tg in total_groups) {
      if (tg$col_var == col_var && tg$label == value) {
        return(list(filters = list(), is_total = TRUE))
      }
    }
  }

  # Check custom groups
  if (!is.null(custom_groups)) {
    for (cg in custom_groups) {
      if (cg$col_var == col_var) {
        for (group_name in names(cg$groups)) {
          if (group_name == value) {
            return(list(
              filters = list(make_in_filter(col_var, cg$groups[[group_name]])),
              is_total = FALSE
            ))
          }
        }
      }
    }
  }

  # Normal value
  list(
    filters = list(make_eq_filter(col_var, value)),
    is_total = FALSE
  )
}

# =============================================================================
# Row type classification and variable mapping (internal)
# =============================================================================

#' Classify an output row as normal, total, missing, or missing_subjects
#'
#' @param output The output data.frame
#' @param row_idx Row index in the output
#' @param layer A tplyr_layer object
#' @param var_to_rl Named list mapping data variables to rowlabel columns
#'
#' @return Character string: "normal", "total", "missing", or "missing_subjects"
#' @keywords internal
classify_row_type <- function(output, row_idx, layer, var_to_rl) {
  settings <- layer$settings

  if (!inherits(layer, "tplyr_count_layer") &&
      !inherits(layer, "tplyr_shift_layer")) {
    return("normal")
  }

  # Find the target_var rowlabel column
  if (inherits(layer, "tplyr_count_layer")) {
    tv <- layer$target_var[1]
  } else {
    tv <- layer$target_var["row"]
  }

  tv_rl_col <- var_to_rl[[tv]]
  if (is.null(tv_rl_col) || !tv_rl_col %in% names(output)) return("normal")

  rl_val <- str_trim(as.character(output[[tv_rl_col]][row_idx]))

  # Check total row
  total_label <- settings$total_row_label %||% "Total"
  if (isTRUE(settings$total_row) && rl_val == total_label) {
    return("total")
  }

  # Check missing count row (takes priority over missing subjects)
  if (!is.null(settings$missing_count)) {
    missing_label <- settings$missing_count$label %||% "Missing"
    if (rl_val == missing_label) {
      return("missing")
    }
  }

  # Check missing subjects row
  if (isTRUE(settings$missing_subjects)) {
    ms_label <- settings$missing_subjects_label %||% "Missing"
    if (rl_val == ms_label) {
      return("missing_subjects")
    }
  }

  "normal"
}

#' Map data variable names to their rowlabel columns
#'
#' @param layer A tplyr_layer object
#' @param by_data_vars Character vector of by-variable data column names
#' @param by_labels Character vector of by-variable label strings
#' @param col_names Character vector of original data column names
#'
#' @return Named list where names are data variables and values are rowlabel
#'   column names (e.g., \code{list(SEX = "rowlabel1")})
#' @keywords internal
build_var_to_rowlabel_map <- function(layer, by_data_vars, by_labels, col_names) {
  var_to_rl <- list()
  rl_idx <- length(by_labels) + 1L

  for (bv in by_data_vars) {
    var_to_rl[[bv]] <- str_c("rowlabel", rl_idx)
    rl_idx <- rl_idx + 1L
  }

  if (inherits(layer, "tplyr_count_layer")) {
    for (tv in layer$target_var) {
      var_to_rl[[tv]] <- str_c("rowlabel", rl_idx)
      rl_idx <- rl_idx + 1L
    }
  } else if (inherits(layer, "tplyr_shift_layer")) {
    var_to_rl[[layer$target_var["row"]]] <- str_c("rowlabel", rl_idx)
  }
  # For desc/analyze: last rowlabel is stat name, not a data variable

  var_to_rl
}

# =============================================================================
# Core filter expression builder (internal)
# =============================================================================

#' Build filter expressions for a single output cell
#'
#' Constructs the complete set of filter expressions for one cell in the output
#' table by inspecting the row's label values, the column label, and the
#' layer/spec configuration.
#'
#' @param output The output data.frame
#' @param row_idx Row index in the output
#' @param rc Result column name (e.g., "res1")
#' @param layer A tplyr_layer object
#' @param layer_idx Integer layer index
#' @param cols Character vector of spec-level column variables
#' @param col_level_map Named list: res column -> column variable level string
#' @param var_to_rl Named list mapping data variables to rowlabel columns
#' @param by_data_vars Character vector of by-variable data column names
#' @param spec The tplyr_spec object
#'
#' @return A tplyr_meta object
#' @keywords internal
build_cell_filter_exprs <- function(output, row_idx, rc, layer, layer_idx,
                                    cols, col_level_map, var_to_rl,
                                    by_data_vars, spec, pop_col_map = NULL) {
  filters <- list()
  names_vec <- character(0)
  anti_join_obj <- NULL

  settings <- layer$settings

  # --- 1. Column variable filters ---
  if (rc %in% names(col_level_map) && length(cols) > 0) {
    col_level <- col_level_map[[rc]]

    if (inherits(layer, "tplyr_shift_layer")) {
      # Shift: label is "cols_val | shift_col_val"
      parts <- str_split(col_level, fixed(" | "))[[1]]
      for (ci in seq_along(cols)) {
        resolved <- translate_group_value(
          parts[ci], cols[ci], spec$total_groups, spec$custom_groups
        )
        filters <- c(filters, resolved$filters)
        names_vec <- c(names_vec, cols[ci])
      }
      # Shift column variable
      shift_col_var <- layer$target_var["column"]
      if (length(parts) > length(cols)) {
        filters <- c(filters, list(make_eq_filter(shift_col_var, parts[length(cols) + 1])))
        names_vec <- c(names_vec, shift_col_var)
      }
    } else if (length(cols) == 1) {
      resolved <- translate_group_value(
        col_level, cols[1], spec$total_groups, spec$custom_groups
      )
      filters <- c(filters, resolved$filters)
      names_vec <- c(names_vec, cols[1])
    } else {
      parts <- str_split(col_level, fixed(" | "))[[1]]
      for (ci in seq_along(cols)) {
        resolved <- translate_group_value(
          parts[ci], cols[ci], spec$total_groups, spec$custom_groups
        )
        filters <- c(filters, resolved$filters)
        names_vec <- c(names_vec, cols[ci])
      }
    }
  }

  # Save column-level filters for potential anti-join use
  col_filters <- filters
  col_names <- names_vec

  # --- 2. Row variable filters (depends on row type) ---
  row_type <- classify_row_type(output, row_idx, layer, var_to_rl)

  # Helper: collect by_data_vars filters from the output rowlabels
  collect_by_filters <- function() {
    by_f <- list()
    by_n <- character(0)
    for (bv in by_data_vars) {
      rl_col <- var_to_rl[[bv]]
      if (!is.null(rl_col) && rl_col %in% names(output)) {
        bv_val <- as.character(output[[rl_col]][row_idx])
        if (str_length(str_trim(bv_val)) > 0) {
          by_f <- c(by_f, list(make_eq_filter(bv, str_trim(bv_val))))
          by_n <- c(by_n, bv)
        }
      }
    }
    list(filters = by_f, names = by_n)
  }

  if (row_type == "normal") {
    for (var_name in names(var_to_rl)) {
      rl_col <- var_to_rl[[var_name]]
      if (rl_col %in% names(output)) {
        rl_val <- as.character(output[[rl_col]][row_idx])
        if (str_length(str_trim(rl_val)) > 0) {
          # Strip indentation for nested counts
          rl_val <- str_trim(rl_val)
          filters <- c(filters, list(make_eq_filter(var_name, rl_val)))
          names_vec <- c(names_vec, var_name)
        }
      }
    }
  } else if (row_type == "total") {
    # Total row: no filter on target_var
    # But if count_missings is FALSE, exclude missing values
    if (inherits(layer, "tplyr_count_layer")) {
      tv <- layer$target_var[1]
    } else {
      tv <- layer$target_var["row"]
    }
    names_vec <- c(names_vec, tv)

    if (!isTRUE(settings$total_row_count_missings) &&
        !is.null(settings$missing_count)) {
      missing_values <- settings$missing_count$missing_values %||% character(0)
      if (length(missing_values) > 0) {
        filters <- c(filters, list(make_not_in_filter(tv, missing_values)))
      }
      filters <- c(filters, list(make_not_na_filter(tv)))
    }

    # Add by_data_vars filters
    by_info <- collect_by_filters()
    filters <- c(filters, by_info$filters)
    names_vec <- c(names_vec, by_info$names)
  } else if (row_type == "missing") {
    # Missing count row: is.na(target_var) or target_var %in% missing_values
    tv <- layer$target_var[1]
    missing_values <- settings$missing_count$missing_values %||% character(0)
    filters <- c(filters, list(make_missing_filter(tv, missing_values)))
    names_vec <- c(names_vec, tv)

    # Add by_data_vars filters
    by_info <- collect_by_filters()
    filters <- c(filters, by_info$filters)
    names_vec <- c(names_vec, by_info$names)
  } else if (row_type == "missing_subjects") {
    # Missing subjects: target-side needs col + by_var + where filters
    # (no target_var filter — we want all subjects in this group)
    by_info <- collect_by_filters()
    filters <- c(filters, by_info$filters)
    names_vec <- c(names_vec, by_info$names, layer$target_var[1])
  }

  # --- 3. Spec-level where filter ---
  if (!is.null(spec$where) && !identical(spec$where, TRUE)) {
    filters <- c(filters, list(spec$where))
    names_vec <- c(names_vec, all.vars(spec$where))
  }

  # --- 4. Layer-level where filter ---
  if (!is.null(layer$where) && !identical(layer$where, TRUE)) {
    filters <- c(filters, list(layer$where))
    names_vec <- c(names_vec, all.vars(layer$where))
  }

  # --- 5. Construct anti-join for missing_subjects ---
  # Built after sections 3/4 so main filters are complete. Anti-join only
  # makes sense when distinct_by is set (subject-level), not row-level.
  if (row_type == "missing_subjects" && !is.null(settings$distinct_by) &&
      length(settings$distinct_by) > 0) {
    # Pop-side filters: column filters + by_var filters + pop_data where
    # When pop columns differ from spec columns, remap filters to use original
    # pop column names so they can be evaluated against the original pop_data.
    if (!is.null(pop_col_map) && !identical(unname(pop_col_map), cols)) {
      # Build reverse mapping: spec col name -> original pop col name
      orig_pop_cols <- unname(pop_col_map)
      spec_col_names <- if (!is.null(names(pop_col_map))) names(pop_col_map) else cols
      pop_aj_filters <- map(col_filters, function(f) {
        for (ci in seq_along(spec_col_names)) {
          f <- do.call(substitute, list(f, stats::setNames(
            list(as.name(orig_pop_cols[ci])), spec_col_names[ci]
          )))
        }
        f
      })
      pop_aj_names <- orig_pop_cols
    } else {
      pop_aj_filters <- col_filters
      pop_aj_names <- col_names
    }

    pop_filters <- c(pop_aj_filters, by_info$filters)
    pop_names <- c(pop_aj_names, by_info$names)

    if (!is.null(spec$pop_data) && !is.null(spec$pop_data$where) &&
        !identical(spec$pop_data$where, TRUE)) {
      pop_filters <- c(pop_filters, list(spec$pop_data$where))
      pop_names <- c(pop_names, all.vars(spec$pop_data$where))
    }

    anti_join_obj <- tplyr_meta_anti_join(
      join_meta = tplyr_meta(
        names = unique(pop_names),
        filters = pop_filters,
        layer_index = as.integer(layer_idx)
      ),
      on = settings$distinct_by
    )
  }

  # For desc/analyze layers, add target_var to names
  if (inherits(layer, "tplyr_desc_layer") ||
      inherits(layer, "tplyr_analyze_layer")) {
    names_vec <- c(names_vec, layer$target_var)
  }

  tplyr_meta(
    names = unique(names_vec),
    filters = filters,
    layer_index = as.integer(layer_idx),
    anti_join = anti_join_obj
  )
}

# =============================================================================
# Main metadata builder (internal)
# =============================================================================

#' Build cell-level metadata for the full output table
#'
#' For each output row x result column combination, constructs filter
#' expressions that describe the source data subset for that cell. The
#' expressions can be evaluated at query time against the original data.
#'
#' @param output data.frame output from tplyr_build (with rowlabel/res/ord cols)
#' @param spec tplyr_spec object
#' @param col_names Character vector of original data column names
#'
#' @return Named list of tplyr_meta objects, keyed by "row_id||column"
#' @keywords internal
build_cell_metadata <- function(output, spec, col_names, pop_col_map = NULL) {
  cols <- spec$cols
  rowlabel_cols <- sort(str_subset(names(output), "^rowlabel\\d+$"))
  res_cols <- sort(str_subset(names(output), "^res\\d+$"))

  if (length(res_cols) == 0) return(list())

  # Parse column variable levels from res column labels
  col_level_map <- list()
  for (rc in res_cols) {
    lbl <- attr(output[[rc]], "label")
    if (!is.null(lbl)) {
      col_level_map[[rc]] <- str_replace(lbl, "\\s*\\(N=\\d+\\)$", "")
    }
  }

  # Generate row IDs
  row_ids <- generate_row_ids(output)

  cells <- list()

  for (row_idx in seq_len(nrow(output))) {
    layer_idx <- output$ord_layer_index[row_idx]
    if (is.na(layer_idx) || layer_idx < 1 ||
        layer_idx > length(spec$layers)) next

    layer <- spec$layers[[layer_idx]]

    # Build variable-to-rowlabel column mapping
    by_info <- classify_by(layer$by, col_names)
    by_data_vars <- by_info$data_vars
    by_labels <- by_info$labels

    var_to_rl <- build_var_to_rowlabel_map(layer, by_data_vars, by_labels, col_names)

    for (rc in res_cols) {
      meta <- build_cell_filter_exprs(
        output, row_idx, rc, layer, layer_idx,
        cols, col_level_map, var_to_rl, by_data_vars, spec,
        pop_col_map = pop_col_map
      )

      key <- paste(row_ids[row_idx], rc, sep = "||")
      cells[[key]] <- meta
    }
  }

  cells
}
