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
#' @param statistic NULL or a character string naming which statistic the
#'   cell displays (set for \code{stat_columns} layers, where the stat
#'   sub-columns of a column group share the same source-data filters)
#'
#' @return A tplyr_meta object
#'
#' @examples
#' # Usually obtained from a build rather than constructed by hand
#' m <- tplyr_meta(
#'   names = c("TRT01P", "AGEGR1"),
#'   filters = list(quote(TRT01P == "Placebo"), quote(AGEGR1 == "65-80")),
#'   layer_index = 1L
#' )
#' m
#'
#' # The filters are ordinary language objects, so they can be applied directly
#' subset(tplyr_adsl, TRT01P == "Placebo" & AGEGR1 == "65-80")[1:3, c("USUBJID", "AGEGR1")]
#'
#' @seealso [tplyr_meta_result()] to retrieve one from a build.
#' @export
tplyr_meta <- function(names = character(0), filters = list(),
                       layer_index = integer(0), anti_join = NULL,
                       statistic = NULL) {
  structure(
    list(
      names = names,
      filters = filters,
      layer_index = layer_index,
      anti_join = anti_join,
      statistic = statistic
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
  if (!is.null(x$statistic)) {
    cat(str_c("  Statistic: ", x$statistic, "\n"))
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
#'
#' @examples
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(group_count("AGEGR1"))
#' )
#' built <- tplyr_build(spec, tplyr_adsl)
#' generate_row_ids(built)
#'
#' # IDs are derived from the row labels, so generate them from an unmodified
#' # build. tplyr_build(metadata = TRUE) attaches a row_id column that survives
#' # post-processing, which is the safer route.
#' with_meta <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
#' with_meta$row_id
#'
#' @export
generate_row_ids <- function(result) {
  rowlabel_cols <- sort(str_subset(names(result), "^rowlabel\\d+$"))
  layer_part <- as.character(result$ord_layer_index)

  if (length(rowlabel_cols) == 0) {
    return(str_c("r", layer_part, "_", seq_len(nrow(result))))
  }

  parts <- map(rowlabel_cols, function(col) as.character(result[[col]]))
  ids <- do.call(paste, c(list(layer_part), parts, list(sep = "_")))

  # IDs are derived from the row label values, so they are only meaningful on an
  # unmodified build. apply_row_masks() blanks repeated labels, which collapses
  # distinct rows onto the same ID and silently breaks metadata lookups.
  if (anyDuplicated(ids)) {
    warning(str_glue(
      "generate_row_ids() produced {sum(duplicated(ids))} duplicate ID(s), so ",
      "metadata lookups will resolve to the wrong cell. This happens when row ",
      "labels have been blanked (apply_row_masks()) or collapsed ",
      "(collapse_row_labels()). Generate IDs from the unmodified ",
      "tplyr_build() output, or use the `row_id` column that ",
      "tplyr_build(metadata = TRUE) attaches -- it survives post-processing."
    ), call. = FALSE)
  }

  ids
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
#'
#' @examples
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(group_count("AGEGR1"))
#' )
#' built <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
#' built[, c("row_id", "rowlabel1", "res1")]
#'
#' # The filters behind the Placebo / 65-80 cell
#' tplyr_meta_result(built, built$row_id[1], "res1")
#'
#' @seealso [tplyr_meta_subset()] to fetch the source rows themselves.
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
#'
#' @examples
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(group_count("AGEGR1"))
#' )
#' built <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
#' built[, c("row_id", "rowlabel1", "res1")]
#'
#' # Trace the first cell back to the subjects it counted
#' src <- tplyr_meta_subset(built, built$row_id[1], "res1", tplyr_adsl)
#' nrow(src)
#' head(src[, c("USUBJID", "TRT01P", "AGEGR1")])
#'
#' # The row count matches the number displayed in the cell
#' built$res1[1]
#'
#' @export
tplyr_meta_subset <- function(result, row_id, column, data, pop_data = NULL) {
  meta_obj <- tplyr_meta_result(result, row_id, column)
  if (is.null(meta_obj)) return(NULL)

  # An empty filter set means "unrestricted", not "nothing matches". A cell can
  # legitimately have no filters -- a total_group() column (which spans every
  # level of the column variable, so contributes no filter) crossed with a row
  # that carries none either, such as a total row or a desc statistic in a layer
  # with no `by` variable. Returning zero rows there made the round trip
  # disagree with a cell that is, correctly, the whole dataset.
  if (length(meta_obj$filters) == 0 && is.null(meta_obj$anti_join)) {
    out <- data
    rownames(out) <- NULL
    return(out)
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

#' Build an inclusion filter expression
#' @param var_name Character name of the variable
#' @param values Vector of values for the inclusion set
#' @keywords internal
make_in_filter <- function(var_name, values) {
  call("%in%", as.name(var_name), values)
}

#' Build an exclusion filter expression
#' @param var_name Character name of the variable
#' @param values Vector of values to exclude
#' @keywords internal
make_not_in_filter <- function(var_name, values) {
  call("!", call("%in%", as.name(var_name), values))
}

#' Build an is.na() filter expression
#' @keywords internal
make_is_na_filter <- function(var_name) {
  call("is.na", as.name(var_name))
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
# Variable mapping (internal)
# =============================================================================

#' Map data variable names to their rowlabel columns
#'
#' @param layer A tplyr_layer object
#' @param by_data_vars Character vector of by-variable data column names
#' @param by_labels Character vector of by-variable label strings
#'
#' @return Named list where names are data variables and values are rowlabel
#'   column names (e.g., \code{list(SEX = "rowlabel1")})
#' @keywords internal
build_var_to_rowlabel_map <- function(layer, by_data_vars, by_labels) {
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
  res_cols <- sort_by_numeric_suffix(str_subset(names(output), "^res\\d+$"))
  n_rows <- nrow(output)
  n_res <- length(res_cols)

  # A `stats_as_columns` desc layer with no `by` variable transposes the block:
  # treatment groups become rows and the result columns are named after the
  # statistics rather than res1, res2, .... Cell metadata is keyed on res
  # columns, so that layout carries none. Say so instead of returning silently.
  no_meta <- map_lgl(spec$layers, function(layer) {
    inherits(layer, "tplyr_desc_layer") &&
      isTRUE(layer$settings$stats_as_columns) &&
      length(classify_by(layer$by, col_names)$data_vars) == 0
  })
  if (any(no_meta)) {
    warning(str_glue(
      "Layer(s) {str_c(which(no_meta), collapse = ', ')}: cell metadata is not ",
      "available for a stats_as_columns layer without a `by` variable, because ",
      "that layout names its result columns after the statistics rather than ",
      "res1, res2, .... Add a `by` variable to get metadata for those cells."
    ), call. = FALSE)
  }

  if (n_res == 0) return(list())

  # Parse column variable levels from res column labels. The "(N=n)" suffix
  # sits at the end of the label, or after the column-group segment when
  # stat_columns appends a " | <stat>" part.
  col_level_map <- list()
  for (rc in res_cols) {
    lbl <- attr(output[[rc]], "label")
    if (!is.null(lbl)) {
      col_level_map[[rc]] <- str_replace(lbl, "\\s*\\(N=\\d+\\)(?= \\| |$)", "")
    }
  }

  # Generate row IDs
  row_ids <- generate_row_ids(output)
  output_names <- names(output)

  # Pre-trim all rowlabel columns as character vectors (avoids per-row
  # str_trim + as.character + str_length calls)
  rl_col_names <- str_subset(output_names, "^rowlabel\\d+$")
  trimmed <- vector("list", length(rl_col_names))
  names(trimmed) <- rl_col_names
  # `trimmed` decides whether a label is present; `untrimmed` supplies the value
  # a filter compares against, since a by variable's own values may legitimately
  # carry leading or trailing whitespace (common in SAS-derived character data)
  # and a trimmed literal would match nothing.
  untrimmed <- vector("list", length(rl_col_names))
  names(untrimmed) <- rl_col_names
  for (rl_col in rl_col_names) {
    raw <- as.character(output[[rl_col]])
    untrimmed[[rl_col]] <- raw
    trimmed[[rl_col]] <- trimws(raw)
  }

  # Pre-compute all cell keys as a matrix: row_ids[i] || res_cols[j]
  # Stored as a vector in row-major order for fast indexing
  all_keys <- as.vector(outer(row_ids, res_cols, paste, sep = "||"))
  # all_keys[(row_idx - 1) * n_res + ri] gives the key for (row_idx, ri)

  # --- Pre-compute per-layer info (avoids repeated work per row) ---
  layer_cache <- vector("list", length(spec$layers))
  layer_idx_col <- output$ord_layer_index

  # Position-indexed row type vector (O(1) lookup, filled per-layer below)
  all_row_types <- rep("normal", n_rows)

  # Set when a missing-subjects row had to be skipped for want of a subject key
  unkeyed_missing_subjects <- FALSE

  for (li in seq_along(spec$layers)) {
    layer <- spec$layers[[li]]
    by_info <- classify_by(layer$by, col_names)
    var_to_rl <- build_var_to_rowlabel_map(
      layer, by_info$data_vars, by_info$labels
    )

    # Pre-compute where filters (same for every cell in this layer)
    where_filters <- list()
    where_names <- character(0)
    if (!is.null(spec$where) && !identical(spec$where, TRUE)) {
      where_filters <- c(where_filters, list(spec$where))
      where_names <- c(where_names, all.vars(spec$where))
    }
    if (!is.null(layer$where) && !identical(layer$where, TRUE)) {
      where_filters <- c(where_filters, list(layer$where))
      where_names <- c(where_names, all.vars(layer$where))
    }

    # Pre-compute column-level filters per res_col for this layer
    col_cache <- vector("list", n_res)
    names(col_cache) <- res_cols
    is_shift <- inherits(layer, "tplyr_shift_layer")
    # Both count-layer `stat_columns` and desc-layer `stats_as_columns` label
    # their result columns "<column group> (N=n) | <statistic>", so the trailing
    # statistic segment has to be stripped before the label is resolved back to
    # column-variable values. Missing the desc case made every filter read
    # `TRT == "A | n"` and match nothing.
    has_stat_cols <- (inherits(layer, "tplyr_count_layer") &&
                        !is.null(layer$settings$stat_columns)) ||
      (inherits(layer, "tplyr_desc_layer") &&
         isTRUE(layer$settings$stats_as_columns))
    for (ri in seq_along(res_cols)) {
      rc <- res_cols[ri]
      cf <- list()
      cn <- character(0)
      stat_lbl <- NULL
      if (has_stat_cols && length(cols) == 0) {
        # No column variables: the label is the stat name itself
        stat_lbl <- col_level_map[[rc]]
      }
      if (rc %in% names(col_level_map) && length(cols) > 0) {
        col_level <- col_level_map[[rc]]
        if (has_stat_cols) {
          # stat_columns labels append " | <stat>" after the column-group
          # segment; strip it before resolving column filters and record it
          # as the cell's statistic
          parts <- str_split(col_level, fixed(" | "))[[1]]
          if (length(parts) > length(cols)) {
            stat_lbl <- parts[length(cols) + 1]
          }
          col_level <- str_c(parts[seq_len(min(length(cols), length(parts)))],
                             collapse = " | ")
        }
        if (is_shift) {
          parts <- str_split(col_level, fixed(" | "))[[1]]
          for (ci in seq_along(cols)) {
            resolved <- translate_group_value(
              parts[ci], cols[ci], spec$total_groups, spec$custom_groups
            )
            cf <- c(cf, resolved$filters)
            cn <- c(cn, cols[ci])
          }
          shift_col_var <- layer$target_var["column"]
          if (length(parts) > length(cols)) {
            cf <- c(cf, list(make_eq_filter(shift_col_var, parts[length(cols) + 1])))
            cn <- c(cn, shift_col_var)
          }
        } else if (length(cols) == 1) {
          resolved <- translate_group_value(
            col_level, cols[1], spec$total_groups, spec$custom_groups
          )
          cf <- c(cf, resolved$filters)
          cn <- c(cn, cols[1])
        } else {
          parts <- str_split(col_level, fixed(" | "))[[1]]
          for (ci in seq_along(cols)) {
            resolved <- translate_group_value(
              parts[ci], cols[ci], spec$total_groups, spec$custom_groups
            )
            cf <- c(cf, resolved$filters)
            cn <- c(cn, cols[ci])
          }
        }
      }

      # Pre-compute pop-side column filters for anti-join (remapped if needed)
      pop_cf <- cf
      pop_cn <- cn
      if (!is.null(pop_col_map) && !identical(unname(pop_col_map), cols)) {
        orig_pop_cols <- unname(pop_col_map)
        spec_col_names <- if (!is.null(names(pop_col_map))) names(pop_col_map) else cols
        pop_cf <- map(cf, function(f) {
          for (ci in seq_along(spec_col_names)) {
            f <- do.call(substitute, list(f, setNames(
              list(as.name(orig_pop_cols[ci])), spec_col_names[ci]
            )))
          }
          f
        })
        pop_cn <- orig_pop_cols
      }

      col_cache[[ri]] <- list(
        filters = cf, names = cn,
        pop_filters = pop_cf, pop_names = pop_cn,
        statistic = stat_lbl
      )
    }

    # Pre-compute pop_data where filter
    pop_where_filters <- list()
    pop_where_names <- character(0)
    if (!is.null(spec$pop_data) && !is.null(spec$pop_data$where) &&
        !identical(spec$pop_data$where, TRUE)) {
      pop_where_filters <- list(spec$pop_data$where)
      pop_where_names <- all.vars(spec$pop_data$where)
    }

    # Pre-compute layer type flags and reusable values
    is_count <- inherits(layer, "tplyr_count_layer")
    is_desc_or_analyze <- inherits(layer, "tplyr_desc_layer") ||
                          inherits(layer, "tplyr_analyze_layer")
    settings <- layer$settings

    # Pre-classify all rows belonging to this layer at once.
    # Store in a position-indexed vector (NOT named) for O(1) lookup.
    layer_row_indices <- which(layer_idx_col == li)

    if ((is_count || is_shift) && length(layer_row_indices) > 0) {
      tv <- if (is_count) layer$target_var[1] else layer$target_var["row"]
      tv_rl_col <- var_to_rl[[tv]]

      if (!is.null(tv_rl_col) && tv_rl_col %in% output_names) {
        rl_vals <- trimmed[[tv_rl_col]][layer_row_indices]
        row_types_for_layer <- rep("normal", length(layer_row_indices))

        # Check total row
        total_label <- settings$total_row_label %||% "Total"
        if (isTRUE(settings$total_row)) {
          row_types_for_layer[rl_vals == total_label] <- "total"
        }

        # Check missing count row (takes priority over missing subjects)
        if (!is.null(settings$missing_count)) {
          missing_label <- settings$missing_count$label %||% "Missing"
          row_types_for_layer[rl_vals == missing_label &
                              row_types_for_layer == "normal"] <- "missing"
        }

        # Check missing subjects row
        if (isTRUE(settings$missing_subjects)) {
          ms_label <- settings$missing_subjects_label %||% "Missing"
          row_types_for_layer[rl_vals == ms_label &
                              row_types_for_layer == "normal"] <- "missing_subjects"
        }

        # Write into global row_types array at the correct positions
        all_row_types[layer_row_indices] <- row_types_for_layer
      }
    }

    # For each res_col, pre-compute the unique union of cc$names with
    # the layer's where_names and desc/analyze target_var names.
    # The row-specific names still need to be added per row, but we can
    # avoid recomputing the column+layer portion.
    col_base_names <- vector("list", n_res)
    for (ri in seq_along(res_cols)) {
      base <- col_cache[[ri]]$names
      if (is_desc_or_analyze) {
        base <- c(base, layer$target_var)
      }
      base <- c(base, where_names)
      col_base_names[[ri]] <- base
    }

    # Pre-compute which by_data_vars have valid rowlabel columns
    valid_by_vars <- character(0)
    valid_by_rl_cols <- character(0)
    for (bv in by_info$data_vars) {
      rl_col <- var_to_rl[[bv]]
      if (!is.null(rl_col) && rl_col %in% output_names) {
        valid_by_vars <- c(valid_by_vars, bv)
        valid_by_rl_cols <- c(valid_by_rl_cols, rl_col)
      }
    }

    # Pre-compute which var_to_rl entries have valid output columns
    valid_vrl_names <- character(0)
    valid_vrl_cols <- character(0)
    for (var_name in names(var_to_rl)) {
      rl_col <- var_to_rl[[var_name]]
      if (rl_col %in% output_names) {
        valid_vrl_names <- c(valid_vrl_names, var_name)
        valid_vrl_cols <- c(valid_vrl_cols, rl_col)
      }
    }

    layer_cache[[li]] <- list(
      layer = layer,
      by_data_vars = by_info$data_vars,
      var_to_rl = var_to_rl,
      where_filters = where_filters,
      where_names = where_names,
      col_cache = col_cache,
      col_base_names = col_base_names,
      pop_where_filters = pop_where_filters,
      pop_where_names = pop_where_names,
      is_shift = is_shift,
      is_count = is_count,
      is_desc_or_analyze = is_desc_or_analyze,
      settings = settings,
      valid_by_vars = valid_by_vars,
      valid_by_rl_cols = valid_by_rl_cols,
      valid_vrl_names = valid_vrl_names,
      valid_vrl_cols = valid_vrl_cols
    )
  }

  # --- Pre-allocate output (avoids O(n^2) named list growth) ---
  n_cells <- n_rows * n_res
  keys <- character(n_cells)
  vals <- vector("list", n_cells)
  cell_idx <- 0L

  for (row_idx in seq_len(n_rows)) {
    layer_idx <- layer_idx_col[row_idx]
    if (is.na(layer_idx) || layer_idx < 1L ||
        layer_idx > length(spec$layers)) next

    lc <- layer_cache[[layer_idx]]
    layer <- lc$layer
    settings <- lc$settings

    # Look up pre-computed row type (O(1) vector index)
    row_type <- all_row_types[row_idx]

    # Use pre-trimmed values for by-variable filters
    n_by <- length(lc$valid_by_vars)
    if (n_by > 0) {
      by_f <- vector("list", n_by)
      by_n <- character(n_by)
      by_count <- 0L
      for (bi in seq_len(n_by)) {
        rl_col <- lc$valid_by_rl_cols[bi]
        bv_val <- untrimmed[[rl_col]][row_idx]
        # A by variable has a value on every row, so an NA label means the data
        # itself is NA (filter on that) and an empty string is a real level
        # (filter on it) -- neither is an absent label to be skipped.
        by_count <- by_count + 1L
        by_f[[by_count]] <- if (is.na(bv_val)) {
          make_is_na_filter(lc$valid_by_vars[bi])
        } else {
          make_eq_filter(lc$valid_by_vars[bi], bv_val)
        }
        by_n[by_count] <- lc$valid_by_vars[bi]
      }
      if (by_count < n_by) {
        by_f <- by_f[seq_len(by_count)]
        by_n <- by_n[seq_len(by_count)]
      }
    } else {
      by_f <- list()
      by_n <- character(0)
    }

    # Use pre-trimmed values for row-specific filters
    row_filters <- list()
    row_names <- character(0)

    if (row_type == "normal") {
      n_vrl <- length(lc$valid_vrl_names)
      if (n_vrl > 0) {
        rf <- vector("list", n_vrl)
        rn <- character(n_vrl)
        rf_count <- 0L
        for (vi in seq_len(n_vrl)) {
          vname <- lc$valid_vrl_names[vi]
          rl_col <- lc$valid_vrl_cols[vi]
          present <- trimmed[[rl_col]][row_idx]
          raw_val <- untrimmed[[rl_col]][row_idx]

          # A `by` data variable has a value on every row, so a blank or NA
          # label is a real level of that variable and must still be filtered
          # on. A target variable is different: a nested layer leaves the inner
          # label empty on an outer-level row, and that absence means "no
          # filter".
          is_by_var <- vname %in% lc$valid_by_vars
          if (!is_by_var && (is.na(present) || !nzchar(present))) next

          rf_count <- rf_count + 1L
          rf[[rf_count]] <- if (is.na(raw_val)) {
            make_is_na_filter(vname)
          } else {
            make_eq_filter(vname, raw_val)
          }
          rn[rf_count] <- vname
        }
        row_filters <- rf[seq_len(rf_count)]
        row_names <- rn[seq_len(rf_count)]
      }
    } else if (row_type == "total") {
      tv <- if (lc$is_count) layer$target_var[1] else layer$target_var["row"]
      row_names <- tv
      if (!isTRUE(settings$total_row_count_missings) &&
          !is.null(settings$missing_count)) {
        missing_values <- settings$missing_count$missing_values %||% character(0)
        if (length(missing_values) > 0) {
          row_filters <- list(make_not_in_filter(tv, missing_values),
                              make_not_na_filter(tv))
        } else {
          row_filters <- list(make_not_na_filter(tv))
        }
      }
      row_filters <- c(row_filters, by_f)
      row_names <- c(row_names, by_n)
    } else if (row_type == "missing") {
      tv <- layer$target_var[1]
      missing_values <- settings$missing_count$missing_values %||% character(0)
      row_filters <- c(list(make_missing_filter(tv, missing_values)), by_f)
      row_names <- c(tv, by_n)
    } else if (row_type == "missing_subjects") {
      row_filters <- by_f
      row_names <- c(by_n, layer$target_var[1])
    }

    # Add where filters (cached per layer)
    row_filters <- c(row_filters, lc$where_filters)
    row_names <- c(row_names, lc$where_names)

    # desc/analyze: add target_var to names
    if (lc$is_desc_or_analyze) {
      row_names <- c(row_names, layer$target_var)
    }

    # --- Combine with each column's pre-computed filters ---
    needs_anti_join <- row_type == "missing_subjects" &&
      !is.null(settings$distinct_by) && length(settings$distinct_by) > 0

    # Without `distinct_by` there is no subject key to anti-join on, so a
    # missing-subjects row is a pure arithmetic difference (population rows minus
    # target rows) that no filter set describes. The filters built above would
    # resolve to the subjects that *do* appear -- the exact complement of what the
    # cell counts -- so emit no metadata for it rather than the wrong metadata.
    if (row_type == "missing_subjects" && !needs_anti_join) {
      unkeyed_missing_subjects <- TRUE
      next
    }

    for (ri in seq_along(res_cols)) {
      cc <- lc$col_cache[[ri]]

      all_filters <- c(cc$filters, row_filters)
      # Use pre-computed base names + row_names
      all_names <- unique(c(lc$col_base_names[[ri]], row_names))

      # Build anti-join for missing_subjects rows
      aj <- NULL
      if (needs_anti_join) {
        pop_filters <- c(cc$pop_filters, by_f, lc$pop_where_filters)
        pop_names <- unique(c(cc$pop_names, by_n, lc$pop_where_names))
        aj <- tplyr_meta_anti_join(
          join_meta = tplyr_meta(
            names = pop_names,
            filters = pop_filters,
            layer_index = as.integer(layer_idx)
          ),
          on = settings$distinct_by
        )
      }

      cell_idx <- cell_idx + 1L
      # Index into pre-computed keys (column-major from outer())
      keys[cell_idx] <- all_keys[(ri - 1L) * n_rows + row_idx]
      vals[[cell_idx]] <- tplyr_meta(
        names = all_names,
        filters = all_filters,
        layer_index = as.integer(layer_idx),
        anti_join = aj,
        statistic = cc$statistic
      )
    }
  }

  if (unkeyed_missing_subjects) {
    warning(str_glue(
      "Cell metadata was not produced for the missing-subjects row(s): without ",
      "`distinct_by` there is no subject key to anti-join on, so the row is a ",
      "population-minus-target difference that no filter set can describe. Set ",
      "`distinct_by` to get traceable missing-subjects cells."
    ), call. = FALSE)
  }

  # Trim to actual size and set names once (O(n) instead of O(n^2))
  vals <- vals[seq_len(cell_idx)]
  names(vals) <- keys[seq_len(cell_idx)]
  vals
}
