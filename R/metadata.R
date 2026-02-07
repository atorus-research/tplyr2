#' Metadata object for a tplyr output cell
#'
#' Contains the source data row indices that contributed to a specific cell
#' in the output table.
#'
#' @param row_indices Integer vector of row indices into the original data
#' @param layer_index Integer layer index (1-based)
#'
#' @return A tplyr_meta object
#' @keywords internal
tplyr_meta <- function(row_indices, layer_index) {
  structure(
    list(
      row_indices = row_indices,
      layer_index = layer_index
    ),
    class = "tplyr_meta"
  )
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
  rowlabel_cols <- sort(grep("^rowlabel\\d+$", names(result), value = TRUE))
  layer_part <- as.character(result$ord_layer_index)

  if (length(rowlabel_cols) == 0) {
    return(paste0("r", layer_part, "_", seq_len(nrow(result))))
  }

  parts <- lapply(rowlabel_cols, function(col) as.character(result[[col]]))
  do.call(paste, c(list(layer_part), parts, list(sep = "_")))
}

#' Get metadata for a specific output cell
#'
#' Returns a \code{tplyr_meta} object containing the source data row indices
#' that contributed to the specified cell.
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
#' Subsets the original data to return only the rows that contributed to
#' the specified output cell.
#'
#' @param result A data.frame from \code{tplyr_build()} built with
#'   \code{metadata = TRUE}
#' @param row_id Character row ID
#' @param column Character column name (e.g., \code{"res1"})
#' @param data The original data.frame that was passed to \code{tplyr_build()}
#'
#' @return A data.frame subset of the original data, or NULL if no metadata
#' @export
tplyr_meta_subset <- function(result, row_id, column, data) {
  meta_obj <- tplyr_meta_result(result, row_id, column)
  if (is.null(meta_obj)) return(NULL)

  idx <- meta_obj$row_indices
  if (length(idx) == 0) return(data[0, , drop = FALSE])

  data[idx, , drop = FALSE]
}

# ---- Internal metadata computation ----

#' Compute group-level row index mapping for a layer
#'
#' Groups the data by the layer's grouping variables and collects the
#' \code{.orig_row_idx} values for each group.
#'
#' @param dt data.table with \code{.orig_row_idx} column
#' @param layer A tplyr_layer object
#' @param cols Character vector of spec-level column variables
#' @param col_names Character vector of all data column names
#'
#' @return A data.table with group variables and a \code{.row_indices} list
#'   column, or NULL if no row index tracking
#' @keywords internal
compute_layer_cell_meta <- function(dt, layer, cols, col_names) {
  if (!".orig_row_idx" %in% names(dt)) return(NULL)

  # Apply layer where filter
  if (!is.null(layer$where) && !identical(layer$where, TRUE)) {
    dt <- dt[eval(layer$where)]
  }

  by_info <- classify_by(layer$by, col_names)
  by_data_vars <- by_info$data_vars

  if (inherits(layer, "tplyr_count_layer")) {
    group_vars <- c(cols, by_data_vars, layer$target_var)
  } else if (inherits(layer, "tplyr_desc_layer") ||
             inherits(layer, "tplyr_analyze_layer")) {
    group_vars <- c(cols, by_data_vars)
  } else if (inherits(layer, "tplyr_shift_layer")) {
    group_vars <- c(cols, layer$target_var["column"],
                    by_data_vars, layer$target_var["row"])
  } else {
    group_vars <- c(cols, by_data_vars)
  }

  # Only use group_vars that exist in dt
  group_vars <- intersect(group_vars, names(dt))

  if (length(group_vars) == 0) {
    return(data.table::data.table(.row_indices = list(dt$.orig_row_idx)))
  }

  dt[, list(.row_indices = list(.orig_row_idx)), by = group_vars]
}

#' Build cell-level metadata for the full output table
#'
#' For each output row x result column combination, determines which source
#' data rows contributed by matching rowlabel values and column labels back
#' to the layer's group-level row index mapping.
#'
#' @param output data.frame output from tplyr_build (with rowlabel/res/ord cols)
#' @param dt data.table with \code{.orig_row_idx} (after global where + groups)
#' @param spec tplyr_spec object
#' @param col_names Character vector of original data column names
#'
#' @return Named list of tplyr_meta objects, keyed by "row_id||column"
#' @keywords internal
build_cell_metadata <- function(output, dt, spec, col_names) {
  cols <- spec$cols
  rowlabel_cols <- sort(grep("^rowlabel\\d+$", names(output), value = TRUE))
  res_cols <- sort(grep("^res\\d+$", names(output), value = TRUE))

  if (length(res_cols) == 0) return(list())

  # Parse column variable levels from res column labels
  col_level_map <- list()
  for (rc in res_cols) {
    lbl <- attr(output[[rc]], "label")
    if (!is.null(lbl)) {
      col_level_map[[rc]] <- sub("\\s*\\(N=\\d+\\)$", "", lbl)
    }
  }

  # Pre-compute layer metadata (group -> row indices mapping)
  layer_metas <- vector("list", length(spec$layers))
  for (i in seq_along(spec$layers)) {
    layer_metas[[i]] <- compute_layer_cell_meta(
      dt, spec$layers[[i]], cols, col_names
    )
  }

  # Generate row IDs
  row_ids <- generate_row_ids(output)

  cells <- list()

  for (row_idx in seq_len(nrow(output))) {
    layer_idx <- output$ord_layer_index[row_idx]
    if (is.na(layer_idx) || layer_idx < 1 ||
        layer_idx > length(spec$layers)) next

    layer <- spec$layers[[layer_idx]]
    layer_meta <- layer_metas[[layer_idx]]
    if (is.null(layer_meta) || nrow(layer_meta) == 0) next

    # Build variable-to-rowlabel column mapping
    by_info <- classify_by(layer$by, col_names)
    by_data_vars <- by_info$data_vars
    by_labels <- by_info$labels

    var_to_rl <- list()
    rl_idx <- length(by_labels) + 1L

    for (bv in by_data_vars) {
      var_to_rl[[bv]] <- paste0("rowlabel", rl_idx)
      rl_idx <- rl_idx + 1L
    }

    if (inherits(layer, "tplyr_count_layer")) {
      for (tv in layer$target_var) {
        var_to_rl[[tv]] <- paste0("rowlabel", rl_idx)
        rl_idx <- rl_idx + 1L
      }
    } else if (inherits(layer, "tplyr_shift_layer")) {
      var_to_rl[[layer$target_var["row"]]] <- paste0("rowlabel", rl_idx)
    }
    # For desc/analyze layers: last rowlabel is stat name (not a data variable)

    for (rc in res_cols) {
      mask <- rep(TRUE, nrow(layer_meta))

      # Match column variable levels
      if (rc %in% names(col_level_map) && length(cols) > 0) {
        col_level <- col_level_map[[rc]]

        if (inherits(layer, "tplyr_shift_layer")) {
          # Shift: label is "cols_val | shift_col_val"
          parts <- strsplit(col_level, " | ", fixed = TRUE)[[1]]
          for (ci in seq_along(cols)) {
            if (cols[ci] %in% names(layer_meta)) {
              mask <- mask &
                as.character(layer_meta[[cols[ci]]]) == parts[ci]
            }
          }
          # Shift column var is after cols parts
          shift_col_var <- layer$target_var["column"]
          if (shift_col_var %in% names(layer_meta) &&
              length(parts) > length(cols)) {
            mask <- mask &
              as.character(layer_meta[[shift_col_var]]) ==
                parts[length(cols) + 1]
          }
        } else if (length(cols) == 1) {
          if (cols[1] %in% names(layer_meta)) {
            mask <- mask &
              as.character(layer_meta[[cols[1]]]) == col_level
          }
        } else {
          parts <- strsplit(col_level, " | ", fixed = TRUE)[[1]]
          for (ci in seq_along(cols)) {
            if (cols[ci] %in% names(layer_meta)) {
              mask <- mask &
                as.character(layer_meta[[cols[ci]]]) == parts[ci]
            }
          }
        }
      }

      # Match row variables from rowlabel values
      for (var_name in names(var_to_rl)) {
        rl_col <- var_to_rl[[var_name]]
        if (rl_col %in% names(output) && var_name %in% names(layer_meta)) {
          rl_val <- as.character(output[[rl_col]][row_idx])
          # Skip empty values (e.g., inner levels of nested counts)
          if (nchar(trimws(rl_val)) > 0) {
            mask <- mask &
              as.character(layer_meta[[var_name]]) == rl_val
          }
        }
      }

      matching <- layer_meta[mask]
      row_indices <- if (nrow(matching) > 0) {
        unlist(matching$.row_indices)
      } else {
        integer(0)
      }

      key <- paste(row_ids[row_idx], rc, sep = "||")
      cells[[key]] <- tplyr_meta(
        row_indices = as.integer(row_indices),
        layer_index = as.integer(layer_idx)
      )
    }
  }

  cells
}
