#' Build a tplyr2 table from a spec and data
#'
#' Executes the table specification against the provided data, producing
#' a formatted output data frame.
#'
#' @param spec A tplyr_spec object (or path to a JSON/YAML spec file)
#' @param data A data.frame to process
#' @param pop_data Optional population data.frame (overrides spec pop_data)
#' @param metadata If TRUE, attach cell-level metadata enabling traceability
#'   back to source data rows via \code{tplyr_meta_result()} and
#'   \code{tplyr_meta_subset()}.
#' @param ... Additional named arguments to override spec-level parameters
#'
#' @return A data.frame with rowlabel, res, and ord columns
#' @export
tplyr_build <- function(spec, data, pop_data = NULL, metadata = FALSE, ...) {
  # If spec is a file path, read it
  if (is.character(spec) && length(spec) == 1 && file.exists(spec)) {
    spec <- tplyr_read_spec(spec)
  }

  # Override scipen to prevent scientific notation in formatted output
  old_scipen <- getOption("scipen")
  options(scipen = getOption("tplyr2.scipen", 9999L))
  on.exit(options(scipen = old_scipen), add = TRUE)

  overrides <- list(...)

  # Apply overrides to a copy of the spec
  spec <- apply_overrides(spec, overrides)

  # Structural validation
  validate_spec(spec)

  cols <- spec$cols

  # Convert to data.table (copy to avoid modifying user's data)
  dt <- data.table::as.data.table(data)

  # Data compatibility validation
  validate_build_data(spec, dt)

  # Apply global where filter
  if (!is.null(spec$where) && !identical(spec$where, TRUE)) {
    dt <- dt[eval(spec$where)]
  }

  # --- Resolve population data ---
  pop_config <- spec$pop_data
  pop_dt <- NULL
  header_n <- NULL

  if (!is.null(pop_data) && is.data.frame(pop_data)) {
    pop_dt <- data.table::as.data.table(pop_data)

    # Apply pop_data where filter from config
    if (!is.null(pop_config) && !is.null(pop_config$where) &&
        !identical(pop_config$where, TRUE)) {
      pop_dt <- pop_dt[eval(pop_config$where)]
    }
  }

  # --- Apply custom groups and total groups ---
  dt <- apply_custom_groups(dt, spec$custom_groups)
  dt <- apply_total_groups(dt, spec$total_groups)
  if (!is.null(pop_dt)) {
    pop_dt <- apply_custom_groups(pop_dt, spec$custom_groups)
    pop_dt <- apply_total_groups(pop_dt, spec$total_groups)
  }

  # --- Compute header N per column group ---
  if (length(cols) > 0) {
    if (!is.null(pop_dt)) {
      pop_cols <- resolve_pop_cols(pop_config, cols)
      # Rename pop_dt columns to match spec cols so all downstream code works
      if (!identical(unname(pop_cols), cols)) {
        new_names <- if (!is.null(names(pop_cols))) names(pop_cols) else cols
        data.table::setnames(pop_dt, unname(pop_cols), new_names)
      }
      col_n <- pop_dt[, list(.n = .N), by = cols]
      header_n <- data.table::copy(col_n)
    } else {
      col_n <- dt[, list(.n = .N), by = cols]
    }
  } else {
    col_n <- NULL
  }

  # Process each layer
  layer_results <- vector("list", length(spec$layers))
  numeric_data <- list()

  for (i in seq_along(spec$layers)) {
    layer <- spec$layers[[i]]

    # Dispatch to layer-specific processor
    layer_dt <- data.table::copy(dt)

    if (inherits(layer, "tplyr_count_layer")) {
      layer_results[[i]] <- build_count_layer(layer_dt, layer, cols, i,
                                               col_n = col_n, pop_dt = pop_dt)
    } else if (inherits(layer, "tplyr_desc_layer")) {
      layer_results[[i]] <- build_desc_layer(layer_dt, layer, cols, i,
                                              col_n = col_n, pop_dt = pop_dt)
    } else if (inherits(layer, "tplyr_shift_layer")) {
      layer_results[[i]] <- build_shift_layer(layer_dt, layer, cols, i,
                                               col_n = col_n, pop_dt = pop_dt)
    } else if (inherits(layer, "tplyr_analyze_layer")) {
      layer_results[[i]] <- build_analyze_layer(layer_dt, layer, cols, i,
                                                 col_n = col_n, pop_dt = pop_dt)
    } else {
      stop("Unknown layer type: ", class(layer)[1])
    }

    # Collect numeric data from layer result
    nd <- attr(layer_results[[i]], "numeric_data")
    if (!is.null(nd)) {
      numeric_data[[as.character(i)]] <- nd
    }
  }

  # Harmonize columns across layers before binding
  result <- harmonize_and_bind(layer_results)

  # Sort by layer index first, then within-layer ordering columns
  all_ord <- str_subset(names(result), "^ord")
  other_ord <- sort(setdiff(all_ord, "ordindx"))
  ord_cols <- c("ordindx", other_ord)
  data.table::setorderv(result, ord_cols)

  # Rename ordering columns to match DESIGN.md convention
  rename_ord_columns(result)

  # Convert to data.frame for output
  output <- as.data.frame(result)

  # Attach header N attribute (after as.data.frame to ensure survival)
  if (!is.null(header_n)) {
    attr(output, "header_n") <- as.data.frame(header_n)
  }

  # Attach numeric data (after as.data.frame to ensure survival)
  if (length(numeric_data) > 0) {
    attr(output, "numeric_data") <- numeric_data
  }

  # Attach metadata (row_id column + cell-level traceability)
  if (metadata) {
    output$row_id <- generate_row_ids(output)
    pop_col_map <- if (!is.null(pop_config)) resolve_pop_cols(pop_config, cols) else NULL
    cell_meta <- build_cell_metadata(output, spec, names(data), pop_col_map)
    attr(output, "tplyr_meta") <- cell_meta
  }

  output
}

#' Apply build-time overrides to a spec
#'
#' Merges override parameters into a copy of the spec. Handles special cases:
#' \itemize{
#'   \item \code{where}: If character, parsed to an expression via
#'     \code{rlang::parse_expr()}.
#'   \item \code{pop_data}: If a list, sub-fields are merged into the existing
#'     pop_data config rather than replacing it entirely.
#' }
#'
#' @param spec A tplyr_spec object
#' @param overrides Named list of override values
#' @return Modified spec (shallow copy)
#' @keywords internal
apply_overrides <- function(spec, overrides) {
  if (length(overrides) == 0) return(spec)

  for (name in names(overrides)) {
    value <- overrides[[name]]

    if (name == "where") {
      # Parse character string to expression if needed
      if (is.character(value) && length(value) == 1) {
        spec$where <- rlang::parse_expr(value)
      } else {
        spec$where <- value
      }
    } else if (name == "pop_data" && is.list(value) && !is.null(spec$pop_data)) {
      # Merge pop_data sub-fields instead of replacing entirely
      for (pd_name in names(value)) {
        spec$pop_data[[pd_name]] <- value[[pd_name]]
      }
    } else if (name %in% names(spec)) {
      spec[[name]] <- value
    }
  }

  spec
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
  label_cols <- sort(str_subset(all_names, "^rowlabel"))
  res_cols <- sort(str_subset(all_names, "^res\\d"))
  rdiff_cols <- sort(str_subset(all_names, "^rdiff"))
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

#' Apply total groups to data
#'
#' Duplicates all rows with the column variable set to the total group label,
#' creating a synthetic "Total" column level.
#'
#' @param dt data.table
#' @param total_groups List of tplyr_total_group objects (or NULL)
#' @return Modified data.table
#' @keywords internal
apply_total_groups <- function(dt, total_groups) {
  if (is.null(total_groups) || length(total_groups) == 0) return(dt)

  for (tg in total_groups) {
    col_var <- tg$col_var
    label <- tg$label

    total_rows <- data.table::copy(dt)
    total_rows[, (col_var) := label]

    dt <- data.table::rbindlist(list(dt, total_rows), use.names = TRUE, fill = TRUE)
  }

  dt
}

#' Apply custom column groups to data
#'
#' Duplicates rows matching source levels with the column variable set to
#' the custom group name.
#'
#' @param dt data.table
#' @param custom_groups List of tplyr_custom_group objects (or NULL)
#' @return Modified data.table
#' @keywords internal
apply_custom_groups <- function(dt, custom_groups) {
  if (is.null(custom_groups) || length(custom_groups) == 0) return(dt)

  for (cg in custom_groups) {
    col_var <- cg$col_var
    for (group_name in names(cg$groups)) {
      source_levels <- cg$groups[[group_name]]
      matching <- dt[get(col_var) %in% source_levels]
      if (nrow(matching) > 0) {
        group_rows <- data.table::copy(matching)
        group_rows[, (col_var) := group_name]
        dt <- data.table::rbindlist(list(dt, group_rows), use.names = TRUE, fill = TRUE)
      }
    }
  }

  dt
}
