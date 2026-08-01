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
  # Custom summaries and assoc_test functions render failures as blank cells;
  # collect_user_fn_errors() reports the reasons as one deduplicated warning
  # when the build finishes.
  collect_user_fn_errors(
    tplyr_build_impl(spec, data, pop_data = pop_data, metadata = metadata, ...)
  )
}

#' @keywords internal
#' @noRd
tplyr_build_impl <- function(spec, data, pop_data = NULL, metadata = FALSE, ...) {
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

  # --- Pin the column-variable universe ---
  # Each layer derives its result columns from the levels present in its own
  # (where-filtered) data. A layer whose `where` leaves a column group empty
  # emitted fewer columns, and harmonize_and_bind() then lined those up
  # positionally under a neighbouring layer's labels — putting that layer's
  # values under the wrong treatment arm. Capturing the level set here, from the
  # table's full data, pins the column set for every layer. The underlying
  # columns are left untouched so nothing that reads the data's own ordering
  # (e.g. an assoc_test reference default) changes.
  spec_col_levels <- get_col_levels(dt, cols, complete = TRUE)

  # --- Compute header N per column group ---
  if (length(cols) > 0) {
    if (!is.null(pop_dt)) {
      pop_cols <- resolve_pop_cols(pop_config, cols)
      # Rename pop_dt columns to match spec cols so all downstream code works
      if (!identical(unname(pop_cols), cols)) {
        new_names <- if (!is.null(names(pop_cols))) names(pop_cols) else cols
        data.table::setnames(pop_dt, unname(pop_cols), new_names)
      }
      validate_pop_data_coverage(dt, pop_dt, cols)
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
                                               col_n = col_n, pop_dt = pop_dt,
                                               col_levels = spec_col_levels)
    } else if (inherits(layer, "tplyr_desc_layer")) {
      layer_results[[i]] <- build_desc_layer(layer_dt, layer, cols, i,
                                              col_n = col_n, pop_dt = pop_dt,
                                              col_levels = spec_col_levels)
    } else if (inherits(layer, "tplyr_shift_layer")) {
      layer_results[[i]] <- build_shift_layer(layer_dt, layer, cols, i,
                                               col_n = col_n, pop_dt = pop_dt,
                                               col_levels = spec_col_levels)
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
  sort_by_ord_columns(result)

  # Rename ordering columns to the public output names
  # (ordindx -> ord_layer_index, ord<n> -> ord_layer_<n>)
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

  # `...` disables R's own argument matching, so a typo'd override used to
  # build a plausible-looking table off the wrong settings — e.g.
  # tplyr_build(spec, adsl, wher = "SAFFL == 'Y'") silently ran unfiltered.
  valid <- union(names(spec), c("where", "pop_data"))
  supplied <- names(overrides) %||% rep("", length(overrides))
  if (any(supplied == "")) {
    stop("All arguments passed to tplyr_build() via ... must be named.",
         call. = FALSE)
  }
  unknown <- setdiff(supplied, valid)
  if (length(unknown) > 0) {
    stop("Unknown override", if (length(unknown) > 1) "s" else "", " passed to ",
         "tplyr_build(): ", str_c(unknown, collapse = ", "),
         "\nValid overrides: ", str_c(sort(valid), collapse = ", "),
         call. = FALSE)
  }

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

#' Name of the per-column-variable synthetic-row marker
#'
#' Records which column variable a duplicated row was created for, so a total
#' group can skip copies made on its own variable while still spanning copies
#' made for a different one.
#'
#' @param col_var Character(1) column variable name
#' @return Character(1) marker column name
#' @keywords internal
synth_marker <- function(col_var) {
  str_c(".tplyr_synth__", col_var)
}

#' Zero-fill synthetic markers after a bind
#'
#' `rbindlist(fill = TRUE)` sets a marker absent from one side to NA; those rows
#' are originals for that variable.
#'
#' @param dt data.table
#' @return `dt`
#' @keywords internal
fill_synth_markers <- function(dt) {
  for (mk in str_subset(names(dt), "^\\.tplyr_synth__")) {
    dt[is.na(get(mk)), (mk) := FALSE]
  }
  dt
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

    # Duplicate only the rows that are original *for this column variable*. A
    # custom group on the same variable is a pooled copy of levels already
    # present, so copying its rows too would count those subjects twice — a
    # 254-subject study reported N=422. Copies made for a *different* column
    # variable must be kept, or the total column would be empty where it crosses
    # that variable's synthetic level.
    marker <- synth_marker(col_var)
    src <- if (marker %in% names(dt)) dt[!get(marker)] else dt

    total_rows <- data.table::copy(src)
    total_rows[, (col_var) := label]

    # Mark the duplicates so a statistical test (assoc_test) can exclude them —
    # they are a display construct for the count columns and would otherwise
    # double-count every subject (#53). Originals default to FALSE.
    if (!".tplyr_synthetic" %in% names(dt)) dt[, .tplyr_synthetic := FALSE]
    total_rows[, .tplyr_synthetic := TRUE]
    if (!marker %in% names(dt)) dt[, (marker) := FALSE]
    total_rows[, (marker) := TRUE]

    dt <- data.table::rbindlist(list(dt, total_rows), use.names = TRUE, fill = TRUE)
    dt <- fill_synth_markers(dt)
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
        # Mark the duplicates so assoc_test can exclude them (see
        # apply_total_groups); originals default to FALSE. The per-variable
        # marker records *which* column variable the copy was made for, so a
        # total group on the same variable can skip it while a total group on a
        # different variable still spans it.
        marker <- synth_marker(col_var)
        if (!".tplyr_synthetic" %in% names(dt)) dt[, .tplyr_synthetic := FALSE]
        group_rows[, .tplyr_synthetic := TRUE]
        if (!marker %in% names(dt)) dt[, (marker) := FALSE]
        group_rows[, (marker) := TRUE]
        dt <- data.table::rbindlist(list(dt, group_rows), use.names = TRUE, fill = TRUE)
        dt <- fill_synth_markers(dt)
      }
    }
  }

  dt
}
