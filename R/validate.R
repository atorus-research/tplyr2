#' Validate a tplyr_spec object structurally
#'
#' Checks that the spec has the correct class and structure. Called after
#' overrides are applied but before data is processed.
#'
#' @param spec A tplyr_spec object
#' @return Invisible TRUE if valid, otherwise stops with informative error
#' @keywords internal
validate_spec <- function(spec) {
  if (!inherits(spec, "tplyr_spec")) {
    stop("'spec' must be a tplyr_spec object (got class: ",
         paste(class(spec), collapse = ", "), ")", call. = FALSE)
  }

  if (!is.null(spec$cols) && !is.character(spec$cols)) {
    stop("'cols' must be a character vector", call. = FALSE)
  }

  if (!is.list(spec$layers)) {
    stop("'layers' must be a list", call. = FALSE)
  }

  for (i in seq_along(spec$layers)) {
    if (!inherits(spec$layers[[i]], "tplyr_layer")) {
      stop(sprintf("Layer %d is not a tplyr_layer object (class: %s)",
                   i, paste(class(spec$layers[[i]]), collapse = ", ")),
           call. = FALSE)
    }
    validate_layer(spec$layers[[i]], i)
  }

  invisible(TRUE)
}

#' Validate a single layer
#'
#' @param layer A tplyr_layer object
#' @param index Integer layer index (for error messages)
#' @return Invisible TRUE if valid
#' @keywords internal
validate_layer <- function(layer, index) {
  if (!is.character(layer$target_var) || length(layer$target_var) == 0) {
    stop(sprintf("Layer %d: target_var must be a non-empty character vector",
                 index), call. = FALSE)
  }

  # Shift-specific: must be named with row and column
  if (inherits(layer, "tplyr_shift_layer")) {
    if (length(layer$target_var) != 2 ||
        is.null(names(layer$target_var)) ||
        !all(c("row", "column") %in% names(layer$target_var))) {
      stop(sprintf(
        "Layer %d: shift layer target_var must have names 'row' and 'column'",
        index), call. = FALSE)
    }
  }

  # Analyze-specific: must have a function
  if (inherits(layer, "tplyr_analyze_layer")) {
    if (is.null(layer$analyze_fn) || !is.function(layer$analyze_fn)) {
      stop(sprintf("Layer %d: analyze layer must have a valid analyze_fn",
                   index), call. = FALSE)
    }
  }

  # Validate format_strings if present
  if (!is.null(layer$settings$format_strings)) {
    validate_format_strings(layer$settings$format_strings, index)
  }

  invisible(TRUE)
}

#' Validate format strings in layer settings
#'
#' @param fmt_list A named list expected to contain f_str objects
#' @param layer_index Integer layer index (for error messages)
#' @return Invisible TRUE if valid
#' @keywords internal
validate_format_strings <- function(fmt_list, layer_index) {
  if (!is.list(fmt_list)) {
    stop(sprintf("Layer %d: format_strings must be a named list of f_str objects",
                 layer_index), call. = FALSE)
  }

  for (nm in names(fmt_list)) {
    if (!inherits(fmt_list[[nm]], "tplyr_f_str")) {
      stop(sprintf("Layer %d: format_strings[['%s']] must be an f_str object",
                   layer_index, nm), call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Validate data compatibility at build time
#'
#' Checks that the columns referenced in the spec actually exist in the data.
#' Called after data conversion to data.table.
#'
#' @param spec A tplyr_spec object
#' @param dt A data.table
#' @return Invisible TRUE if valid
#' @keywords internal
validate_build_data <- function(spec, dt) {
  dt_names <- names(dt)

  # Check that cols exist in data
  for (col in spec$cols) {
    if (!col %in% dt_names) {
      stop(sprintf(
        "Column variable '%s' not found in data. Available columns: %s",
        col, paste(utils::head(dt_names, 20), collapse = ", ")),
        call. = FALSE)
    }
  }

  # Check each layer's target_var and by vars
  for (i in seq_along(spec$layers)) {
    layer <- spec$layers[[i]]

    # For shift layers, check both row and column target vars
    if (inherits(layer, "tplyr_shift_layer")) {
      for (tv in layer$target_var) {
        if (!tv %in% dt_names) {
          stop(sprintf("Layer %d: target variable '%s' not found in data",
                       i, tv), call. = FALSE)
        }
      }
    } else {
      for (tv in layer$target_var) {
        if (!tv %in% dt_names) {
          stop(sprintf("Layer %d: target variable '%s' not found in data",
                       i, tv), call. = FALSE)
        }
      }
    }

    # Check by data vars (not labels)
    if (!is.null(layer$by)) {
      by_info <- classify_by(layer$by, dt_names)
      for (bv in by_info$data_vars) {
        if (!bv %in% dt_names) {
          stop(sprintf("Layer %d: by variable '%s' not found in data",
                       i, bv), call. = FALSE)
        }
      }
    }

    # Warn about unknown stat names in format strings
    validate_layer_stats(layer, i)
  }

  invisible(TRUE)
}

#' Validate that format string vars are valid stats for the layer type
#'
#' Issues warnings (not errors) for unrecognized statistic names, since custom
#' summaries can add arbitrary stat names.
#'
#' @param layer A tplyr_layer object
#' @param index Integer layer index
#' @return Invisible TRUE
#' @keywords internal
validate_layer_stats <- function(layer, index) {
  count_stats <- c("n", "pct", "total", "distinct_n", "distinct_pct",
                   "distinct_total")
  desc_stats <- c("n", "mean", "sd", "median", "var", "min", "max",
                  "iqr", "q1", "q3", "missing", "total", "pct")

  if (is.null(layer$settings$format_strings)) return(invisible(TRUE))

  for (nm in names(layer$settings$format_strings)) {
    fmt <- layer$settings$format_strings[[nm]]

    if (inherits(layer, "tplyr_count_layer") ||
        inherits(layer, "tplyr_shift_layer")) {
      valid_stats <- count_stats
    } else if (inherits(layer, "tplyr_desc_layer")) {
      # Desc layers may have custom summaries adding arbitrary stat names
      custom_names <- c(
        names(layer$settings$custom_summaries),
        names(getOption("tplyr2.custom_summaries", list()))
      )
      valid_stats <- c(desc_stats, custom_names)
    } else {
      # Analyze layers or unknown — skip validation
      return(invisible(TRUE))
    }

    for (v in fmt$vars) {
      if (!v %in% valid_stats) {
        warning(sprintf(
          "Layer %d: format string '%s' references variable '%s' which is not a recognized statistic for %s layers",
          index, nm, v, layer$layer_type),
          call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}
