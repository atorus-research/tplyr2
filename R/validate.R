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
         str_c(class(spec), collapse = ", "), ")", call. = FALSE)
  }

  if (!is.null(spec$cols) && !is.character(spec$cols)) {
    stop("'cols' must be a character vector", call. = FALSE)
  }

  if (!is.list(spec$layers)) {
    stop("'layers' must be a list", call. = FALSE)
  }

  iwalk(spec$layers, function(layer, i) {
    if (!inherits(layer, "tplyr_layer")) {
      stop(str_glue("Layer {i} is not a tplyr_layer object (class: {str_c(class(layer), collapse = ', ')})"),
           call. = FALSE)
    }
    validate_layer(layer, i, cols = spec$cols)
  })

  validate_stat_columns_alignment(spec$layers)

  invisible(TRUE)
}

#' Validate stat_columns consistency across layers
#'
#' Layers using stat_columns emit one res column per statistic per column
#' group, while other layers emit one per column group. harmonize_and_bind()
#' aligns layers positionally by res column name, so mixing the two shapes
#' in one spec would silently place results under the wrong column labels.
#'
#' @param layers List of tplyr_layer objects
#' @return Invisible TRUE if valid
#' @keywords internal
validate_stat_columns_alignment <- function(layers) {
  sc_names <- map(layers, function(l) {
    if (inherits(l, "tplyr_count_layer")) names(l$settings$stat_columns) else NULL
  })
  has_sc <- map_lgl(sc_names, Negate(is.null))

  if (!any(has_sc) || length(layers) == 1) return(invisible(TRUE))

  all_count <- all(map_lgl(layers, inherits, "tplyr_count_layer"))
  if (!all_count || !all(has_sc) ||
      !all(map_lgl(sc_names, identical, sc_names[[1]]))) {
    stop("When any layer uses stat_columns, all layers in the spec must be ",
         "count layers using stat_columns with the same statistic names, so ",
         "that result columns align across layers. Apply stat_columns ",
         "consistently or build separate specs.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate a single layer
#'
#' @param layer A tplyr_layer object
#' @param index Integer layer index (for error messages)
#' @param cols Character vector of spec column variables (for cross-checks such
#'   as pairwise assoc_test); may be NULL when validating a layer in isolation.
#' @return Invisible TRUE if valid
#' @keywords internal
validate_layer <- function(layer, index, cols = NULL) {
  if (!is.character(layer$target_var) || length(layer$target_var) == 0) {
    stop(str_glue("Layer {index}: target_var must be a non-empty character vector"),
         call. = FALSE)
  }

  # Shift-specific: must be named with row and column
  if (inherits(layer, "tplyr_shift_layer")) {
    if (length(layer$target_var) != 2 ||
        is.null(names(layer$target_var)) ||
        !all(c("row", "column") %in% names(layer$target_var))) {
      stop(str_glue("Layer {index}: shift layer target_var must have names 'row' and 'column'"),
           call. = FALSE)
    }
  }

  # Analyze-specific: must have a function
  if (inherits(layer, "tplyr_analyze_layer")) {
    if (is.null(layer$analyze_fn) || !is.function(layer$analyze_fn)) {
      stop(str_glue("Layer {index}: analyze layer must have a valid analyze_fn"),
           call. = FALSE)
    }
  }

  # Validate format_strings if present
  if (!is.null(layer$settings$format_strings)) {
    validate_format_strings(layer$settings$format_strings, index)
  }

  # Validate stat_columns if present (count layers only; other layer types
  # silently ignore the setting, consistent with layer_settings() docs)
  if (!is.null(layer$settings$stat_columns) &&
      inherits(layer, "tplyr_count_layer")) {
    validate_stat_columns(layer$settings$stat_columns, index)

    if (!is.null(layer$settings$format_strings)) {
      warning(str_glue("Layer {index}: both stat_columns and format_strings are set; stat_columns takes precedence for count layers"),
              call. = FALSE)
    }
  }

  # Validate percent-threshold and zero-count display settings (count layers)
  pct_lt <- layer$settings$pct_lt
  if (!is.null(pct_lt) && (!is.numeric(pct_lt) || length(pct_lt) != 1 || pct_lt <= 0)) {
    stop(str_glue("Layer {index}: pct_lt must be a single positive number"),
         call. = FALSE)
  }
  pct_gt <- layer$settings$pct_gt
  if (!is.null(pct_gt) && (!is.numeric(pct_gt) || length(pct_gt) != 1 || pct_gt >= 100)) {
    stop(str_glue("Layer {index}: pct_gt must be a single number below 100"),
         call. = FALSE)
  }
  zcd <- layer$settings$zero_count_display
  if (!is.null(zcd) && (!is.character(zcd) || length(zcd) != 1 ||
                        !zcd %in% c("full", "count_only", "blank"))) {
    stop(str_glue("Layer {index}: zero_count_display must be one of ",
                  "\"full\", \"count_only\", or \"blank\""),
         call. = FALSE)
  }

  if (!is.null(layer$settings$assoc_test) &&
      !inherits(layer$settings$assoc_test, "tplyr_assoc_test")) {
    stop(str_glue("Layer {index}: assoc_test must be an assoc_test() object"),
         call. = FALSE)
  }

  # Validate single-proportion CI settings (count layers)
  ci_method <- layer$settings$ci_method
  ci_methods <- c("clopper_pearson", "wilson", "wald", "agresti_coull",
                  "jeffreys")
  if (!is.null(ci_method) &&
      (!is.character(ci_method) || length(ci_method) != 1 ||
       !ci_method %in% ci_methods)) {
    stop(str_glue("Layer {index}: ci_method must be one of ",
                  "{str_c(ci_methods, collapse = ', ')}"),
         call. = FALSE)
  }
  ci_level <- layer$settings$ci_level
  if (!is.null(ci_level) &&
      (!is.numeric(ci_level) || length(ci_level) != 1 ||
       is.na(ci_level) || ci_level <= 0 || ci_level >= 1)) {
    stop(str_glue("Layer {index}: ci_level must be a single number in (0, 1)"),
         call. = FALSE)
  }

  # Pairwise assoc_test cross-checks: needs a column variable and, when a
  # reference is supplied explicitly, it should differ from the comparisons.
  at <- layer$settings$assoc_test
  if (inherits(at, "tplyr_assoc_test") && isTRUE(at$pairwise)) {
    if (!is.null(cols) && length(cols) == 0) {
      stop(str_glue("Layer {index}: pairwise assoc_test (comparisons = ...) ",
                    "requires at least one column variable (cols)"),
           call. = FALSE)
    }
    if (!is.null(at$reference) && at$reference %in% at$comparisons) {
      stop(str_glue("Layer {index}: assoc_test reference ",
                    "(\"{at$reference}\") must not also appear in comparisons"),
           call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Validate stat_columns in layer settings
#'
#' @param stat_cols A named list expected to contain f_str objects
#' @param layer_index Integer layer index (for error messages)
#' @return Invisible TRUE if valid
#' @keywords internal
validate_stat_columns <- function(stat_cols, layer_index) {
  if (!is.list(stat_cols) || length(stat_cols) == 0) {
    stop(str_glue("Layer {layer_index}: stat_columns must be a non-empty named list of f_str objects"),
         call. = FALSE)
  }

  nms <- names(stat_cols)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop(str_glue("Layer {layer_index}: every stat_columns element must be named; names become the column sub-labels"),
         call. = FALSE)
  }
  if (anyDuplicated(nms) > 0) {
    stop(str_glue("Layer {layer_index}: stat_columns names must be unique"),
         call. = FALSE)
  }

  walk(nms, function(nm) {
    # ' | ' and '(N=' are load-bearing in the column label grammar parsed by
    # build_col_labels() and cell metadata; names containing them would
    # corrupt column identity downstream
    if (str_detect(nm, fixed(" | "))) {
      stop(str_glue("Layer {layer_index}: stat_columns name '{nm}' may not contain ' | ', which is reserved as the column label separator"),
           call. = FALSE)
    }
    if (str_detect(nm, fixed("(N="))) {
      stop(str_glue("Layer {layer_index}: stat_columns name '{nm}' may not contain '(N=', which is reserved for header N labels"),
           call. = FALSE)
    }
    if (!inherits(stat_cols[[nm]], "tplyr_f_str")) {
      stop(str_glue("Layer {layer_index}: stat_columns[['{nm}']] must be an f_str object"),
           call. = FALSE)
    }
  })

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
    stop(str_glue("Layer {layer_index}: format_strings must be a named list of f_str objects"),
         call. = FALSE)
  }

  walk(names(fmt_list), function(nm) {
    if (!inherits(fmt_list[[nm]], "tplyr_f_str")) {
      stop(str_glue("Layer {layer_index}: format_strings[['{nm}']] must be an f_str object"),
           call. = FALSE)
    }
  })

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
  walk(spec$cols, function(col) {
    if (!col %in% dt_names) {
      stop(str_glue("Column variable '{col}' not found in data. Available columns: {str_c(utils::head(dt_names, 20), collapse = ', ')}"),
           call. = FALSE)
    }
  })

  # Check each layer's target_var and by vars
  iwalk(spec$layers, function(layer, i) {
    # Check target vars exist in data
    walk(layer$target_var, function(tv) {
      if (!tv %in% dt_names) {
        stop(str_glue("Layer {i}: target variable '{tv}' not found in data"),
             call. = FALSE)
      }
    })

    # Check by data vars (not labels)
    if (!is.null(layer$by)) {
      by_info <- classify_by(layer$by, dt_names)
      walk(by_info$data_vars, function(bv) {
        if (!bv %in% dt_names) {
          stop(str_glue("Layer {i}: by variable '{bv}' not found in data"),
               call. = FALSE)
        }
      })
    }

    # Warn about unknown stat names in format strings
    validate_layer_stats(layer, i)
  })

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
                   "distinct_total", "ci_lower", "ci_upper",
                   "distinct_ci_lower", "distinct_ci_upper")
  desc_stats <- c("n", "n_records", "mean", "sd", "median", "var", "min", "max",
                  "iqr", "q1", "q3", "missing", "total", "pct")

  fmt_lists <- c(layer$settings$format_strings, layer$settings$stat_columns)
  if (is.null(fmt_lists) || length(fmt_lists) == 0) return(invisible(TRUE))

  walk(names(fmt_lists), function(nm) {
    fmt <- fmt_lists[[nm]]

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

    walk(fmt$vars, function(v) {
      if (!v %in% valid_stats) {
        warning(str_glue("Layer {index}: format string '{nm}' references variable '{v}' which is not a recognized statistic for {layer$layer_type} layers"),
                call. = FALSE)
      }
    })
  })

  invisible(TRUE)
}
