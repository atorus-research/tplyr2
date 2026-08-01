#' Convert tplyr_build output to Analysis Results Data (ARD) format
#'
#' Transforms the numeric data attached to a \code{tplyr_build()} result into
#' a long-format data frame with one row per statistic per group combination.
#' This is compatible with the CDISC Analysis Results Data standard.
#'
#' @param result A data.frame produced by \code{tplyr_build()}
#'
#' @return A data.frame in long format with columns:
#'   \describe{
#'     \item{analysis_id}{Integer layer index}
#'     \item{stat_name}{Character name of the statistic}
#'     \item{stat_value}{Numeric value of the statistic}
#'     \item{...}{Grouping columns from the original data}
#'   }
#' @export
tplyr_to_ard <- function(result) {
  nd <- attr(result, "numeric_data")
  if (is.null(nd) || length(nd) == 0) {
    stop("No numeric data available. tplyr_to_ard() requires a result from ",
         "tplyr_build()", call. = FALSE)
  }

  # Internal columns to exclude from ARD
  internal_cols <- c("formatted", ".missing_sort", ".total_sort", ".sort_key",
                     ".nest_level", ".row_order", ".comp_idx", ".formatted",
                     ".sort_outer", ".sort_inner", ".dummy",
                     ".row_indices")

  ard_parts <- vector("list", length(nd))

  for (layer_name in names(nd)) {
    layer_data <- nd[[layer_name]]
    if (is.null(layer_data) || nrow(layer_data) == 0) next

    dt <- data.table::as.data.table(layer_data)
    all_cols <- names(dt)

    # Classify columns
    stat_cols <- character(0)
    group_cols <- character(0)

    for (col in all_cols) {
      # Skip internal columns
      if (col %in% internal_cols) next
      if (str_detect(col, "^\\.")) next
      if (str_detect(col, "^rowlabel")) next
      # stat_columns snapshots carry one formatted column per statistic
      if (str_detect(col, "^formatted_\\d+$")) next

      if (is.numeric(dt[[col]])) {
        stat_cols <- c(stat_cols, col)
      } else {
        group_cols <- c(group_cols, col)
      }
    }

    if (length(stat_cols) == 0) next

    # Coerce all stat columns to double to avoid melt type coercion warnings
    melt_dt <- dt[, c(group_cols, stat_cols), with = FALSE]
    for (sc in stat_cols) {
      if (!is.double(melt_dt[[sc]])) {
        data.table::set(melt_dt, j = sc, value = as.double(melt_dt[[sc]]))
      }
    }

    # Melt to long format: one row per stat per group
    melted <- data.table::melt(
      melt_dt,
      id.vars = group_cols,
      measure.vars = stat_cols,
      variable.name = "stat_name",
      value.name = "stat_value",
      variable.factor = FALSE
    )

    melted[, analysis_id := as.integer(layer_name)]

    ard_parts[[layer_name]] <- melted
  }

  ard <- data.table::rbindlist(ard_parts, use.names = TRUE, fill = TRUE)

  # Reorder columns: analysis_id first, then groups, then stat name/value
  col_order <- c("analysis_id",
                 setdiff(names(ard), c("analysis_id", "stat_name", "stat_value")),
                 "stat_name", "stat_value")
  data.table::setcolorder(ard, intersect(col_order, names(ard)))

  as.data.frame(ard)
}

#' Reconstruct a formatted table from ARD and a spec
#'
#' Takes Analysis Results Data (long format) and a \code{tplyr_spec}, then
#' applies the spec's formatting rules to produce a formatted output table.
#'
#' @param ard A data.frame in ARD format (as produced by \code{tplyr_to_ard()})
#' @param spec A \code{tplyr_spec} object defining the table structure
#'
#' @return A data.frame with the same structure as \code{tplyr_build()} output
#' @export
tplyr_from_ard <- function(ard, spec) {
  if (!inherits(spec, "tplyr_spec")) {
    stop("'spec' must be a tplyr_spec object", call. = FALSE)
  }

  cols <- spec$cols
  ard_dt <- data.table::as.data.table(ard)

  if (!"analysis_id" %in% names(ard_dt)) {
    stop("ARD must contain an 'analysis_id' column", call. = FALSE)
  }

  layer_results <- vector("list", length(spec$layers))

  for (i in seq_along(spec$layers)) {
    layer <- spec$layers[[i]]
    layer_ard <- ard_dt[analysis_id == i]
    if (nrow(layer_ard) == 0) next

    layer_results[[i]] <- reconstruct_layer_from_ard(
      layer_ard, layer, cols, i
    )
  }

  # Remove NULL entries
  layer_results <- discard(layer_results, is.null)
  if (length(layer_results) == 0) {
    return(as.data.frame(data.table::data.table()))
  }

  result <- harmonize_and_bind(layer_results)

  sort_by_ord_columns(result)
  rename_ord_columns(result)

  as.data.frame(result)
}

#' Reconstruct a single layer from ARD data
#' @keywords internal
reconstruct_layer_from_ard <- function(layer_ard, layer, cols, layer_index) {
  # Identify grouping columns (exclude ARD structural columns)
  group_cols <- setdiff(
    names(layer_ard),
    c("analysis_id", "stat_name", "stat_value")
  )

  # Pivot from long to wide: one column per stat name
  if (length(group_cols) > 0) {
    formula_str <- str_c(str_c(group_cols, collapse = " + "), " ~ stat_name")
  } else {
    layer_ard[, .row := 1L]
    formula_str <- ".row ~ stat_name"
    group_cols <- ".row"
  }

  wide_stats <- data.table::dcast(
    layer_ard,
    as.formula(formula_str),
    value.var = "stat_value"
  )

  # Remove dummy column
  if (".row" %in% names(wide_stats)) {
    wide_stats[, .row := NULL]
    group_cols <- setdiff(group_cols, ".row")
  }

  settings <- layer$settings
  by_info <- classify_by(layer$by, group_cols)
  by_data_vars <- by_info$data_vars
  by_labels <- by_info$labels

  stat_labels <- NULL

  if (inherits(layer, "tplyr_count_layer") ||
      inherits(layer, "tplyr_shift_layer")) {
    # Apply count format(s); stat_columns reconstructs the multi-column
    # layout by writing one formatted_<i> column per statistic (count only —
    # shift layers ignore stat_columns, matching tplyr_build)
    if (inherits(layer, "tplyr_count_layer")) {
      fmts <- get_count_formats(settings)
    } else {
      fmts <- list(get_count_format(settings))
    }
    stat_labels <- names(fmts)
    walk(seq_along(fmts), function(i) {
      fmt <- fmts[[i]]
      fmt_args <- map(fmt$vars, function(v) {
        if (v %in% names(wide_stats)) wide_stats[[v]] else NA_real_
      })
      col_name <- if (is.null(stat_labels)) "formatted" else str_c("formatted_", i)
      wide_stats[, (col_name) := do.call(apply_formats, c(list(fmt), fmt_args))]
    })

    # Determine target variable for row labels
    if (inherits(layer, "tplyr_shift_layer")) {
      tv <- layer$target_var["row"]
    } else {
      tv <- layer$target_var[1]
    }

    row_label_cols <- build_row_labels_long(wide_stats, by_labels, by_data_vars, tv)

    # For shift, include the shift column variable in cols for casting
    if (inherits(layer, "tplyr_shift_layer")) {
      cast_cols <- c(cols, layer$target_var["column"])
    } else {
      cast_cols <- cols
    }

  } else if (inherits(layer, "tplyr_desc_layer") ||
             inherits(layer, "tplyr_analyze_layer")) {
    # Desc/analyze: each format_string becomes a row
    fmt_list <- settings$format_strings
    if (is.null(fmt_list) || length(fmt_list) == 0) {
      fmt_list <- list("n" = f_str("xx", "n"))
    }

    desc_group <- intersect(c(cols, by_data_vars), names(wide_stats))
    wide_stats <- format_analyze_results(wide_stats, fmt_list, desc_group)

    row_label_cols <- build_row_labels_long(wide_stats, by_labels, by_data_vars,
                                            "row_label")

    cast_cols <- cols
  } else {
    return(NULL)
  }

  cast_to_wide(wide_stats, row_label_cols, cast_cols, layer_index,
               stat_labels = stat_labels)
}

