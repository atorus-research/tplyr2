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
  layer_results <- Filter(Negate(is.null), layer_results)
  if (length(layer_results) == 0) {
    return(as.data.frame(data.table::data.table()))
  }

  result <- harmonize_and_bind(layer_results)

  all_ord <- str_subset(names(result), "^ord")
  other_ord <- sort(setdiff(all_ord, "ordindx"))
  ord_cols <- c("ordindx", other_ord)
  data.table::setorderv(result, ord_cols)
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

  if (inherits(layer, "tplyr_count_layer") ||
      inherits(layer, "tplyr_shift_layer")) {
    # Apply count format
    fmt <- get_count_format(settings)
    fmt_args <- map(fmt$vars, function(v) {
      if (v %in% names(wide_stats)) wide_stats[[v]] else NA_real_
    })
    wide_stats[, formatted := do.call(apply_formats, c(list(fmt), fmt_args))]

    # Determine target variable for row labels
    if (inherits(layer, "tplyr_shift_layer")) {
      tv <- layer$target_var["row"]
    } else {
      tv <- layer$target_var[1]
    }

    # Build row labels
    row_label_cols <- character(0)
    col_idx <- 1L

    for (lbl in by_labels) {
      col_name <- str_c("rowlabel", col_idx)
      wide_stats[, (col_name) := lbl]
      row_label_cols <- c(row_label_cols, col_name)
      col_idx <- col_idx + 1L
    }
    for (bv in by_data_vars) {
      col_name <- str_c("rowlabel", col_idx)
      wide_stats[, (col_name) := as.character(get(bv))]
      row_label_cols <- c(row_label_cols, col_name)
      col_idx <- col_idx + 1L
    }
    col_name <- paste0("rowlabel", col_idx)
    if (tv %in% names(wide_stats)) {
      wide_stats[, (col_name) := as.character(get(tv))]
    }
    row_label_cols <- c(row_label_cols, col_name)

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
    wide_stats <- create_desc_rows_from_ard(wide_stats, fmt_list, desc_group)

    # Build row labels
    row_label_cols <- character(0)
    col_idx <- 1L

    for (lbl in by_labels) {
      col_name <- str_c("rowlabel", col_idx)
      wide_stats[, (col_name) := lbl]
      row_label_cols <- c(row_label_cols, col_name)
      col_idx <- col_idx + 1L
    }
    for (bv in by_data_vars) {
      col_name <- str_c("rowlabel", col_idx)
      wide_stats[, (col_name) := as.character(get(bv))]
      row_label_cols <- c(row_label_cols, col_name)
      col_idx <- col_idx + 1L
    }
    col_name <- paste0("rowlabel", col_idx)
    wide_stats[, (col_name) := as.character(row_label)]
    row_label_cols <- c(row_label_cols, col_name)

    cast_cols <- cols
  } else {
    return(NULL)
  }

  cast_to_wide(wide_stats, row_label_cols, cast_cols, layer_index)
}

#' Create formatted rows for desc layer from pivoted ARD stats
#' @keywords internal
create_desc_rows_from_ard <- function(wide_stats, fmt_list, desc_group) {
  if (length(desc_group) > 0) {
    groups_unique <- unique(wide_stats[, desc_group, with = FALSE])
  } else {
    groups_unique <- data.table::data.table(.dummy = 1L)
  }

  output_rows <- vector("list", nrow(groups_unique) * length(fmt_list))
  idx <- 1L

  for (g in seq_len(nrow(groups_unique))) {
    if (length(desc_group) > 0) {
      mask <- rep(TRUE, nrow(wide_stats))
      for (gv in desc_group) {
        mask <- mask & wide_stats[[gv]] == groups_unique[[gv]][g]
      }
      stat_row <- wide_stats[mask][1]
    } else {
      stat_row <- wide_stats[1]
    }

    for (fname in names(fmt_list)) {
      fmt <- fmt_list[[fname]]
      fmt_args <- map(fmt$vars, function(v) {
        if (v %in% names(stat_row)) stat_row[[v]] else NA_real_
      })
      formatted_val <- do.call(apply_formats, c(list(fmt), fmt_args))

      row_dt <- data.table::data.table(
        row_label = fname,
        formatted = formatted_val
      )

      if (length(desc_group) > 0) {
        for (gv in desc_group) {
          row_dt[[gv]] <- groups_unique[[gv]][g]
        }
      }

      output_rows[[idx]] <- row_dt
      idx <- idx + 1L
    }
  }

  data.table::rbindlist(output_rows, use.names = TRUE, fill = TRUE)
}
