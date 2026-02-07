#' Process a shift layer
#'
#' Builds a cross-tabulation (row variable × column variable) within each
#' treatment arm. The shift column variable becomes an additional column
#' dimension, producing result columns like "Placebo_H", "Placebo_N", etc.
#'
#' @param dt data.table with the (filtered) input data
#' @param layer A tplyr_shift_layer object
#' @param cols Character vector of column variable names from the spec
#' @param layer_index Integer index of this layer
#'
#' @return A data.table with rowlabel*, res*, and ord* columns
#' @keywords internal
build_shift_layer <- function(dt, layer, cols, layer_index, col_n = NULL, pop_dt = NULL) {
  target_var <- layer$target_var
  by <- layer$by
  settings <- layer$settings

  # Validate target_var
  if (length(target_var) != 2 || is.null(names(target_var)) ||
      !all(c("row", "column") %in% names(target_var))) {
    stop("group_shift() target_var must be a named character vector ",
         "with 'row' and 'column' elements, e.g. c(row = \"BNRIND\", column = \"ANRIND\")")
  }

  row_var <- target_var[["row"]]
  col_var <- target_var[["column"]]

  # Apply layer-level where
  if (!is.null(layer$where) && !identical(layer$where, TRUE)) {
    dt <- dt[eval(layer$where)]
  }

  # Separate by into data variables and labels
  by_info <- classify_by(by, names(dt))
  by_data_vars <- by_info$data_vars
  by_labels <- by_info$labels

  # The shift column variable acts as an additional cols dimension
  all_cols <- c(cols, col_var)

  # Extract settings
  distinct_by  <- settings$distinct_by
  denoms_by    <- settings$denoms_by
  denom_where  <- settings$denom_where
  denom_ignore <- settings$denom_ignore

  # Group variables: all_cols + by_data_vars + row_var
  group_vars <- c(all_cols, by_data_vars, row_var)

  # --- Count rows ---
  counts <- dt[, list(n = .N), by = group_vars]

  # --- Distinct counting ---
  if (!is.null(distinct_by)) {
    distinct_counts <- dt[, list(distinct_n = uniqueN(get(distinct_by))), by = group_vars]
    counts <- merge(counts, distinct_counts, by = group_vars, all.x = TRUE)
  }

  # --- Prepare denominator dataset ---
  denom_dt <- data.table::copy(dt)
  if (!is.null(denom_where) && !identical(denom_where, TRUE)) {
    denom_dt <- denom_dt[eval(denom_where)]
  }
  if (!is.null(denom_ignore)) {
    denom_dt <- denom_dt[!get(row_var) %in% denom_ignore]
  }

  # --- Compute denominators ---
  # Default denom_group for shift: use spec cols only (not the shift col_var)
  denom_group <- if (!is.null(denoms_by)) denoms_by else {
    if (length(cols) > 0) cols else character(0)
  }

  if (length(denom_group) > 0) {
    denoms <- denom_dt[, list(total = .N), by = denom_group]
    counts <- merge(counts, denoms, by = intersect(denom_group, names(counts)), all.x = TRUE)
  } else {
    counts[, total := nrow(denom_dt)]
  }
  counts[, pct := ifelse(total > 0, n / total * 100, 0)]

  # --- Distinct denominators ---
  if (!is.null(distinct_by)) {
    if (length(denom_group) > 0) {
      distinct_denoms <- denom_dt[, list(distinct_total = uniqueN(get(distinct_by))), by = denom_group]
      counts <- merge(counts, distinct_denoms, by = intersect(denom_group, names(counts)), all.x = TRUE)
    } else {
      counts[, distinct_total := uniqueN(denom_dt[[distinct_by]])]
    }
    counts[, distinct_pct := ifelse(distinct_total > 0, distinct_n / distinct_total * 100, 0)]
  }

  # --- Data completion ---
  counts <- complete_shift_counts(counts, dt, all_cols, by_data_vars, row_var, denom_group)

  # --- Capture numeric data before formatting ---
  numeric_snapshot <- data.table::copy(counts)

  # --- Format ---
  fmt <- get_count_format(settings)
  fmt_args <- lapply(fmt$vars, function(v) counts[[v]])
  counts[, formatted := do.call(apply_formats, c(list(fmt), fmt_args))]

  # --- Build row labels ---
  row_label_cols <- build_shift_row_labels(counts, by_labels, by_data_vars, row_var)

  # --- Preserve factor-level ordering for row variable ---
  # dcast sorts LHS alphabetically; add a numeric sort key to the LHS
  # so the factor level order is preserved through dcast
  if (is.factor(dt[[row_var]])) {
    row_levels <- levels(dt[[row_var]])
    counts[, .row_order := match(get(row_var), row_levels)]
  } else {
    counts[, .row_order := data.table::frank(get(row_var), ties.method = "dense")]
  }
  # Include by_data_vars ordering too
  for (bv in by_data_vars) {
    if (is.factor(dt[[bv]])) {
      counts[, paste0(".by_order_", bv) := match(get(bv), levels(dt[[bv]]))]
    }
  }

  # --- Cast to wide format ---
  # Include .row_order in row labels for dcast, then remove after
  order_cols <- grep("^\\.row_order$|^\\.by_order_", names(counts), value = TRUE)
  row_label_cols_with_order <- c(order_cols, row_label_cols)
  wide <- cast_to_wide(counts, row_label_cols_with_order, all_cols, layer_index, col_n = col_n)

  # Remove order columns
  for (col in order_cols) {
    if (col %in% names(wide)) {
      wide[, (col) := NULL]
    }
  }

  # Attach numeric data snapshot
  data.table::setattr(wide, "numeric_data", as.data.frame(numeric_snapshot))

  wide
}

#' Complete shift count data
#'
#' Ensures all row_var × col_var × cols combinations exist, filling missing
#' combinations with zero counts.
#'
#' @keywords internal
complete_shift_counts <- function(counts, dt, all_cols, by_data_vars, row_var,
                                   denom_group = NULL) {
  grid_vars <- list()

  for (col in all_cols) {
    vals <- sort(unique(dt[[col]]))
    if (is.factor(dt[[col]])) vals <- levels(dt[[col]])
    grid_vars[[col]] <- vals
  }

  for (bv in by_data_vars) {
    vals <- sort(unique(dt[[bv]]))
    if (is.factor(dt[[bv]])) vals <- levels(dt[[bv]])
    grid_vars[[bv]] <- vals
  }

  vals <- sort(unique(dt[[row_var]]))
  if (is.factor(dt[[row_var]])) vals <- levels(dt[[row_var]])
  grid_vars[[row_var]] <- vals

  if (length(grid_vars) == 0) return(counts)

  grid <- do.call(data.table::CJ, grid_vars)
  result <- merge(grid, counts, by = names(grid_vars), all.x = TRUE)

  # Fill NAs with 0
  for (v in c("n", "pct", "distinct_n", "distinct_pct")) {
    if (v %in% names(result)) {
      data.table::set(result, which(is.na(result[[v]])), v, 0)
    }
  }

  # Fill total and distinct_total from existing data
  denom_vars <- if (!is.null(denom_group) && length(denom_group) > 0) {
    intersect(denom_group, names(result))
  } else {
    character(0)
  }
  if ("total" %in% names(result) && length(denom_vars) > 0) {
    denom_dt <- unique(counts[!is.na(total), .SD, .SDcols = c(denom_vars, "total")])
    result[, total := NULL]
    result <- merge(result, denom_dt, by = denom_vars, all.x = TRUE)
  }
  if ("distinct_total" %in% names(result) && length(denom_vars) > 0) {
    dist_denom_dt <- unique(counts[!is.na(distinct_total), .SD, .SDcols = c(denom_vars, "distinct_total")])
    result[, distinct_total := NULL]
    result <- merge(result, dist_denom_dt, by = denom_vars, all.x = TRUE)
  }

  result
}

#' Build row labels for shift layer
#' @keywords internal
build_shift_row_labels <- function(counts, by_labels, by_data_vars, row_var) {
  label_cols <- list()
  col_idx <- 1L

  for (lbl in by_labels) {
    col_name <- paste0("rowlabel", col_idx)
    counts[, (col_name) := lbl]
    label_cols[[col_name]] <- col_name
    col_idx <- col_idx + 1L
  }

  for (bv in by_data_vars) {
    col_name <- paste0("rowlabel", col_idx)
    counts[, (col_name) := as.character(get(bv))]
    label_cols[[col_name]] <- col_name
    col_idx <- col_idx + 1L
  }

  col_name <- paste0("rowlabel", col_idx)
  counts[, (col_name) := as.character(get(row_var))]
  label_cols[[col_name]] <- col_name

  names(label_cols)
}
