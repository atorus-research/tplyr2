#' Create a population data configuration
#'
#' Configuration object specifying how population data maps to the spec.
#' The actual population data.frame is provided at build time via
#' `tplyr_build(spec, data, pop_data = ...)`.
#'
#' @param cols Character vector of column variable names in the population data.
#'   If named, names are the spec column names and values are the pop_data column
#'   names (e.g., `c("TRTA" = "TRT01P")`). If unnamed, maps positionally to
#'   spec cols.
#' @param where Expression for filtering the population data (optional)
#'
#' @return A tplyr_pop_data object
#' @export
pop_data <- function(cols, where = NULL) {
  where_expr <- rlang::enexpr(where)

  structure(
    list(
      cols = cols,
      where = where_expr
    ),
    class = "tplyr_pop_data"
  )
}

#' Check if an object is a tplyr_pop_data
#'
#' @param x An object to check
#' @return Logical
#' @export
is_pop_data <- function(x) {
  inherits(x, "tplyr_pop_data")
}

#' @export
print.tplyr_pop_data <- function(x, ...) {
  cat("tplyr2 population data config\n")
  cat(str_glue("  Columns: {str_c(x$cols, collapse = ', ')}\n"))
  if (!is.null(x$where) && !identical(x$where, TRUE)) {
    cat(str_glue("  Where: {deparse(x$where)}\n"))
  }
  invisible(x)
}

#' Create a total group configuration
#'
#' Specifies that a synthetic "Total" column level should be added by
#' duplicating all rows with the specified column variable set to the label.
#'
#' @param col_var Character string naming the column variable to totalize
#' @param label Character string for the total group label (default: "Total")
#'
#' @return A tplyr_total_group object
#' @export
total_group <- function(col_var, label = "Total") {
  structure(
    list(
      col_var = col_var,
      label = label
    ),
    class = "tplyr_total_group"
  )
}

#' @export
print.tplyr_total_group <- function(x, ...) {
  cat(str_glue("tplyr2 total group: {x$col_var} = \"{x$label}\"\n"))
  invisible(x)
}

#' Create a custom column group configuration
#'
#' Combines existing column levels into a custom group. Rows matching any of
#' the source levels are duplicated with the column variable set to the group name.
#'
#' @param col_var Character string naming the column variable
#' @param ... Named arguments where names are group labels and values are
#'   character vectors of source levels to combine.
#'   Example: `"High Dose" = c("Dose 1", "Dose 2")`
#'
#' @return A tplyr_custom_group object
#' @export
custom_group <- function(col_var, ...) {
  groups <- list(...)
  structure(
    list(
      col_var = col_var,
      groups = groups
    ),
    class = "tplyr_custom_group"
  )
}

#' @export
print.tplyr_custom_group <- function(x, ...) {
  cat(str_glue("tplyr2 custom group on {x$col_var}\n"))
  walk(names(x$groups), function(nm) {
    cat(str_glue("  \"{nm}\" = [{str_c(x$groups[[nm]], collapse = ', ')}]\n"))
  })
  invisible(x)
}

#' Extract header N from a tplyr2 build result
#'
#' Returns the population-based header N values that were computed during
#' `tplyr_build()`. Only available when population data was provided.
#'
#' @param result A data.frame produced by `tplyr_build()`
#'
#' @return A data.frame with column variable levels and their N values,
#'   or NULL if no population data was used.
#' @export
tplyr_header_n <- function(result) {
  attr(result, "header_n")
}

#' Resolve population data column mapping
#'
#' Maps population data columns to spec columns. Handles named (explicit)
#' and unnamed (positional) mapping.
#'
#' @param pop_config A tplyr_pop_data object or NULL
#' @param spec_cols Character vector of spec-level column names
#'
#' @return Character vector of column names to use in the population data
#' @keywords internal
resolve_pop_cols <- function(pop_config, spec_cols) {
  if (is.null(pop_config)) return(spec_cols)

  pop_cols <- pop_config$cols
  if (is.null(pop_cols)) return(spec_cols)

  # Named mapping: names are spec cols, values are pop cols
  if (!is.null(names(pop_cols)) && any(names(pop_cols) != "")) {
    return(pop_cols)
  }

  # Unnamed: positional mapping (pop_cols should match spec_cols)
  pop_cols
}
