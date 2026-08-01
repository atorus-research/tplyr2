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
#'
#' @examples
#' # The AE data's TRTA maps to the subject-level TRT01P. Denominators and the
#' # header N then come from the population, not from the AE records.
#' spec <- tplyr_spec(
#'   cols = "TRTA",
#'   pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
#'   layers = tplyr_layers(
#'     group_count("AEBODSYS",
#'                 settings = layer_settings(distinct_by = "USUBJID"))
#'   )
#' )
#' built <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
#' head(built)
#'
#' # Header N reflects the 254 enrolled subjects, not the 200 AE records
#' tplyr_header_n(built)
#'
#' # Restrict the population to the safety set
#' saf <- pop_data(cols = c("TRTA" = "TRT01P"), where = SAFFL == "Y")
#' saf
#'
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
#'
#' @examples
#' is_pop_data(pop_data(cols = "TRT01P"))
#' is_pop_data("TRT01P")
#'
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
#'
#' @examples
#' # Adds a "Total" column spanning every arm, alongside the individual arms
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   total_groups = list(total_group("TRT01P")),
#'   layers = tplyr_layers(group_count("AGEGR1"))
#' )
#' tplyr_build(spec, tplyr_adsl)
#'
#' # Rename the total column
#' all_pts <- tplyr_spec(
#'   cols = "TRT01P",
#'   total_groups = list(total_group("TRT01P", label = "All Patients")),
#'   layers = tplyr_layers(group_count("SEX"))
#' )
#' tplyr_build(all_pts, tplyr_adsl)
#'
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
#'
#' @examples
#' # Pool the two dose arms into one "Xanomeline (All)" column, kept alongside
#' # the arms it is built from
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   custom_groups = list(custom_group(
#'     "TRT01P",
#'     "Xanomeline (All)" = c("Xanomeline High Dose", "Xanomeline Low Dose")
#'   )),
#'   layers = tplyr_layers(group_count("AGEGR1"))
#' )
#' tplyr_build(spec, tplyr_adsl)
#'
#' # Several groups at once
#' custom_group(
#'   "TRT01P",
#'   "Active"  = c("Xanomeline High Dose", "Xanomeline Low Dose"),
#'   "Control" = "Placebo"
#' )
#'
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
#'
#' @examples
#' spec <- tplyr_spec(
#'   cols = "TRTA",
#'   pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
#'   layers = tplyr_layers(group_count("AEBODSYS"))
#' )
#' built <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
#' tplyr_header_n(built)
#'
#' # NULL when the build had no population data to draw an N from
#' no_pop <- tplyr_build(
#'   tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("SEX"))),
#'   tplyr_adsl
#' )
#' tplyr_header_n(no_pop)
#'
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
