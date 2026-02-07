#' Create a text label for use in by parameters
#'
#' Explicitly marks a string as a text label (not a data variable name).
#' Useful when a label string might coincidentally match a column name.
#'
#' @param x Character string to use as a label
#'
#' @return A tplyr_label object
#' @export
label <- function(x) {
  structure(x, class = c("tplyr_label", "character"))
}

#' Check if an object is a tplyr_label
#' @param x Object to check
#' @return Logical
#' @keywords internal
is_label <- function(x) {
  inherits(x, "tplyr_label")
}

#' Create a count layer
#'
#' @param target_var Character string or vector naming the target variable(s).
#'   Multiple variables create nested/hierarchical counts.
#' @param by Character string or vector for row grouping. Strings that don't
#'   match column names are treated as text labels. Use `label()` for explicit
#'   disambiguation.
#' @param where Expression for filtering data for this layer
#' @param settings A layer_settings object
#'
#' @return A tplyr_count_layer object
#' @export
group_count <- function(target_var, by = NULL, where = NULL, settings = layer_settings()) {
  where_expr <- rlang::enexpr(where)

  structure(
    list(
      target_var = target_var,
      by = by,
      where = where_expr,
      settings = settings,
      layer_type = "count"
    ),
    class = c("tplyr_count_layer", "tplyr_layer")
  )
}

#' Create a descriptive statistics layer
#'
#' @param target_var Character string or vector naming the target variable(s)
#' @param by Character string or vector for row grouping
#' @param where Expression for filtering data for this layer
#' @param settings A layer_settings object
#'
#' @return A tplyr_desc_layer object
#' @export
group_desc <- function(target_var, by = NULL, where = NULL, settings = layer_settings()) {
  where_expr <- rlang::enexpr(where)

  structure(
    list(
      target_var = target_var,
      by = by,
      where = where_expr,
      settings = settings,
      layer_type = "desc"
    ),
    class = c("tplyr_desc_layer", "tplyr_layer")
  )
}

#' Create a shift layer
#'
#' @param target_var Named character vector with `row` and `column` elements
#' @param by Character string or vector for row grouping
#' @param where Expression for filtering data for this layer
#' @param settings A layer_settings object
#'
#' @return A tplyr_shift_layer object
#' @export
group_shift <- function(target_var, by = NULL, where = NULL, settings = layer_settings()) {
  # Validate target_var: must be named character vector with "row" and "column"
  if (!is.character(target_var) || length(target_var) != 2) {
    stop("group_shift() target_var must be a character vector of length 2")
  }
  if (is.null(names(target_var)) || !all(c("row", "column") %in% names(target_var))) {
    stop("group_shift() target_var must have names 'row' and 'column', ",
         "e.g. c(row = \"BNRIND\", column = \"ANRIND\")")
  }

  where_expr <- rlang::enexpr(where)

  structure(
    list(
      target_var = target_var,
      by = by,
      where = where_expr,
      settings = settings,
      layer_type = "shift"
    ),
    class = c("tplyr_shift_layer", "tplyr_layer")
  )
}

#' Create a custom analysis layer
#'
#' Allows a user-defined function to compute summary statistics. The function
#' receives a data subset and the target variable name for each group
#' combination, and returns a data.frame of results.
#'
#' @param target_var Character string naming the target variable(s)
#' @param by Character string or vector for row grouping
#' @param where Expression for filtering data for this layer
#' @param analyze_fn A function with signature \code{function(.data, .target_var)}
#'   that returns a data.frame. See Details.
#' @param settings A layer_settings object
#'
#' @details
#' The \code{analyze_fn} is called once per group combination (defined by
#' \code{cols} and \code{by} data variables). It receives:
#' \itemize{
#'   \item \code{.data}: A data.frame subset for the current group
#'   \item \code{.target_var}: Character string with the target variable name(s)
#' }
#'
#' If \code{format_strings} are provided in settings, \code{analyze_fn} should
#' return a single-row data.frame of named numeric values. Each format string
#' entry becomes one output row, with its name used as the row label.
#'
#' If no \code{format_strings} are provided, \code{analyze_fn} must return a
#' data.frame with \code{row_label} and \code{formatted} columns.
#'
#' @return A tplyr_analyze_layer object
#' @export
group_analyze <- function(target_var, by = NULL, where = NULL,
                          analyze_fn, settings = layer_settings()) {
  if (!is.function(analyze_fn)) {
    stop("'analyze_fn' must be a function", call. = FALSE)
  }

  where_expr <- rlang::enexpr(where)

  structure(
    list(
      target_var = target_var,
      by = by,
      where = where_expr,
      analyze_fn = analyze_fn,
      settings = settings,
      layer_type = "analyze"
    ),
    class = c("tplyr_analyze_layer", "tplyr_layer")
  )
}

#' Create a list of layers
#'
#' Wraps one or more layer objects into a validated list for use in tplyr_spec().
#'
#' @param ... Layer objects created by group_count(), group_desc(),
#'   group_shift(), or group_analyze()
#'
#' @return A list of tplyr_layer objects
#' @export
tplyr_layers <- function(...) {
  layers <- list(...)
  if (!all(vapply(layers, inherits, logical(1), "tplyr_layer"))) {
    stop("All arguments to tplyr_layers() must be tplyr_layer objects")
  }
  layers
}

#' Check if an object is a tplyr_layer
#' @param x Object to check
#' @return Logical
#' @export
is_tplyr_layer <- function(x) {
  inherits(x, "tplyr_layer")
}
