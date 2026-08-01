#' Create a text label for use in by parameters
#'
#' Explicitly marks a string as a text label (not a data variable name).
#' Useful when a label string might coincidentally match a column name.
#'
#' @param x Character string to use as a label
#'
#' @return A tplyr_label object
#'
#' @examples
#' # A `by` string that matches no column is already treated as a label, but
#' # label() is explicit -- and necessary when the text matches a column name.
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(
#'     group_count("AGEGR1", by = label("Age Group (y)"))
#'   )
#' )
#' head(tplyr_build(spec, tplyr_adsl))
#'
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
#'
#' @examples
#' # Counts of a categorical variable within each column group
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(group_count("AGEGR1"))
#' )
#' tplyr_build(spec, tplyr_adsl)
#'
#' # Distinct subject counts with a total row, filtered to serious events
#' ae <- tplyr_spec(
#'   cols = "TRTA",
#'   layers = tplyr_layers(
#'     group_count("AEBODSYS",
#'       where = AESER == "Y",
#'       settings = layer_settings(distinct_by = "USUBJID", total_row = TRUE))
#'   )
#' )
#' head(tplyr_build(ae, tplyr_adae))
#'
#' # Two target variables nest: preferred term within body system
#' nested <- tplyr_spec(
#'   cols = "TRTA",
#'   layers = tplyr_layers(group_count(c("AEBODSYS", "AEDECOD")))
#' )
#' head(tplyr_build(nested, tplyr_adae))
#'
#' @seealso [layer_settings()] for denominators, sorting, and special rows.
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
#'
#' @examples
#' # Default summary: n, Mean (SD), Median, Q1/Q3, Min/Max, Missing
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(group_desc("AGE"))
#' )
#' tplyr_build(spec, tplyr_adsl)
#'
#' # Choose the statistics and their formats
#' custom <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(
#'     group_desc("AGE", settings = layer_settings(
#'       format_strings = list(
#'         "n"         = f_str("xx", "n"),
#'         "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
#'       )))
#'   )
#' )
#' tplyr_build(custom, tplyr_adsl)
#'
#' # Several target variables in one layer, grouped by visit
#' multi <- tplyr_spec(
#'   cols = "TRTA",
#'   layers = tplyr_layers(group_desc(c("AVAL", "CHG"), by = "AVISIT"))
#' )
#' head(tplyr_build(multi, tplyr_adlb))
#'
#' @seealso [layer_settings()] for auto-precision and custom summaries.
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
#'
#' @examples
#' # Baseline (rows) against post-baseline (columns) reference ranges
#' spec <- tplyr_spec(
#'   cols = "TRTA",
#'   layers = tplyr_layers(
#'     group_shift(c(row = "BNRIND", column = "ANRIND"))
#'   )
#' )
#' head(tplyr_build(spec, tplyr_adlb))
#'
#' # Percentages relative to each baseline (column) group rather than the arm
#' by_col <- tplyr_spec(
#'   cols = "TRTA",
#'   layers = tplyr_layers(
#'     group_shift(c(row = "BNRIND", column = "ANRIND"),
#'                 settings = layer_settings(shift_denom = "column"))
#'   )
#' )
#' head(tplyr_build(by_col, tplyr_adlb))
#'
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
#' Note that \code{analyze_fn} is called once per \code{cols} x \code{by}
#' combination, so it only ever sees a single treatment column at a time — it
#' cannot compute a statistic \emph{across} the treatment columns. For an
#' omnibus association test that spans the columns (e.g. Fisher's exact or CMH
#' on a count/shift layer), see \code{\link{assoc_test}}.
#'
#' @return A tplyr_analyze_layer object
#'
#' @examples
#' # format_strings mode: the function returns one row of named numbers, and
#' # each format string becomes an output row.
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(
#'     group_analyze("AGE",
#'       analyze_fn = function(.data, .target_var) {
#'         v <- .data[[.target_var]]
#'         data.frame(gmean = exp(mean(log(v))), rng = diff(range(v)))
#'       },
#'       settings = layer_settings(format_strings = list(
#'         "Geometric mean" = f_str("xx.xx", "gmean"),
#'         "Range"          = f_str("xx", "rng")
#'       )))
#'   )
#' )
#' tplyr_build(spec, tplyr_adsl)
#'
#' # Pre-formatted mode: the function supplies row_label and formatted itself.
#' pre <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(
#'     group_analyze("AGE",
#'       analyze_fn = function(.data, .target_var) {
#'         v <- .data[[.target_var]]
#'         data.frame(
#'           row_label = "Median [IQR]",
#'           formatted = sprintf("%.1f [%.1f]", median(v), IQR(v))
#'         )
#'       })
#'   )
#' )
#' tplyr_build(pre, tplyr_adsl)
#'
#' @seealso \code{\link{assoc_test}} for cross-column association tests.
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
#'
#' @examples
#' # Layers stack in the order given, and may mix types freely
#' layers <- tplyr_layers(
#'   group_desc("AGE"),
#'   group_count("SEX"),
#'   group_count("AGEGR1")
#' )
#' length(layers)
#'
#' spec <- tplyr_spec(cols = "TRT01P", layers = layers)
#' tplyr_build(spec, tplyr_adsl)
#'
#' @export
tplyr_layers <- function(...) {
  layers <- list(...)
  if (!all(map_lgl(layers, inherits, "tplyr_layer"))) {
    stop("All arguments to tplyr_layers() must be tplyr_layer objects")
  }
  layers
}

#' Check if an object is a tplyr_layer
#' @param x Object to check
#' @return Logical
#'
#' @examples
#' is_tplyr_layer(group_count("SEX"))
#' is_tplyr_layer(group_desc("AGE"))
#' is_tplyr_layer("SEX")
#'
#' @export
is_tplyr_layer <- function(x) {
  inherits(x, "tplyr_layer")
}
