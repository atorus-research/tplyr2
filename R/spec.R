#' Create a tplyr2 table specification
#'
#' The spec is a pure configuration object describing what to compute.
#' No data processing occurs until `tplyr_build()` is called.
#'
#' @param cols Character vector of column variable names
#' @param where Expression for global data filter (optional)
#' @param pop_data A pop_data() object for population-based features (optional)
#' @param total_groups List of total_group() objects (optional)
#' @param custom_groups List of custom_group() objects (optional)
#' @param layers A list of layer objects from tplyr_layers()
#' @param settings Additional spec-level settings (optional)
#'
#' @return A tplyr_spec object
#' @export
tplyr_spec <- function(
    cols,
    where = NULL,
    pop_data = NULL,
    total_groups = NULL,
    custom_groups = NULL,
    layers = tplyr_layers(),
    settings = NULL
) {
  where_expr <- rlang::enexpr(where)

  structure(
    list(
      cols = cols,
      where = where_expr,
      pop_data = pop_data,
      total_groups = total_groups,
      custom_groups = custom_groups,
      layers = layers,
      settings = settings
    ),
    class = "tplyr_spec"
  )
}

#' Check if an object is a tplyr_spec
#'
#' @param x An object to check
#' @return Logical
#' @export
is_tplyr_spec <- function(x) {
  inherits(x, "tplyr_spec")
}

#' @export
print.tplyr_spec <- function(x, ...) {
  cat("tplyr2 table specification\n")
  cat(sprintf("  Column variables: %s\n", paste(x$cols, collapse = ", ")))
  if (!is.null(x$where) && !identical(x$where, TRUE)) {
    cat(sprintf("  Where: %s\n", deparse(x$where)))
  }
  cat(sprintf("  Layers: %d\n", length(x$layers)))
  for (i in seq_along(x$layers)) {
    layer <- x$layers[[i]]
    name <- layer$settings$name %||% paste0("Layer ", i)
    cat(sprintf("    [%d] %s: %s (%s)\n",
                i, layer$layer_type, paste(layer$target_var, collapse = " > "), name))
  }
  invisible(x)
}
