#' Retrieve raw numeric data from a tplyr_build result
#'
#' Returns the unformatted numeric data that was computed during the build
#' process, before formatting and pivoting to wide format.
#'
#' @param result A data.frame produced by \code{tplyr_build()}
#' @param layer Integer layer index (1-based), or NULL for all layers
#'
#' @return If \code{layer} is specified, a data.frame of raw statistics for that
#'   layer. If \code{layer} is NULL, a named list of data.frames keyed by layer
#'   index. Returns NULL if numeric data was not retained.
#' @export
tplyr_numeric_data <- function(result, layer = NULL) {
  nd <- attr(result, "numeric_data")
  if (is.null(nd)) return(NULL)

  if (is.null(layer)) {
    return(nd)
  }

  layer_name <- as.character(layer)
  if (layer_name %in% names(nd)) {
    nd[[layer_name]]
  } else {
    NULL
  }
}

#' Retrieve raw statistic values from a tplyr_build result
#'
#' Filters the raw numeric data for a specific layer and statistic.
#'
#' @param result A data.frame produced by \code{tplyr_build()}
#' @param layer Integer layer index (1-based)
#' @param statistic Character string naming the statistic column to extract
#'   (e.g., "n", "pct", "mean", "sd")
#'
#' @return A data.frame with the grouping columns and the requested statistic.
#'   Returns NULL if not available.
#' @export
tplyr_stats_data <- function(result, layer, statistic) {
  nd <- tplyr_numeric_data(result, layer)
  if (is.null(nd)) return(NULL)

  if (statistic %in% names(nd)) {
    nd
  } else {
    NULL
  }
}
