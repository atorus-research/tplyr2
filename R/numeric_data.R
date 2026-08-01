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
#' Filters the raw numeric data for a specific layer and statistic. Use
#' \code{\link{tplyr_numeric_data}()} to get every statistic for the layer.
#'
#' @param result A data.frame produced by \code{tplyr_build()}
#' @param layer Integer layer index (1-based)
#' @param statistic Character string naming the statistic column to extract
#'   (e.g., "n", "pct", "mean", "sd")
#'
#' @return A data.frame with the layer's grouping columns and the requested
#'   statistic. Returns NULL if the layer has no numeric data or does not
#'   compute the statistic.
#'
#' @examples
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(group_desc("AGE"))
#' )
#' built <- tplyr_build(spec, tplyr_adsl)
#' tplyr_stats_data(built, 1, "mean")
#'
#' @export
tplyr_stats_data <- function(result, layer, statistic) {
  nd <- tplyr_numeric_data(result, layer)
  if (is.null(nd)) return(NULL)
  if (!statistic %in% names(nd)) return(NULL)

  group_cols <- setdiff(numeric_data_group_cols(nd), statistic)
  nd[, c(group_cols, statistic), drop = FALSE]
}

#' Grouping columns of a numeric-data snapshot
#'
#' Layer builders tag each snapshot with the columns that identify a row (see
#' \code{tag_numeric_group_cols()}). Snapshots from an older build lack the
#' attribute, so fall back to treating the non-numeric columns as grouping.
#'
#' @param nd A numeric-data snapshot data.frame
#' @return Character vector of grouping column names
#' @keywords internal
numeric_data_group_cols <- function(nd) {
  tagged <- attr(nd, "group_cols")
  if (!is.null(tagged)) return(intersect(tagged, names(nd)))
  names(nd)[!map_lgl(nd, is.numeric)]
}

#' Tag a numeric-data snapshot with its grouping columns
#'
#' Records which columns identify a row rather than hold a statistic, so
#' \code{tplyr_stats_data()} can subset to grouping columns plus one statistic
#' without guessing from column types (a grouping variable can be numeric).
#'
#' @param snapshot data.table snapshot, modified by reference
#' @param group_cols Character vector of candidate grouping column names
#' @return \code{snapshot}, invisibly
#' @keywords internal
tag_numeric_group_cols <- function(snapshot, group_cols) {
  data.table::setattr(snapshot, "group_cols",
                      intersect(group_cols, names(snapshot)))
  invisible(snapshot)
}
