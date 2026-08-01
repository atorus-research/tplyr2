#' Get or set tplyr2 package options
#'
#' View and modify tplyr2-specific options. When called with no arguments,
#' returns all current tplyr2 options with their defaults. When called with
#' named arguments, sets those options.
#'
#' @param ... Named arguments to set (e.g., \code{IBMRounding = TRUE}).
#'   Option names are automatically prefixed with \code{tplyr2.}.
#'
#' @return When called with no arguments, a named list of current option values.
#'   When called with arguments, invisibly returns the previous values.
#'
#' @details
#' Available options:
#' \describe{
#'   \item{tplyr2.IBMRounding}{Logical. Use round-half-away-from-zero instead
#'     of R's default banker's rounding. Default: \code{FALSE}.}
#'   \item{tplyr2.quantile_type}{Integer. Quantile algorithm type passed to
#'     \code{quantile()}. Default: \code{7}.}
#'   \item{tplyr2.precision_cap}{Named numeric vector \code{c(int=, dec=)}.
#'     Maximum int/dec widths for auto-precision. Default: \code{NULL}.}
#'   \item{tplyr2.custom_summaries}{Named list of expressions for global custom
#'     summary functions. Default: \code{NULL}.}
#'   \item{tplyr2.scipen}{Integer. scipen value used during \code{tplyr_build()}
#'     to prevent scientific notation. Default: \code{9999}.}
#' }
#'
#' An unrecognized option name is an error rather than a silent no-op, so a
#' misspelling cannot quietly leave the build on the default behavior.
#'
#' @examples
#' # Inspect the current values
#' tplyr2_options()
#'
#' # Setting returns the previous values, so the change can be undone
#' old <- tplyr2_options(IBMRounding = TRUE)
#' getOption("tplyr2.IBMRounding")
#' do.call(options, old)
#' getOption("tplyr2.IBMRounding")
#'
#' # IBM (half-away-from-zero) rounding vs R's banker's rounding
#' fmt <- f_str("xx", "n")
#' apply_formats(fmt, n = 2.5)
#' old <- tplyr2_options(IBMRounding = TRUE)
#' apply_formats(fmt, n = 2.5)
#' do.call(options, old)
#'
#' # A misspelled name errors instead of setting a dead option
#' try(tplyr2_options(IBMrounding = TRUE))
#'
#' @export
tplyr2_options <- function(...) {
  defaults <- list(
    tplyr2.IBMRounding     = FALSE,
    tplyr2.quantile_type   = 7L,
    tplyr2.precision_cap   = NULL,
    tplyr2.custom_summaries = NULL,
    tplyr2.scipen          = 9999L
  )

  args <- list(...)
  if (length(args) == 0) {
    # Return current values with defaults
    result <- map(names(defaults), function(nm) {
      getOption(nm, defaults[[nm]])
    })
    names(result) <- names(defaults)
    return(result)
  }

  # A misspelled option sets a key nothing reads — e.g. IBMrounding for
  # IBMRounding leaves the whole output package on banker's rounding with no
  # signal — so reject names that are not real options.
  supplied <- names(args) %||% rep("", length(args))
  valid <- str_replace(names(defaults), "^tplyr2\\.", "")
  unknown <- setdiff(supplied, valid)
  if (length(unknown) > 0) {
    stop("Unknown option", if (length(unknown) > 1) "s" else "", ": ",
         str_c(ifelse(unknown == "", "<unnamed>", unknown), collapse = ", "),
         "\nValid options: ", str_c(valid, collapse = ", "), call. = FALSE)
  }

  # Set options
  opt_names <- str_c("tplyr2.", names(args))
  old_vals <- map(opt_names, getOption)
  names(old_vals) <- opt_names

  new_opts <- setNames(args, opt_names)
  do.call(options, new_opts)

  invisible(old_vals)
}

#' Extract variable labels from a data.frame
#'
#' Returns a named character vector of variable labels. Labels are extracted
#' from the \code{"label"} attribute of each column (standard for Haven-imported
#' CDISC data).
#'
#' @param data A data.frame
#'
#' @return Named character vector where names are column names and values are
#'   labels. Columns without labels return \code{NA_character_}.
#'
#' @examples
#' # CDISC data imported via haven carries a "label" attribute per column
#' labs <- get_data_labels(tplyr_adsl)
#' head(labs)
#'
#' # Columns with no label attribute come back NA
#' get_data_labels(data.frame(a = 1, b = 2))
#'
#' @export
get_data_labels <- function(data) {
  if (!is.data.frame(data)) stop("data must be a data.frame")

  map_chr(data, function(col_data) {
    lbl <- attr(col_data, "label")
    if (is.null(lbl)) NA_character_ else as.character(lbl)
  })
}
