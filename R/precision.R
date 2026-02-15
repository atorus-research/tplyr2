#' Collect precision from data
#'
#' Scans a numeric variable to determine the maximum integer width and
#' maximum decimal precision present in the data, optionally grouped.
#'
#' @param dt data.table with the data
#' @param precision_on Character string naming the variable to scan
#' @param precision_by Character vector of grouping variables (can be empty)
#' @param precision_data Optional external data.frame with pre-computed precision
#' @param precision_cap Named numeric vector c(int=, dec=) for capping
#'
#' @return data.table with precision_by columns + "max_int" and "max_dec" columns
#' @keywords internal
collect_precision <- function(dt, precision_on, precision_by = character(0),
                               precision_data = NULL, precision_cap = NULL) {
  if (!is.null(precision_data)) {
    prec_dt <- data.table::as.data.table(precision_data)
    if (!all(c("max_int", "max_dec") %in% names(prec_dt))) {
      stop("precision_data must contain 'max_int' and 'max_dec' columns")
    }
    return(apply_precision_cap(prec_dt, precision_cap))
  }

  if (length(precision_by) > 0) {
    prec <- dt[, {
      vals <- get(precision_on)
      vals <- vals[!is.na(vals) & is.finite(vals)]
      collect_precision_values(vals)
    }, by = precision_by]
  } else {
    vals <- dt[[precision_on]]
    vals <- vals[!is.na(vals) & is.finite(vals)]
    prec <- data.table::as.data.table(collect_precision_values(vals))
  }

  apply_precision_cap(prec, precision_cap)
}

#' Compute max integer width and max decimal precision from a numeric vector
#'
#' @param vals Numeric vector (should already have NA/Inf removed)
#' @return list(max_int, max_dec)
#' @keywords internal
collect_precision_values <- function(vals) {
  if (length(vals) == 0) {
    return(list(max_int = 1L, max_dec = 0L))
  }

  # Max integer width: number of digits in the integer part
  abs_vals <- abs(vals)
  int_parts <- floor(abs_vals)
  int_chars <- str_length(as.character(as.integer(int_parts)))
  max_int <- max(int_chars, na.rm = TRUE)

  # Max decimal precision: count significant decimal places
  # Use format to avoid scientific notation
  char_vals <- format(vals, scientific = FALSE, trim = TRUE)
  dec_counts <- map_int(char_vals, function(s) {
    if (!str_detect(s, "\\.")) return(0L)
    dec_part <- str_replace(s, ".*\\.", "")
    # Remove trailing zeros
    dec_part <- str_replace(dec_part, "0+$", "")
    str_length(dec_part)
  })
  max_dec <- max(dec_counts, 0L, na.rm = TRUE)

  list(max_int = as.integer(max_int), max_dec = as.integer(max_dec))
}

#' Resolve auto-precision widths for a format group
#'
#' Given a parsed format group and collected precision values, returns
#' the effective integer width and decimal width.
#'
#' @param group A parsed format group (from parse_format_group)
#' @param max_int Integer, the collected max integer width
#' @param max_dec Integer, the collected max decimal precision
#'
#' @return list(int_width, dec_width)
#' @keywords internal
resolve_precision <- function(group, max_int, max_dec) {
  int_width <- if (group$int$auto) {
    max_int + group$int$offset
  } else {
    group$int$width
  }

  dec_width <- if (group$has_decimal && group$dec$auto) {
    max_dec + group$dec$offset
  } else {
    group$dec$width
  }

  # Ensure minimum widths
  int_width <- max(int_width, 1L)
  dec_width <- max(dec_width, 0L)

  list(int_width = as.integer(int_width), dec_width = as.integer(dec_width))
}

#' Apply precision caps
#'
#' Applies layer-level cap first, falls back to global option.
#'
#' @param prec data.table with max_int and max_dec columns
#' @param precision_cap Named numeric vector c(int=, dec=) or NULL
#' @return Modified data.table
#' @keywords internal
apply_precision_cap <- function(prec, precision_cap = NULL) {
  cap <- precision_cap
  if (is.null(cap)) {
    cap <- getOption("tplyr2.precision_cap", NULL)
  }
  if (is.null(cap)) return(prec)

  if ("int" %in% names(cap)) {
    prec[max_int > cap[["int"]], max_int := as.integer(cap[["int"]])]
  }
  if ("dec" %in% names(cap)) {
    prec[max_dec > cap[["dec"]], max_dec := as.integer(cap[["dec"]])]
  }

  prec
}
