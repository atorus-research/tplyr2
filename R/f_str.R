#' Create a format string object
#'
#' @param format_string Character string defining the display template
#' @param ... Character strings naming the variables that populate the template
#' @param empty Value(s) to display when data is NA/missing
#'
#' @return A tplyr_f_str object
#' @export
f_str <- function(format_string, ..., empty = NULL) {
  vars <- c(...)
  parsed <- parse_format_string(format_string)

  if (length(parsed$groups) != length(vars)) {
    stop(str_glue("Format string has {length(parsed$groups)} format group(s) but {length(vars)} variable(s) were provided"))
  }

  structure(
    list(
      format_string = format_string,
      vars = vars,
      parsed = parsed,
      empty = empty
    ),
    class = "tplyr_f_str"
  )
}

#' Apply format strings to numeric values
#'
#' Vectorized formatting function. Takes an f_str object and numeric vectors,
#' returns a character vector of formatted strings.
#'
#' @param fmt An f_str object or character format string
#' @param ... Numeric vectors, one per variable in the f_str (positional matching)
#' @param precision Optional list of resolved precision per group (for auto-precision)
#'
#' @return Character vector of formatted values
#' @export
apply_formats <- function(fmt, ..., precision = NULL) {
  if (is.character(fmt)) {
    # Parse on the fly for standalone use
    fmt <- f_str(fmt, ...)
    stop("Standalone character format strings require variable names. Use an f_str object.")
  }

  args <- list(...)
  parsed <- fmt$parsed
  groups <- parsed$groups
  literals <- parsed$literals

  if (length(args) != length(groups)) {
    stop(str_glue("Expected {length(groups)} numeric vector(s) but got {length(args)}"))
  }

  n <- length(args[[1]])

  # Format each variable column
  formatted_parts <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    prec_i <- if (!is.null(precision)) precision[[i]] else NULL
    formatted_parts[[i]] <- format_number_vec(args[[i]], groups[[i]], precision = prec_i)
  }

  # Paste together with literals, applying parenthesis hugging where needed
  result <- rep(literals[1], n)
  for (i in seq_along(groups)) {
    grp <- groups[[i]]
    is_hug <- grp$int$hug || grp$dec$hug

    if (is_hug) {
      # Hugging: shift leading spaces from number to after trailing literal
      result <- hug_format_group(result, formatted_parts[[i]], literals[i + 1])
    } else {
      result <- paste0(result, formatted_parts[[i]], literals[i + 1])
    }
  }

  # Handle empty values: if all format group values are NA, replace with empty
  if (!is.null(fmt$empty)) {
    all_na <- Reduce(`&`, map(args, is.na))
    if (".overall" %in% names(fmt$empty)) {
      result[all_na] <- fmt$empty[[".overall"]]
    }
  }

  result
}

#' Round numbers with optional IBM rounding
#'
#' When \code{getOption("tplyr2.IBMRounding", FALSE)} is TRUE, uses
#' round-half-away-from-zero (IBM convention) instead of R's default
#' banker's rounding (round half to even).
#'
#' @param x Numeric vector
#' @param digits Number of decimal places
#' @return Numeric vector
#' @keywords internal
tplyr_round <- function(x, digits = 0) {
  if (isTRUE(getOption("tplyr2.IBMRounding", FALSE))) {
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
  } else {
    round(x, digits)
  }
}

#' @keywords internal
format_number_vec <- function(values, group, precision = NULL) {
  if (!is.null(precision)) {
    int_width <- precision$int_width
    dec_width <- precision$dec_width
  } else {
    int_width <- group$int$width
    dec_width <- group$dec$width
  }
  total_width <- int_width + if (dec_width > 0) 1L + dec_width else 0L

  result <- character(length(values))
  na_mask <- is.na(values)

  if (any(!na_mask)) {
    if (dec_width > 0) {
      result[!na_mask] <- formatC(
        tplyr_round(values[!na_mask], dec_width),
        format = "f", digits = dec_width, width = total_width
      )
    } else {
      result[!na_mask] <- formatC(
        tplyr_round(values[!na_mask], 0),
        format = "d", width = int_width
      )
    }
  }

  if (any(na_mask)) {
    result[na_mask] <- strrep(" ", total_width)
  }

  result
}

#' Apply parenthesis hugging to a format group
#'
#' Shifts leading spaces from the formatted number to after the trailing
#' literal, so that characters like \code{(} hug the number.
#'
#' @param prefix Character vector of accumulated result so far
#' @param num_part Character vector of formatted numbers (with leading spaces)
#' @param trailing_literal Character string of the literal after this group
#'
#' @return Character vector with hugged result
#' @keywords internal
hug_format_group <- function(prefix, num_part, trailing_literal) {
  # Split trailing literal into body + closing delimiter (last character)
  lit_len <- str_length(trailing_literal)
  if (lit_len > 0) {
    lit_body <- str_sub(trailing_literal, 1L, lit_len - 1L)
    lit_close <- str_sub(trailing_literal, lit_len, lit_len)
  } else {
    lit_body <- ""
    lit_close <- ""
  }

  map_chr(seq_along(prefix), function(j) {
    np <- num_part[j]
    # Count leading spaces in formatted number
    stripped <- str_replace(np, "^ +", "")
    n_spaces <- str_length(np) - str_length(stripped)
    # Hugged: stripped number + literal body + shifted spaces + closing delimiter
    hugged <- str_c(stripped, lit_body, strrep(" ", n_spaces), lit_close)
    str_c(prefix[j], hugged)
  })
}

#' Parse a format string into groups and literals
#' @keywords internal
parse_format_string <- function(fmt) {
  # Pattern matches format groups: x/X/a/A characters with optional +N and decimal
  pattern <- "[xXaA]+(\\+\\d+)?(\\.[xXaA]+(\\+\\d+)?)?"

  match_positions <- str_locate_all(fmt, pattern)[[1]]

  if (nrow(match_positions) == 0) {
    stop("No format groups found in format string: ", fmt)
  }

  match_starts <- match_positions[, "start"]
  match_ends <- match_positions[, "end"]

  n_groups <- length(match_starts)
  groups <- vector("list", n_groups)
  literals <- character(n_groups + 1L)

  prev_end <- 0L
  for (i in seq_along(match_starts)) {
    # Literal before this group
    if (match_starts[i] > prev_end + 1L) {
      literals[i] <- str_sub(fmt, prev_end + 1L, match_starts[i] - 1L)
    }

    # Parse the group
    group_str <- str_sub(fmt, match_starts[i], match_ends[i])
    groups[[i]] <- parse_format_group(group_str)

    prev_end <- match_ends[i]
  }

  # Trailing literal
  if (prev_end < str_length(fmt)) {
    literals[n_groups + 1L] <- str_sub(fmt, prev_end + 1L, str_length(fmt))
  }

  list(groups = groups, literals = literals)
}

#' Parse a single format group
#' @keywords internal
parse_format_group <- function(group_str) {
  # Split on decimal point
  if (str_detect(group_str, "\\.")) {
    dot_pos <- str_locate(group_str, "\\.")[1, "start"]
    int_part <- str_sub(group_str, 1, dot_pos - 1L)
    dec_part <- str_sub(group_str, dot_pos + 1L, str_length(group_str))
  } else {
    int_part <- group_str
    dec_part <- ""
  }

  int_info <- parse_format_part(int_part)
  dec_info <- if (str_length(dec_part) > 0) {
    parse_format_part(dec_part)
  } else {
    list(width = 0L, auto = FALSE, offset = 0L, hug = FALSE)
  }

  list(
    int = int_info,
    dec = dec_info,
    has_decimal = str_length(dec_part) > 0
  )
}

#' Parse one side (int or dec) of a format group
#' @keywords internal
parse_format_part <- function(part) {
  # Check for +N suffix
  offset <- 0L
  if (str_detect(part, "\\+\\d+$")) {
    offset_match <- str_extract(part, "\\+\\d+$")
    offset <- as.integer(str_replace(offset_match, "\\+", ""))
    part <- str_replace(part, "\\+\\d+$", "")
  }

  chars <- str_split(part, "")[[1]]
  width <- length(chars)
  auto <- any(chars %in% c("a", "A"))
  hug <- any(chars %in% c("X", "A"))

  list(width = width, auto = auto, offset = offset, hug = hug)
}

#' @export
print.tplyr_f_str <- function(x, ...) {
  cat(str_glue("tplyr format string: \"{x$format_string}\"\n"))
  cat(str_glue("  Variables: {str_c(x$vars, collapse = ', ')}\n"))
  if (!is.null(x$empty)) {
    cat(str_glue("  Empty: {deparse(x$empty)}\n"))
  }
  invisible(x)
}
