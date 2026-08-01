#' Create a format string object
#'
#' @param format_string Character string defining the display template
#' @param ... Character strings naming the variables that populate the template
#' @param empty Value to display when data is NA/missing. Supplied as
#'   \code{c(.overall = "...")}, it replaces the entire cell, but only once
#'   \emph{every} format group in the string is NA. Supplied unnamed (e.g.
#'   \code{empty = "NA"}), it instead fills each NA format group in place,
#'   right-justified to the width that group would have occupied, so a partially
#'   missing cell keeps its alignment -- \code{f_str("xx (xxx)", "n", "pct",
#'   empty = "NA")} renders \code{"NA ( NA)"}. The default (\code{NULL}) leaves
#'   NA groups as blanks of the field width.
#'
#' @return A tplyr_f_str object
#'
#' @details
#' Each run of `x` characters is one format group, and each group is filled by
#' the correspondingly-positioned variable in `...`. The count of `x`s sets the
#' field width, so `"xx.x"` renders two integer digits and one decimal. Literal
#' text between groups is preserved verbatim. `a` (and `A`) request
#' auto-precision, where the decimal count comes from the data.
#'
#' @examples
#' # Two format groups filled by n and pct
#' fmt <- f_str("xx (xx.x%)", "n", "pct")
#' fmt
#' apply_formats(fmt, n = c(5, 12), pct = c(4.5, 33.33))
#'
#' # Width is set by the number of x's
#' apply_formats(f_str("xxx", "n"), n = 7)
#' apply_formats(f_str("x", "n"), n = 7)
#'
#' # `empty` fills each NA group in place, preserving alignment
#' apply_formats(f_str("xx (xxx)", "n", "pct", empty = "NA"),
#'               n = NA, pct = NA)
#'
#' # `.overall` replaces the whole cell, but only when every group is NA
#' both_na <- f_str("xx (xxx)", "n", "pct", empty = c(.overall = "Not est."))
#' apply_formats(both_na, n = NA, pct = NA)
#'
#' # Used in a layer
#' spec <- tplyr_spec(
#'   cols = "TRT01P",
#'   layers = tplyr_layers(
#'     group_count("AGEGR1", settings = layer_settings(
#'       format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))))
#'   )
#' )
#' tplyr_build(spec, tplyr_adsl)
#'
#' @seealso [apply_formats()] to render values outside a build.
#' @export
f_str <- function(format_string, ..., empty = NULL) {
  vars <- c(...)
  parsed <- parse_format_string(format_string)

  if (length(parsed$groups) != length(vars)) {
    stop(str_glue("Format string has {length(parsed$groups)} format group(s) but {length(vars)} variable(s) were provided"))
  }

  # Parenthesis hugging relocates a number's leading spaces to just inside the
  # trailing literal. A hugged group with no literal in front of it has nothing
  # to hug, and simply left-justifies with trailing whitespace instead.
  hug_no_delim <- map_lgl(seq_along(parsed$groups), function(i) {
    grp <- parsed$groups[[i]]
    (grp$int$hug || grp$dec$hug) && !nzchar(parsed$literals[i])
  })
  if (any(hug_no_delim)) {
    warning(str_glue(
      "Format string \"{format_string}\": format group ",
      "{str_c(which(hug_no_delim), collapse = ', ')} uses parenthesis hugging ",
      "(X/A) but has no literal text before it, so there is nothing to hug -- ",
      "the number will be left-justified with trailing spaces. Use lowercase ",
      "x/a for a leading group."
    ), call. = FALSE)
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
#' @param fmt An [f_str()] object. A bare character format string is rejected,
#'   since the variable names are what bind `...` to the format groups.
#' @param ... Numeric vectors, one per variable in the f_str (positional matching)
#' @param precision Optional list of resolved precision per group (for auto-precision)
#' @param lt Optional numeric less-than threshold applied to the group named by
#'   `lt_gt_group`: values in `(0, lt)` render as `"<" lt` (see [format_number_vec()]).
#' @param gt Optional numeric greater-than threshold applied to the group named by
#'   `lt_gt_group`: values in `(gt, 100)` render as `">" gt`.
#' @param lt_gt_group Optional integer index of the format group to which `lt`/`gt`
#'   apply (used by count layers to target the percent statistic). NULL disables.
#' @param na Optional string substituted for cells whose format-group inputs are
#'   all NA, used *instead of* the default blank-width fill. `na = ""` produces a
#'   truly empty cell (`nchar` 0); `na = "NE"` renders `"NE"`. The default `NULL`
#'   preserves the blank-width fill. This lets `apply_formats()` replace
#'   hand-rolled fixed-width formatters for externally row-bound statistics.
#' @param width Optional integer total width to pad each formatted token to,
#'   using [stringr::str_pad()]. When the `na` substitution applies to a cell,
#'   `na` wins and that cell is *not* padded. The default `NULL` leaves tokens at
#'   their natural format width.
#' @param pad Side to pad on when `width` is set: `"right"` (default, trailing
#'   spaces) or `"left"` (leading spaces).
#'
#' @return Character vector of formatted values
#'
#' @examples
#' # Vectorized: one formatted string per element
#' apply_formats(f_str("xx (xx.x%)", "n", "pct"),
#'               n = c(5, 12, 103), pct = c(4.5, 33.333, 99.9))
#'
#' # `na` replaces the default blank-width fill
#' apply_formats(f_str("xx.x", "mean"), mean = c(1.2, NA))
#' apply_formats(f_str("xx.x", "mean"), mean = c(1.2, NA), na = "NE")
#' apply_formats(f_str("xx.x", "mean"), mean = c(1.2, NA), na = "")
#'
#' # lt/gt thresholds, targeting the percent group (index 2)
#' apply_formats(f_str("xx (xx.x%)", "n", "pct"), n = c(1, 199), pct = c(0.4, 99.7),
#'               lt = 1, gt = 99, lt_gt_group = 2)
#'
#' # Pad to a fixed width for row-binding against other output
#' apply_formats(f_str("xx.x", "mean"), mean = c(1.2, 10.75), width = 10)
#' apply_formats(f_str("xx.x", "mean"), mean = c(1.2, 10.75), width = 10, pad = "left")
#'
#' @seealso [f_str()] for the format-string grammar.
#' @export
apply_formats <- function(fmt, ..., precision = NULL, lt = NULL, gt = NULL,
                          lt_gt_group = NULL, na = NULL, width = NULL,
                          pad = c("right", "left")) {
  if (is.character(fmt)) {
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

  # Two `empty` modes. A value keyed by `.overall` replaces the whole cell once
  # every format group is NA (applied after assembly, below). Any other form is
  # a per-group fill: each NA group is replaced in place, right-justified to the
  # width that group would have occupied, so the column stays aligned.
  empty_overall <- if (!is.null(fmt$empty) && ".overall" %in% names(fmt$empty)) {
    fmt$empty[[".overall"]]
  } else {
    NULL
  }
  empty_fill <- if (!is.null(fmt$empty) && is.null(empty_overall)) {
    as.character(fmt$empty)[1]
  } else {
    NULL
  }

  # Format each variable column
  formatted_parts <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    prec_i <- if (!is.null(precision)) precision[[i]] else NULL
    lt_i <- if (!is.null(lt_gt_group) && i == lt_gt_group) lt else NULL
    gt_i <- if (!is.null(lt_gt_group) && i == lt_gt_group) gt else NULL
    formatted_parts[[i]] <- format_number_vec(args[[i]], groups[[i]],
                                              precision = prec_i, lt = lt_i, gt = gt_i)

    if (!is.null(empty_fill)) {
      na_i <- is.na(args[[i]])
      if (any(na_i)) {
        # format_number_vec() fills an NA with spaces of the field width, so the
        # blank cell itself carries the width to justify against.
        fill_width <- str_length(formatted_parts[[i]][na_i][1])
        formatted_parts[[i]][na_i] <- formatC(empty_fill, width = fill_width)
      }
    }
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

  # Handle empty values: if all format group values are NA, replace the cell
  if (!is.null(empty_overall)) {
    all_na <- Reduce(`&`, map(args, is.na))
    result[all_na] <- empty_overall
  }

  # Optional fixed total-width padding of the whole token
  if (!is.null(width)) {
    result <- str_pad(result, width = width, side = match.arg(pad))
  }

  # NA substitution: rows whose format-group inputs are all NA render as `na`.
  # Applied last so it wins over both the blank-width fill and width padding.
  if (!is.null(na)) {
    all_na <- Reduce(`&`, map(args, is.na))
    result[all_na] <- na
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

#' Format a numeric vector to a fixed-width field
#'
#' @param values Numeric vector to format
#' @param group A parsed format group (from [parse_format_group()])
#' @param precision Optional resolved precision (int_width/dec_width) overriding
#'   the group's static widths
#' @param lt Optional numeric less-than threshold. Values strictly greater than 0
#'   whose rounded display would fall below `lt` render as `"<" lt` (e.g. a
#'   percent of 0.4 renders as `<1`), right-justified to the field width. Used
#'   for the regulatory "<1%" convention on count-layer percents.
#' @param gt Optional numeric greater-than threshold. Values strictly less than
#'   100 whose rounded display would exceed `gt` render as `">" gt` (e.g. 99.6
#'   renders as `>99`).
#' @keywords internal
format_number_vec <- function(values, group, precision = NULL, lt = NULL, gt = NULL) {
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
      rounded <- tplyr_round(values[!na_mask], dec_width)
      # Normalize negative zero to zero so a value rounding to -0 displays as
      # "0.0", matching base R format() which drops the sign (issue #29).
      rounded[rounded == 0] <- 0
      result[!na_mask] <- formatC(
        rounded, format = "f", digits = dec_width, width = total_width
      )
    } else {
      rounded <- tplyr_round(values[!na_mask], 0)
      rounded[rounded == 0] <- 0
      result[!na_mask] <- formatC(
        rounded, format = "d", width = int_width
      )
    }
  }

  if (any(na_mask)) {
    result[na_mask] <- strrep(" ", total_width)
  }

  # Less-than / greater-than thresholds (e.g. "<1", ">99" percent display).
  # Compare against the rounded display value so a value that rounds up to the
  # threshold (0.6% -> 1%) still shows its number rather than the "<" token.
  fmt_threshold <- function(x) {
    if (dec_width > 0) {
      str_trim(formatC(x, format = "f", digits = dec_width))
    } else {
      str_trim(formatC(as.integer(tplyr_round(x, 0)), format = "d"))
    }
  }
  if (!is.null(lt)) {
    disp <- tplyr_round(values, dec_width)
    m <- !na_mask & values > 0 & disp < lt
    if (any(m)) {
      result[m] <- formatC(str_c("<", fmt_threshold(lt)), width = total_width)
    }
  }
  if (!is.null(gt)) {
    disp <- tplyr_round(values, dec_width)
    m <- !na_mask & values < 100 & disp > gt
    if (any(m)) {
      result[m] <- formatC(str_c(">", fmt_threshold(gt)), width = total_width)
    }
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
  # str_glue() strips the trailing newline, so emit it separately
  cat(str_glue("tplyr format string: \"{x$format_string}\""), "\n", sep = "")
  cat(str_glue("  Variables: {str_c(x$vars, collapse = ', ')}"), "\n", sep = "")
  if (!is.null(x$empty)) {
    cat(str_glue("  Empty: {str_c(deparse(x$empty), collapse = '')}"), "\n", sep = "")
  }
  invisible(x)
}
