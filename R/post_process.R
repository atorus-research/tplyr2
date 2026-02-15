#' Apply row masks to blank repeated row labels
#'
#' Walks each \code{rowlabel*} column top-to-bottom and blanks values that
#' are identical to the previous row, respecting layer boundaries
#' (\code{ord_layer_index}).
#'
#' @param result A data.frame produced by \code{tplyr_build()}
#' @param row_breaks Logical. If TRUE, insert a blank row between layers.
#'
#' @return A data.frame with repeated labels blanked
#' @export
apply_row_masks <- function(result, row_breaks = FALSE) {
  if (!is.data.frame(result) || nrow(result) == 0) return(result)

  label_cols <- sort(str_subset(names(result), "^rowlabel\\d+$"))
  if (length(label_cols) == 0) return(result)

  layer_idx <- result[["ord_layer_index"]]


  for (col in label_cols) {
    vals <- result[[col]]
    prev_vals <- c(NA_character_, vals[-length(vals)])

    # Identify rows where current value matches the previous row
    same_as_prev <- !is.na(vals) & !is.na(prev_vals) & vals == prev_vals

    # Don't mask across layer boundaries
    if (!is.null(layer_idx)) {
      prev_layer <- c(NA_real_, layer_idx[-length(layer_idx)])
      layer_boundary <- is.na(layer_idx) | is.na(prev_layer) | layer_idx != prev_layer
      same_as_prev <- same_as_prev & !layer_boundary
    }

    result[[col]][same_as_prev] <- ""
  }

  if (row_breaks && !is.null(layer_idx)) {
    layers <- unique(layer_idx[!is.na(layer_idx)])
    if (length(layers) > 1) {
      # Find positions where layer changes
      break_positions <- which(diff(layer_idx) != 0)
      if (length(break_positions) > 0) {
        # Build blank row template
        blank <- result[1, , drop = FALSE]
        blank[1, ] <- map(blank, function(col) if (is.numeric(col)) NA_real_ else "")

        # Split at break positions and reassemble with blank rows interleaved
        split_at <- c(0L, break_positions, nrow(result))
        chunks <- map2(
          head(split_at, -1) + 1L,
          tail(split_at, -1),
          function(from, to) result[from:to, , drop = FALSE]
        )
        blanks <- map(seq_len(length(chunks) - 1), ~blank)
        pieces <- c(
          list(chunks[[1]]),
          unlist(map2(blanks, chunks[-1], ~list(.x, .y)), recursive = FALSE)
        )
        result <- do.call(rbind, pieces)
        rownames(result) <- NULL
      }
    }
  }

  result
}

#' Collapse row labels into a single column
#'
#' This is a generalized post processing function that allows you to take groups
#' of by variables and collapse them into a single column. Repeating values are
#' split into separate rows, and for each level of nesting, a specified
#' indentation level can be applied.
#'
#' @param x Input data frame
#' @param ... Column names (as character strings) to be collapsed, must be 2 or
#'   more
#' @param indent Indentation string to be used, which is multiplied at each
#'   indentation level
#' @param target_col Character string naming the output column containing
#'   collapsed row labels
#'
#' @return data.frame with row labels collapsed into a single column
#' @export
#'
#' @examples
#' x <- data.frame(
#'   row_label1 = c("A", "A", "A", "B", "B"),
#'   row_label2 = c("C", "C", "D", "E", "F"),
#'   var1 = 1:5,
#'   stringsAsFactors = FALSE
#' )
#'
#' collapse_row_labels(x, "row_label1", "row_label2")
#'
#' collapse_row_labels(x, "row_label1", "row_label2", indent = "    ",
#'                     target_col = "rl")
#'
collapse_row_labels <- function(x, ..., indent = "  ", target_col = "row_label") {

  dot_names <- c(...)

  if (!inherits(x, "data.frame")) {
    stop("x must be a data frame", call. = FALSE)
  }

  if (!inherits(indent, "character")) {
    stop("indent must be a character string", call. = FALSE)
  }

  if (!is.character(dot_names)) {
    stop("Column names must be provided as character strings.", call. = FALSE)
  }

  if (!all(dot_names %in% names(x))) {
    missing <- setdiff(dot_names, names(x))
    stop(str_glue("Columns missing from x: {str_c(missing, collapse = ', ')}"), call. = FALSE)
  }

  if (!is.character(target_col) || length(target_col) != 1) {
    stop("target_col must be a single character string.", call. = FALSE)
  }

  if (length(dot_names) < 2) {
    stop("Must have two or more columns to collapse", call. = FALSE)
  }

  all_but_last_names <- dot_names[seq_len(length(dot_names) - 1)]
  last_name <- dot_names[length(dot_names)]

  # Work with data.table copy
  dt <- as.data.table(x)
  dt[, og_row := .I]

  # Get distinct combinations of all-but-last columns with their first og_row
  rowlabs <- dt[, c(all_but_last_names, "og_row"), with = FALSE]
  stubs_dt <- rowlabs[, .SD[1], by = all_but_last_names]

  # Melt to create stub rows (pivot_longer equivalent)
  stubs_long <- melt(
    stubs_dt,
    id.vars = "og_row",
    measure.vars = all_but_last_names,
    variable.name = ".var_name",
    value.name = target_col
  )

  # Sort to match pivot_longer interleaving: by og_row, then column order
  setorder(stubs_long, og_row, .var_name)

  # Add stub_sort (nesting level within each og_row group)
  stubs_long[, stub_sort := seq_len(.N), by = og_row]
  stubs_long[, .var_name := NULL]
  stubs_long[, id := "2"]

  # Prepare original data
  dt[, id := "1"]
  dt[, stub_sort := NA_integer_]

  # Combine stubs with original data
  combined <- rbindlist(list(dt, stubs_long), fill = TRUE, use.names = TRUE)

  # Sort: by og_row, then stubs before data (id "2" > "1")
  setorder(combined, og_row, -id)

  # Forward fill stub_sort
  combined[, stub_sort := nafill(stub_sort, type = "locf")]

  # Original rows get one more indent level
  combined[id == "1", stub_sort := stub_sort + 1L]

  # For original rows, fill target column from last dot column
  combined[id == "1" & is.na(get(target_col)), (target_col) := get(last_name)]

  # Apply indentation
  combined[, (target_col) := map2_chr(
    stub_sort, get(target_col), add_indentation, indent = indent
  )]

  # Fill NA characters with empty string
  char_cols <- names(combined)[map_lgl(combined, is.character)]
  for (col in char_cols) {
    set(combined, which(is.na(combined[[col]])), col, "")
  }

  # Select: target_col first, then everything except helpers and dot columns
  remove_cols <- c("id", "og_row", "stub_sort", dot_names)
  keep_cols <- c(target_col, setdiff(names(combined), c(target_col, remove_cols)))

  as.data.frame(combined[, keep_cols, with = FALSE])
}

#' Add indentation based on nesting level
#'
#' @param .x The number of levels to indent
#' @param .y Input variable for which indentation will be done
#' @param indent Indentation string to be used, which is multiplied at each
#'   indentation level
#'
#' @return Character string with indentation applied
#' @noRd
add_indentation <- function(.x, .y, indent = "  ") {
  str_c(c(rep("", .x - 1), .y), collapse = indent)
}

#' Wrap strings to a specific width with hyphenation while preserving
#' indentation
#'
#' Leverages \code{stringr::str_wrap()} under the hood, but takes extra steps
#' to preserve any indentation that has been applied to a character element,
#' and use hyphenated wrapping of single words that run longer than the
#' allotted wrapping width.
#'
#' \code{stringr::str_wrap()} is highly efficient, but in the context of table
#' creation there are two features missing --- hyphenation for long running
#' strings that overflow width, and respect for pre-indentation of a character
#' element. For example, in an adverse event table, you may have body system
#' rows as an un-indented column, and preferred terms as indented columns. These
#' strings may run long and require wrapping to not surpass the column width.
#' Furthermore, for crowded tables a single word may be longer than the column
#' width itself.
#'
#' This function resolves these two issues, while minimizing additional overhead
#' required to apply the wrapping of strings.
#'
#' Note: This function automatically converts tabs to spaces. Tab width varies
#' depending on font, so width cannot automatically be determined within a data
#' frame. Users can specify the width via \code{tab_width}.
#'
#' @param x An input character vector
#' @param width The desired width of elements within the output character vector
#' @param tab_width The number of spaces to which tabs should be converted
#'
#' @return A character vector with string wrapping applied
#' @export
#'
#' @examples
#' ex_text1 <- c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS")
#' ex_text2 <- c("RENAL AND URINARY DISORDERS", "\tNEPHROLITHIASIS")
#'
#' cat(paste(str_indent_wrap(ex_text1, width = 8), collapse = "\n\n"), "\n")
#' cat(paste(str_indent_wrap(ex_text2, tab_width = 4), collapse = "\n\n"), "\n")
str_indent_wrap <- function(x, width = 10, tab_width = 5) {

  if (!inherits(x, "character")) {
    stop("x must be a character vector", call. = FALSE)
  }

  # Scan out tabs and convert them to spaces
  x <- str_replace_all(x, "\\t", strrep(" ", tab_width))

  # Find where the splits need to happen for hyphenation
  sections <- str_locate_all(x, str_c("\\w{", width - 1, "}(?=\\w)"))

  # Using the locations, build up the matrix of substrings
  split_mat <- map(sections, ~ matrix(c(1, .[, 2] + 1, .[, 2], -1), ncol = 2))

  # Divide the string into the necessary chunks
  splits <- map2(x, split_mat, str_sub)

  hyph_str <- map_chr(splits, str_c, collapse = "- ")

  # Get the indentation of the strings and build a data.table for grouped wrapping
  l <- get_ind_len(x)
  w <- width - l

  wrap_dt <- data.table(
    .idx = seq_along(x),
    l = l,
    w = w,
    s = hyph_str
  )

  # Group by width and indent length, wrap the strings, and return the output vector
  wrap_dt[, out := str_wrap(s, width = w[1L], indent = l[1L], exdent = l[1L]), by = .(w, l)]

  wrap_dt[order(.idx), out]
}

#' Get the indentation length
#'
#' Vectorized approach to extracting the length of indentation, with
#' compensation for NAs.
#'
#' @param s Input string to have indentation length measured
#'
#' @return Integer vector of character length of indentation
#' @noRd
get_ind_len <- function(s) {
  inds <- str_extract(s, "^\\s+")
  inds[is.na(inds)] <- ""
  str_length(inds)
}

#' Conditional reformatting of a pre-populated string of numbers
#'
#' This function allows you to conditionally re-format a string of numbers
#' based on a numeric value within the string itself. By selecting a "format
#' group", which is targeting a specific number within the string, a user can
#' establish a condition upon which a provided replacement string can be used.
#' Either the entire replacement can be used to replace the entire string, or
#' the replacement text can refill the "format group" while preserving the
#' original width and alignment of the target string.
#'
#' @param string Target character vector where text may be replaced
#' @param format_group An integer representing the targeted numeric field within
#'   the string, numbered from left to right
#' @param condition An expression, using the variable name \code{x} as the
#'   target variable within the condition
#' @param replacement A string to use as the replacement value
#' @param full_string TRUE if the full string should be replaced, FALSE if the
#'   replacement should be done within the format group
#'
#' @return A character vector
#' @export
#'
#' @examples
#' string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")
#'
#' apply_conditional_format(string, 2, x == 0, " 0        ", full_string = TRUE)
#'
#' apply_conditional_format(string, 2, x < 1, "(<1%)")
#'
apply_conditional_format <- function(string, format_group, condition, replacement, full_string = FALSE) {

  condition <- rlang::enexpr(condition)

  # Validate all parameters
  validate_conditional_format_params(string, format_group, condition, replacement, full_string)

  # Pull out regex to drive the work
  f_grp_rx <- get_format_group_regex()

  # Pull out all the match groups and then get the numeric for the conditional number
  match_groups <- str_match_all(string, f_grp_rx)

  # Get the number upon which the condition will be evaluated
  x <- map_dbl(
    match_groups,
    ~ if (nrow(.) < format_group) NA_real_ else as.double(.[format_group, 2])
  )

  # Get the bool vector for where strings should be replaced and handle NAs
  tf <- eval(condition)
  tf[is.na(tf)] <- FALSE

  if (full_string) {
    out_string <- ifelse(tf, replacement, string)
  } else {
    # Grab the match locations to use for sub stringing
    match_locs <- str_locate_all(string, f_grp_rx)
    # Get the group length out to ensure that the string is fully padded
    group_length <- map_int(
      match_groups,
      ~ if (nrow(.) < format_group) NA_integer_ else as.integer(str_length(.[format_group, 1]))
    )

    if (any(str_length(replacement) > group_length[!is.na(group_length)])) {
      warning(
        str_c(
          "Replacement string length is longer that some string's format group length.",
          "Some alignment will not be preserved"
        )
      )
    }

    # Pad at least as long as the format group space
    pad_length <- map_int(
      group_length,
      ~ if (str_length(replacement) > .x) str_length(replacement) else .x
    )

    # Pull out locs for the format group
    end_locs <- map_int(
      match_locs,
      ~ if (nrow(.) < format_group) NA_integer_ else .[format_group, "end"]
    )
    start_locs <- end_locs - pad_length + 1

    # Build the sub string matrix
    sub_mat <- matrix(c(rbind(start_locs, end_locs)), ncol = 2, byrow = TRUE)

    # Generate a vector with replacements already done
    rep_string <- string
    str_sub(rep_string, sub_mat) <- str_pad(replacement, pad_length)

    out_string <- ifelse(tf, rep_string, string)
  }

  out_string
}

#' Get regex pattern for format groups
#'
#' Builds a regex to identify numeric "format groups" within formatted
#' strings. A format group is a numeric value (integer or decimal, possibly
#' negative) surrounded by optional non-digit characters.
#'
#' @return A character string regex pattern
#' @noRd
get_format_group_regex <- function() {
  nwsd <- "[^\\s\\d]*"            # 0 or more non-whitespace-or-digit chars
  ws   <- "\\s*"                  # 0 or more whitespace
  num  <- "(\\-?\\d+(\\.\\d+)?)"  # Positive or negative integer or decimal
  nws  <- "\\S*"                  # 0 or more non-whitespace
  str_c(nwsd, ws, num, nws)
}

#' Validate parameters for apply_conditional_format
#'
#' @noRd
validate_conditional_format_params <- function(string, format_group, condition, replacement, full_string) {
  if (!inherits(string, "character")) {
    stop("Parameter `string` must be a character vector", call. = FALSE)
  }

  if (!inherits(format_group, "numeric") || (inherits(format_group, "numeric") && format_group %% 1 != 0)) {
    stop("Parameter `format_group` must be an integer", call. = FALSE)
  }

  if (!inherits(replacement, "character")) {
    stop("Parameter `replacement` must be a string", call. = FALSE)
  }

  # Condition statement must use the variable name 'x'
  if (!identical(all.vars(condition), "x")) {
    stop("Condition must be a valid expression only using the variable name `x`", call. = FALSE)
  }

  if (!inherits(full_string, "logical")) {
    stop("Parameter `full_string` must be bool", call. = FALSE)
  }
}

#' Extract numeric values from formatted strings
#'
#' Extracts the Nth numeric value from a formatted tplyr2 string.
#'
#' @param x Character vector of formatted strings
#' @param index Integer, which numeric value to extract (1-based)
#'
#' @return Numeric vector
#' @export
str_extract_num <- function(x, index = 1L) {
  map_dbl(x, function(s) {
    if (is.na(s)) return(NA_real_)
    nums <- str_extract_all(s, "-?[0-9]+\\.?[0-9]*")[[1]]
    if (length(nums) < index) return(NA_real_)
    as.numeric(nums[index])
  })
}

#' Replace leading whitespace with a specified string
#'
#' Useful for HTML rendering where leading spaces are collapsed.
#'
#' @param x Character vector
#' @param replace_with Replacement string for each leading space
#'
#' @return Character vector with leading spaces replaced
#' @export
replace_leading_whitespace <- function(x, replace_with = "\u00a0") {
  map_chr(x, function(s) {
    if (is.na(s)) return(NA_character_)
    stripped <- str_replace(s, "^ +", "")
    leading <- str_length(s) - str_length(stripped)
    if (leading > 0) {
      str_c(strrep(replace_with, leading), stripped)
    } else {
      s
    }
  })
}
