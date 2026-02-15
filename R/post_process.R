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
    for (i in seq_len(nrow(result))[-1]) {
      # Don't mask across layer boundaries
      if (!is.null(layer_idx) && !is.na(layer_idx[i]) && !is.na(layer_idx[i - 1]) &&
          layer_idx[i] != layer_idx[i - 1]) {
        next
      }
      if (!is.na(vals[i]) && !is.na(vals[i - 1]) && vals[i] == vals[i - 1]) {
        result[[col]][i] <- ""
      }
    }
  }

  if (row_breaks && !is.null(layer_idx)) {
    layers <- unique(layer_idx[!is.na(layer_idx)])
    if (length(layers) > 1) {
      # Find positions where layer changes
      break_positions <- which(diff(layer_idx) != 0)
      if (length(break_positions) > 0) {
        # Build blank row template
        blank <- result[1, , drop = FALSE]
        for (col_name in names(blank)) {
          if (is.numeric(blank[[col_name]])) {
            blank[[col_name]] <- NA_real_
          } else {
            blank[[col_name]] <- ""
          }
        }

        # Insert blank rows (from bottom to top to preserve indices)
        for (pos in rev(break_positions)) {
          top <- result[seq_len(pos), , drop = FALSE]
          bottom <- result[seq(pos + 1, nrow(result)), , drop = FALSE]
          result <- rbind(top, blank, bottom)
        }
        rownames(result) <- NULL
      }
    }
  }

  result
}

#' Collapse multiple row label columns into one with indentation
#'
#' Merges all \code{rowlabel*} columns into a single \code{row_label} column.
#' Deeper nesting levels receive progressively more indentation.
#'
#' @param result A data.frame produced by \code{tplyr_build()}
#' @param indent Character string used for each level of indentation
#'
#' @return A data.frame with a single \code{row_label} column replacing the
#'   \code{rowlabel*} columns
#' @export
collapse_row_labels <- function(result, indent = "  ") {
  if (!is.data.frame(result) || nrow(result) == 0) return(result)

  label_cols <- sort(str_subset(names(result), "^rowlabel\\d+$"))
  if (length(label_cols) == 0) return(result)

  row_label <- character(nrow(result))
  for (i in seq_len(nrow(result))) {
    # Find the deepest non-empty label
    depth <- 0L
    value <- ""
    for (j in seq_along(label_cols)) {
      val <- result[[label_cols[j]]][i]
      if (!is.na(val) && str_length(val) > 0) {
        depth <- j
        value <- val
      }
    }
    if (depth > 0) {
      row_label[i] <- paste0(strrep(indent, depth - 1L), value)
    }
  }

  # Remove old label columns and insert row_label
  other_cols <- setdiff(names(result), label_cols)
  result <- result[, other_cols, drop = FALSE]
  result <- cbind(data.frame(row_label = row_label, stringsAsFactors = FALSE), result)
  result
}

#' Add column headers to a result table
#'
#' Builds header rows from the column labels on \code{res*} columns and
#' optionally from the \code{header_n} attribute.
#'
#' @param result A data.frame produced by \code{tplyr_build()}
#' @param header_format Optional character string with \code{" | "}-delimited
#'   column header entries. Use \code{**level**} placeholders to substitute
#'   \code{(N=n)} from header N data.
#'
#' @return A data.frame with header row(s) prepended
#' @export
add_column_headers <- function(result, header_format = NULL) {
  if (!is.data.frame(result) || nrow(result) == 0) return(result)

  res_cols <- sort(str_subset(names(result), "^res\\d+$"))
  if (length(res_cols) == 0) return(result)

  # Build header from column labels
  header_row <- result[1, , drop = FALSE]
  for (col_name in names(header_row)) {
    if (is.numeric(header_row[[col_name]])) {
      header_row[[col_name]] <- NA_real_
    } else {
      header_row[[col_name]] <- ""
    }
  }

  if (!is.null(header_format)) {
    parts <- str_split(header_format, " \\| ")[[1]]
    # First part goes to row label column
    label_cols <- sort(str_subset(names(result), "^rowlabel\\d+$|^row_label$"))
    if (length(label_cols) > 0 && length(parts) > 0) {
      header_row[[label_cols[1]]] <- parts[1]
    }
    # Remaining parts go to res columns
    for (i in seq_along(res_cols)) {
      if (i + 1 <= length(parts)) {
        header_row[[res_cols[i]]] <- parts[i + 1]
      }
    }
  } else {
    # Use column label attributes
    for (rc in res_cols) {
      lbl <- attr(result[[rc]], "label")
      if (!is.null(lbl)) {
        header_row[[rc]] <- lbl
      }
    }
  }

  # Substitute **level** placeholders with (N=n)
  header_n <- attr(result, "header_n")
  if (!is.null(header_n) && is.data.frame(header_n)) {
    for (rc in res_cols) {
      val <- header_row[[rc]]
      if (is.character(val) && str_detect(val, "\\*\\*")) {
        for (r in seq_len(nrow(header_n))) {
          # Get the level name from the first non-.n column
          level_col <- setdiff(names(header_n), ".n")
          level_val <- as.character(header_n[[level_col[1]]][r])
          n_val <- header_n$.n[r]
          pattern <- str_c("\\*\\*", level_val, "\\*\\*")
          replacement <- str_c("(N=", n_val, ")")
          val <- str_replace_all(val, pattern, replacement)
        }
        header_row[[rc]] <- val
      }
    }
  }

  result <- rbind(header_row, result)
  rownames(result) <- NULL
  result
}

#' Wrap text with indentation
#'
#' Thin wrapper around \code{strwrap()} that returns a single string with
#' newlines for text wrapping.
#'
#' @param x Character string to wrap
#' @param width Maximum line width
#' @param indent Indentation for the first line
#' @param exdent Indentation for subsequent lines (hanging indent)
#'
#' @return Character string with newlines inserted
#' @export
str_indent_wrap <- function(x, width = 80, indent = 0, exdent = 0) {
  map_chr(x, function(s) {
    if (is.na(s) || str_length(s) == 0) return(s)
    wrapped <- strwrap(s, width = width, indent = indent, exdent = exdent)
    str_c(wrapped, collapse = "\n")
  })
}

#' Apply conditional formatting to a column
#'
#' Applies a formatting function to cells in a column where a condition is met.
#'
#' @param result A data.frame
#' @param column Character string naming the column to format
#' @param condition_fn Function that takes a value and returns logical
#' @param format_fn Function that takes a value and returns the formatted value
#'
#' @return Modified data.frame
#' @export
apply_conditional_format <- function(result, column, condition_fn, format_fn) {
  if (!column %in% names(result)) {
    stop(str_glue("Column '{column}' not found in result"))
  }

  vals <- result[[column]]
  mask <- map_lgl(vals, condition_fn)
  result[[column]][mask] <- map_chr(vals[mask], format_fn)
  result
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
