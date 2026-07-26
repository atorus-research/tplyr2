#' Association-test column(s) for count and shift layers
#'
#' Configures an association test and lands its formatted result beside the
#' n(\%) block. Two modes are supported, selected by whether \code{comparisons}
#' is supplied:
#'
#' \strong{Omnibus mode} (\code{comparisons = NULL}, the default). Runs
#' \code{fn} once per \code{by} group, \emph{across} the treatment columns, and
#' lands its result as a single trailing column. The supplied function receives
#' the raw source-data subset for the \code{by} group (all \code{cols} levels
#' and all target/row levels), so a caller can tabulate and test naturally
#' (e.g. \code{fisher.test(table(.data$TRT, .data$RESP))} or
#' \code{coin::cmh_test(...)}). When the layer has no \code{by} variable the
#' test runs once over the whole layer; otherwise once per \code{by} group, with
#' the value placed on that group's first output row.
#'
#' \strong{Pairwise / per-level mode} (\code{comparisons} non-\code{NULL}).
#' Count layers only. Emits one \code{pval} column per comparison, each
#' comparing an arm level of the first \code{cols} variable to \code{reference},
#' with a value on \emph{every} target-level row (like \code{risk_diff}'s
#' \code{rdiff} columns). Here \code{fn} receives, for one (target level,
#' comparison) pair, a 2x2 contingency \strong{matrix}
#' \code{matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)} --
#' rows are (reference, comparison) arm, columns are (event, no event) -- where
#' \code{n} is the cell count and \code{N} the population denominator for that
#' arm. When the layer sets \code{distinct_by}, the distinct counts/denominators
#' are used. \code{fn} returns a scalar p-value -- numeric (formatted with
#' \code{format}) or a verbatim character display string (\code{NA} renders a
#' blank).
#'
#' Attach it to a layer via
#' \code{layer_settings(assoc_test = assoc_test(...))}.
#'
#' @param fn A function of one argument. In omnibus mode it is called with the
#'   source-data subset (a data.frame) for a single \code{by} group; in pairwise
#'   mode it is called with a 2x2 numeric matrix (see Details). It returns a
#'   single value that is rendered into the cell one of two ways: a
#'   \strong{numeric} (typically a p-value) is formatted with \code{format}, or a
#'   \strong{character} string is passed through \emph{verbatim} -- letting the
#'   function that computes an arbitrary test also supply the finished display,
#'   e.g. a significance flag (\code{"0.031*"}), a ceiling/floor
#'   (\code{">.99"}, \code{"<.0001"}), or a sentinel (\code{"NE"}). Return
#'   \code{NA} (numeric or character) to render a blank.
#' @param format An \code{\link{f_str}} object formatting a \strong{numeric}
#'   return; it is ignored when \code{fn} returns a character string. The f_str
#'   must reference a single variable (any name; the returned scalar is passed
#'   positionally). Defaults to \code{f_str("x.xxx", "p")}.
#' @param label Character string used as the result column's header label. In
#'   pairwise mode it may be a vector with one entry per comparison (or a single
#'   value recycled across comparisons); \code{NULL} generates a default
#'   \code{"<reference> vs <comparison>"} label per comparison. In omnibus mode
#'   defaults to \code{"p-value"}.
#' @param reference Pairwise mode only. Character(1) naming the reference arm
#'   level of the first \code{cols} variable. \code{NULL} (default) uses that
#'   variable's first level at build time.
#' @param comparisons Pairwise mode only. A character vector (or list of single
#'   levels) of arm levels, each compared to \code{reference} (e.g.
#'   \code{c("Low", "High")}). Supplying this switches on pairwise/per-level
#'   mode; \code{NULL} (default) keeps omnibus mode.
#'
#' @return A \code{tplyr_assoc_test} object.
#' @examples
#' # Omnibus
#' at <- assoc_test(
#'   fn = function(.data) fisher.test(table(.data$TRT, .data$RESP))$p.value,
#'   format = f_str("x.xxx", "p"),
#'   label = "p-value [1]"
#' )
#'
#' # Pairwise per-level (count layer): Fisher on an incidence 2x2
#' at2 <- assoc_test(
#'   fn = function(m) fisher.test(m)$p.value,
#'   reference = "Placebo",
#'   comparisons = c("Low", "High"),
#'   format = f_str("x.xxx", "p")
#' )
#' @export
assoc_test <- function(fn, format = f_str("x.xxx", "p"), label = NULL,
                       reference = NULL, comparisons = NULL) {
  if (!is.function(fn)) {
    stop("`fn` must be a function of one argument (the by-group data subset, ",
         "or a 2x2 matrix in pairwise mode)",
         call. = FALSE)
  }
  if (!inherits(format, "tplyr_f_str")) {
    stop("`format` must be an f_str() object", call. = FALSE)
  }
  if (length(format$vars) != 1) {
    stop("`format` must reference exactly one variable (the returned scalar)",
         call. = FALSE)
  }

  pairwise <- !is.null(comparisons)

  if (!pairwise) {
    if (!is.null(reference)) {
      stop("`reference` is only used in pairwise mode (supply `comparisons`)",
           call. = FALSE)
    }
    if (is.null(label)) label <- "p-value"
    if (!is.character(label) || length(label) != 1) {
      stop("`label` must be a single character string in omnibus mode",
           call. = FALSE)
    }
  } else {
    # Normalize comparisons to a character vector (accept a list of single levels)
    if (is.list(comparisons)) {
      comparisons <- map_chr(comparisons, function(x) {
        if (length(x) != 1) {
          stop("each `comparisons` entry must be a single arm level", call. = FALSE)
        }
        as.character(x)
      })
    } else {
      comparisons <- as.character(comparisons)
    }
    if (length(comparisons) == 0) {
      stop("`comparisons` must name at least one arm level", call. = FALSE)
    }
    if (!is.null(reference) &&
        (!is.character(reference) || length(reference) != 1)) {
      stop("`reference` must be a single arm level (character(1)) or NULL",
           call. = FALSE)
    }
    if (!is.null(label)) {
      if (!is.character(label) ||
          !length(label) %in% c(1L, length(comparisons))) {
        stop("`label` must be NULL, a single string, or one string per ",
             "comparison", call. = FALSE)
      }
    }
  }

  structure(
    list(fn = fn, format = format, label = label,
         reference = reference, comparisons = comparisons,
         pairwise = pairwise),
    class = "tplyr_assoc_test"
  )
}

#' @export
print.tplyr_assoc_test <- function(x, ...) {
  if (isTRUE(x$pairwise)) {
    ref <- x$reference %||% "<first level>"
    cat(str_glue("tplyr pairwise association test (reference: \"{ref}\")\n"))
    cat(str_glue("  Comparisons: {str_c(x$comparisons, collapse = ', ')}\n"))
  } else {
    cat(str_glue("tplyr association test: \"{x$label}\"\n"))
  }
  cat(str_glue("  Format: \"{x$format$format_string}\"\n"))
  invisible(x)
}

#' Render an \code{assoc_test} \code{fn} return value for display
#'
#' Turns a single value returned by a caller-supplied \code{fn} into the string
#' shown in the \code{pval} cell:
#' \itemize{
#'   \item a length-1 \strong{numeric} (or logical) is formatted with
#'     \code{config$format}; \code{NA} renders a blank;
#'   \item a length-1 \strong{character} is passed through verbatim (issue #47),
#'     so a caller computing an arbitrary test can also supply the finished
#'     display (significance flags, \code{">.99"}/\code{"<.0001"} ceilings,
#'     \code{"NE"} sentinels, trailing-space alignment); \code{NA_character_}
#'     renders a blank;
#'   \item anything else (wrong length, non-atomic) renders a blank.
#' }
#'
#' @param raw The raw value returned by \code{fn} (already wrapped so errors
#'   arrive as \code{NA}).
#' @param format An \code{\link{f_str}} object used for numeric returns.
#' @return A length-1 character display string.
#' @keywords internal
format_assoc_return <- function(raw, format) {
  if (length(raw) != 1) return("")
  if (is.character(raw)) return(if (is.na(raw)) "" else raw)
  if (is.numeric(raw) || is.logical(raw)) {
    if (is.na(raw)) return("")
    return(apply_formats(format, as.numeric(raw)))
  }
  ""
}

#' Compute the association-test result per by-group
#'
#' Runs \code{config$fn} once per \code{by} group over the source-data subset
#' for that group, and returns the display string keyed by the by variables.
#'
#' @param source_dt data.table of source rows for the layer (after any layer
#'   \code{where}), holding the \code{cols}, \code{by}, and target/row variables.
#' @param by_data_vars Character vector of by data-variable names (may be empty).
#' @param config A \code{tplyr_assoc_test} object.
#'
#' @return A data.table with the \code{by_data_vars} columns (as character) plus
#'   a formatted character column \code{.assoc_p}. When \code{by_data_vars} is
#'   empty, a single-row table with only \code{.assoc_p}.
#' @keywords internal
compute_assoc_test <- function(source_dt, by_data_vars, config) {
  run_one <- function(sub) {
    raw <- tryCatch(config$fn(as.data.frame(sub)), error = function(e) NA_real_)
    format_assoc_return(raw, config$format)
  }

  if (length(by_data_vars) > 0) {
    res <- source_dt[, list(.assoc_p = run_one(.SD)), by = by_data_vars]
    for (bv in by_data_vars) res[, (bv) := as.character(get(bv))]
  } else {
    res <- data.table::data.table(.assoc_p = run_one(source_dt))
  }

  res
}

#' Attach an association-test result column to a wide layer result
#'
#' Adds a \code{pval1} column carrying the formatted per-by-group result, placed
#' on the first output row of each by group (blank elsewhere), with
#' \code{config$label} as its \code{label} attribute.
#'
#' @param wide data.table layer result (with rowlabel/res/ord columns).
#' @param assoc data.table from \code{compute_assoc_test()}.
#' @param by_rl_cols Character vector of the rowlabel columns holding the by
#'   variable values (in by-variable order); empty when the layer has no by.
#' @param by_data_vars Character vector of by data-variable names.
#' @param config A \code{tplyr_assoc_test} object.
#' @keywords internal
merge_assoc_column <- function(wide, assoc, by_rl_cols, by_data_vars, config) {
  wide[, pval1 := ""]

  if (length(by_rl_cols) > 0) {
    # Key each wide row by its by-group (trimmed rowlabel values) and match to
    # the computed results; place the value on the first row of each group.
    wide_key <- do.call(paste, c(lapply(by_rl_cols, function(c) trimws(wide[[c]])),
                                 sep = "\r"))
    assoc_key <- do.call(paste, c(lapply(by_data_vars, function(c) trimws(assoc[[c]])),
                                  sep = "\r"))
    lookup <- setNames(assoc$.assoc_p, assoc_key)
    first <- !duplicated(wide_key)
    wide[first, pval1 := lookup[wide_key[first]]]
    wide[is.na(pval1), pval1 := ""]
  } else {
    # No by variable: a single result on the first row
    if (nrow(wide) > 0) wide[1L, pval1 := assoc$.assoc_p[1]]
  }

  data.table::setattr(wide[["pval1"]], "label", config$label)
  invisible(wide)
}

# =============================================================================
# Pairwise / per-level association test (count layer)
# =============================================================================

#' Resolve the reference arm level for a pairwise association test
#'
#' Returns \code{config$reference} when supplied, otherwise the first level of
#' the first \code{cols} variable at build time (factor level order when the
#' variable is a factor, else first value in appearance order).
#'
#' @param config A \code{tplyr_assoc_test} object.
#' @param dt data.table of source rows for the layer.
#' @param cols Character vector of column variable names from the spec.
#' @return Character(1) reference level.
#' @keywords internal
resolve_assoc_reference <- function(config, dt, cols) {
  if (!is.null(config$reference)) return(as.character(config$reference))
  if (length(cols) == 0) {
    stop("pairwise assoc_test requires at least one column variable (cols)",
         call. = FALSE)
  }
  x <- dt[[cols[1]]]
  lv <- if (is.factor(x)) levels(x) else unique(as.character(x))
  lv[1]
}

#' Compute pairwise per-level association-test p-values from a counts table
#'
#' For each comparison arm and each target-variable level, builds a 2x2
#' contingency matrix from the assembled cell counts and population
#' denominators and calls \code{config$fn} to obtain a scalar p-value. This
#' mirrors \code{compute_risk_diff()} in placement (a value per target level per
#' comparison) but delegates the test to the caller-supplied function.
#'
#' @param counts_long data.table (pre-formatting) with the column variable, any
#'   \code{by} variables, the target variable, and \code{n}/\code{total}
#'   (plus \code{distinct_n}/\code{distinct_total} when distinct counting).
#' @param cols Character vector of column variable names from the spec.
#' @param tv Character string naming the target variable.
#' @param by_data_vars Character vector of by-variable names.
#' @param distinct_by Distinct-by variable name (or NULL); selects the
#'   distinct counts/denominators when non-NULL.
#' @param config A \code{tplyr_assoc_test} object (pairwise mode).
#' @param reference Character(1) resolved reference arm level.
#'
#' @return A data.table with one row per target level per comparison, holding
#'   the row variables, \code{.comp_idx}, and the formatted display string
#'   \code{.disp}. Numeric \code{fn} returns are formatted with
#'   \code{config$format}; a character \code{fn} return is passed through
#'   verbatim (issue #47); \code{NA} and a zero denominator render a blank.
#' @keywords internal
compute_pairwise_assoc <- function(counts_long, cols, tv, by_data_vars,
                                   distinct_by, config, reference) {
  comparisons <- config$comparisons
  if (length(cols) == 0) {
    stop("pairwise assoc_test requires at least one column variable (cols)",
         call. = FALSE)
  }
  col_var <- cols[1]
  n_col     <- if (!is.null(distinct_by)) "distinct_n" else "n"
  total_col <- if (!is.null(distinct_by)) "distinct_total" else "total"

  row_vars <- c(by_data_vars, tv)

  run_one <- function(n_ref, n_cmp, N_ref, N_cmp) {
    if (is.na(n_ref) || is.na(n_cmp) || is.na(N_ref) || is.na(N_cmp) ||
        N_ref == 0 || N_cmp == 0) {
      return("")
    }
    m <- matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)
    raw <- tryCatch(config$fn(m), error = function(e) NA_real_)
    format_assoc_return(raw, config$format)
  }

  ref_dt <- counts_long[get(col_var) == reference,
                        c(row_vars, n_col, total_col), with = FALSE]
  data.table::setnames(ref_dt, c(n_col, total_col), c("n_ref", "N_ref"))

  # A count layer always has a target variable, so row_vars is never empty.
  results <- imap(comparisons, function(cmp_level, ci_idx) {
    cmp_dt <- counts_long[get(col_var) == cmp_level,
                          c(row_vars, n_col, total_col), with = FALSE]
    data.table::setnames(cmp_dt, c(n_col, total_col), c("n_cmp", "N_cmp"))

    paired <- merge(ref_dt, cmp_dt, by = row_vars, all = TRUE)

    disp_vec <- map_chr(seq_len(nrow(paired)), function(r) {
      run_one(paired$n_ref[r], paired$n_cmp[r],
              paired$N_ref[r], paired$N_cmp[r])
    })

    cbind(
      paired[, row_vars, with = FALSE],
      data.table::data.table(.comp_idx = ci_idx, .disp = disp_vec)
    )
  })

  data.table::rbindlist(results, fill = TRUE)
}

#' Attach pairwise per-level association-test columns to a wide layer result
#'
#' Appends one \code{pval<k>} column per comparison to the wide-format output,
#' each carrying the formatted p-value on \emph{every} target-level row (blank
#' for special rows such as Total/Missing), with the per-comparison label as the
#' column's \code{label} attribute.
#'
#' @param wide data.table in wide format (after \code{cast_to_wide()}).
#' @param assoc_data data.table from \code{compute_pairwise_assoc()}.
#' @param config A \code{tplyr_assoc_test} object (pairwise mode).
#' @param tv Character string naming the target variable.
#' @param by_data_vars Character vector of by-variable names.
#' @param by_labels Character vector of by string-labels (non-data by entries).
#' @param reference Character(1) resolved reference arm level.
#' @keywords internal
merge_pairwise_assoc <- function(wide, assoc_data, config, tv, by_data_vars,
                                 by_labels, reference) {
  comparisons <- config$comparisons

  # Resolve per-comparison labels
  labels <- if (is.null(config$label)) {
    map_chr(comparisons, function(cmp) str_c(reference, " vs ", cmp))
  } else if (length(config$label) == 1) {
    rep(config$label, length(comparisons))
  } else {
    config$label
  }

  if (is.null(assoc_data) || nrow(assoc_data) == 0) {
    # Still emit blank columns for a consistent shape
    iwalk(comparisons, function(cmp, ci_idx) {
      pcol <- str_c("pval", ci_idx)
      wide[, (pcol) := ""]
      data.table::setattr(wide[[pcol]], "label", labels[ci_idx])
    })
    return(invisible(wide))
  }

  # The target variable sits in the last rowlabel column; by data vars occupy
  # the rowlabel columns after any by string-labels.
  all_label_cols <- sort(str_subset(names(wide), "^rowlabel\\d+$"))
  if (length(all_label_cols) == 0) return(invisible(wide))
  tv_label_col <- all_label_cols[length(all_label_cols)]

  iwalk(comparisons, function(cmp, ci_idx) {
    pcol <- str_c("pval", ci_idx)
    wide[, (pcol) := ""]

    sub <- assoc_data[.comp_idx == ci_idx]
    if (nrow(sub) > 0) {
      # .disp already carries the formatted (numeric) or verbatim (character)
      # display string from compute_pairwise_assoc().
      wide_join_cols <- tv_label_col
      sub_join_cols <- tv
      if (length(by_data_vars) > 0) {
        bv_wide_cols <- utils::head(all_label_cols, length(by_data_vars))
        bv_sub_cols <- intersect(by_data_vars, names(sub))
        wide_join_cols <- c(bv_wide_cols[seq_along(bv_sub_cols)], wide_join_cols)
        sub_join_cols <- c(bv_sub_cols, sub_join_cols)
      }

      for (k in sub_join_cols) {
        data.table::set(sub, j = k, value = as.character(sub[[k]]))
      }

      on_clause <- setNames(sub_join_cols, wide_join_cols)
      wide[sub, (pcol) := i..disp, on = on_clause]
      wide[is.na(get(pcol)), (pcol) := ""]
    }

    data.table::setattr(wide[[pcol]], "label", labels[ci_idx])
  })

  invisible(wide)
}
