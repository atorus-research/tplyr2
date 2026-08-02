#' Association-test column(s) for count, shift, and desc layers
#'
#' Configures an association test and lands its formatted result beside the
#' n(\%) (or statistic) block. Two modes are supported, selected by whether
#' \code{comparisons} is supplied:
#'
#' \strong{Omnibus mode} (\code{comparisons = NULL}, the default). Runs
#' \code{fn} once per \code{by} group, \emph{across} the treatment columns, and
#' lands its result as a single trailing column. The supplied function receives
#' the raw source-data subset for the \code{by} group (all \code{cols} levels
#' and all target/row levels), so a caller can tabulate and test naturally
#' (e.g. \code{fisher.test(table(.data$TRT, .data$RESP))} or
#' \code{coin::cmh_test(...)}). When the layer has no \code{by} variable the
#' test runs once over the whole layer; otherwise once per \code{by} group, with
#' the value placed on that group's first output row. This mode works on
#' \strong{count}, \strong{shift}, and \strong{desc} layers -- on a desc layer
#' it is the natural home for a continuous-variable comparison across arms
#' (ANOVA / Kruskal-Wallis / t-test), e.g.
#' \code{fn = function(.data) anova(lm(AGE ~ TRT, .data))[["Pr(>F)"]][1]}, with
#' one p-value per \code{by} group on that group's first statistic row.
#'
#' \strong{Pairwise / per-level mode} (\code{comparisons} non-\code{NULL}).
#' Count layers only. Emits one \code{pval} column per comparison, each
#' comparing an arm level of the first \code{cols} variable to \code{reference},
#' with a value on \emph{every} target-level row (like \code{risk_diff}'s
#' \code{rdiff} columns). On a \strong{nested} count layer it emits a value on
#' every row of every level -- each inner (e.g. preferred-term) row and each
#' outer (e.g. system-organ-class subtotal) row, each row's 2x2 built from that
#' row's own counts -- and, when the layer has a total row, on the grand-total
#' row too (see \code{total_row}). Here \code{fn} receives, for one (row,
#' comparison) pair, a 2x2 contingency \strong{matrix}
#' \code{matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)} --
#' rows are (reference, comparison) arm, columns are (event, no event) -- where
#' \code{n} is the cell count and \code{N} the population denominator for that
#' arm. When the layer sets \code{distinct_by}, the distinct counts/denominators
#' are used. \code{fn} returns either a numeric value (a scalar, or a vector of
#' several statistics matching a multi-variable \code{format}) or a verbatim
#' character display string (\code{NA} renders a blank).
#'
#' Attach it to a layer via
#' \code{layer_settings(assoc_test = assoc_test(...))}.
#'
#' @param fn A function of one argument. In omnibus mode it is called with the
#'   source-data subset (a data.frame) for a single \code{by} group; in pairwise
#'   mode it is called with a 2x2 numeric matrix (see Details). Its return is
#'   rendered into the cell one of two ways: a \strong{numeric} value (or a
#'   numeric vector matching the number of variables in \code{format}) is
#'   formatted with \code{format} -- a scalar p-value, or several statistics such
#'   as an odds ratio with its confidence interval mapped positionally onto a
#'   multi-variable f_str; or a \strong{character} string is passed through
#'   \emph{verbatim}, letting the function that computes an arbitrary test also
#'   supply the finished display (a significance flag \code{"0.031*"}, a
#'   ceiling/floor \code{">.99"}/\code{"<.0001"}, a sentinel \code{"NE"}). Return
#'   \code{NA} (numeric or character) to render a blank.
#' @param format An \code{\link{f_str}} object formatting a \strong{numeric}
#'   return; it is ignored when \code{fn} returns a character string. Its
#'   variable count sets how many values \code{fn} must return: one variable for
#'   a scalar (e.g. \code{f_str("x.xxx", "p")}, the default), or several for a
#'   tuple (e.g. \code{f_str("xx.xx (xx.xx, xx.xx)", "or", "lo", "hi")}). The
#'   returned values are passed positionally, so the variable names are free.
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
#' @param total_row Pairwise mode only. Logical(1); when the layer also emits a
#'   total row (\code{layer_settings(total_row = TRUE)}), \code{TRUE} (default)
#'   computes the pairwise p-value on that row too -- for a nested AE layer the
#'   grand-total ("any event anywhere") 2x2 -- while \code{FALSE} leaves it
#'   blank. Missing rows are always left blank.
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
#'
#' # Omnibus on a desc layer: continuous comparison across arms (ANOVA)
#' at3 <- assoc_test(
#'   fn = function(.data) anova(lm(AGE ~ TRT, .data))[["Pr(>F)"]][1],
#'   format = f_str("x.xxx", "p")
#' )
#'
#' # Multiple statistics in one cell: odds ratio with a confidence interval
#' at4 <- assoc_test(
#'   fn = function(m) {
#'     ft <- fisher.test(m)
#'     c(ft$estimate, ft$conf.int[1], ft$conf.int[2])
#'   },
#'   reference = "Placebo",
#'   comparisons = c("Low", "High"),
#'   format = f_str("xx.xx (xx.xx, xx.xx)", "or", "lo", "hi"),
#'   label = "OR (95% CI)"
#' )
#' @export
assoc_test <- function(fn, format = f_str("x.xxx", "p"), label = NULL,
                       reference = NULL, comparisons = NULL, total_row = TRUE) {
  if (!is.function(fn)) {
    stop("`fn` must be a function of one argument (the by-group data subset, ",
         "or a 2x2 matrix in pairwise mode)",
         call. = FALSE)
  }
  if (!inherits(format, "tplyr_f_str")) {
    stop("`format` must be an f_str() object", call. = FALSE)
  }
  if (length(format$vars) < 1) {
    stop("`format` must reference at least one variable", call. = FALSE)
  }
  if (!is.logical(total_row) || length(total_row) != 1 || is.na(total_row)) {
    stop("`total_row` must be a single logical (TRUE/FALSE)", call. = FALSE)
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
         pairwise = pairwise, total_row = isTRUE(total_row)),
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
#' Turns the value returned by a caller-supplied \code{fn} into the string shown
#' in the \code{pval} cell:
#' \itemize{
#'   \item a \strong{numeric} (or logical) vector whose length equals the number
#'     of variables in \code{format} is mapped positionally onto the format and
#'     rendered into one cell -- a scalar with a one-variable format (a p-value),
#'     or several values with a multi-variable format (issue #60), e.g. an odds
#'     ratio with a confidence interval. An all-\code{NA} return (or an arity
#'     that does not match the format) renders a blank; a single \code{NA} field
#'     within a longer return blanks just that field via the \code{f_str}
#'     grammar;
#'   \item a length-1 \strong{character} is passed through verbatim (issue #47),
#'     so a caller computing an arbitrary test can also supply the finished
#'     display (significance flags, \code{">.99"}/\code{"<.0001"} ceilings,
#'     \code{"NE"} sentinels, trailing-space alignment); \code{NA_character_}
#'     renders a blank;
#'   \item anything else (non-atomic, mismatched length) renders a blank.
#' }
#'
#' @param raw The raw value returned by \code{fn} (already wrapped so errors
#'   arrive as \code{NA}).
#' @param format An \code{\link{f_str}} object; its variable count determines how
#'   many values a numeric return must supply.
#' @param label What produced \code{raw}, used when reporting a shape mismatch.
#' @param group Optional group identifier used when reporting a shape mismatch.
#' @return A length-1 character display string.
#' @keywords internal
format_assoc_return <- function(raw, format, label = "assoc_test fn",
                                group = NULL) {
  # Character escape hatch: a single string is the whole cell, verbatim.
  if (is.character(raw) && length(raw) == 1) {
    return(if (is.na(raw)) "" else raw)
  }
  # Numeric/logical: map positionally onto the format's variables (one value per
  # variable). One value + one-variable format is the classic scalar p-value.
  if (is.numeric(raw) || is.logical(raw)) {
    n_vars <- length(format$vars)
    if (length(raw) != n_vars) {
      # A caller bug, not a statistical outcome — blanking it silently reads
      # as "the test didn't apply here".
      report_assoc_shape_mismatch(
        label, group,
        str_c("returned ", length(raw), " value",
              if (length(raw) == 1) "" else "s", " but the format declares ",
              n_vars, " (", str_c(format$vars, collapse = ", "), ")"))
      return("")
    }
    if (all(is.na(raw))) return("")         # nothing to show -> blank
    return(do.call(apply_formats, c(list(format), as.list(as.numeric(raw)))))
  }

  if (!is.null(raw) && !all(is.na(raw))) {
    report_assoc_shape_mismatch(
      label, group,
      str_c("returned an unsupported type (", class(raw)[1],
            "); expected a numeric vector or a length-1 character"))
  }
  ""
}

#' Record an assoc_test return-shape mismatch
#' @keywords internal
report_assoc_shape_mismatch <- function(label, group, detail) {
  record_user_fn_error(label, simpleCondition(detail), group)
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
  # total_group()/custom_group() duplicate rows are a display construct for the
  # count columns; they must not enter a statistical test or they double-count
  # every subject and silently return a wrong p-value (#53). Drop them (and the
  # internal marker) so `fn` sees only the real observations.
  if (".tplyr_synthetic" %in% names(source_dt)) {
    keep <- setdiff(names(source_dt), ".tplyr_synthetic")
    source_dt <- source_dt[.tplyr_synthetic %in% c(FALSE, NA), keep, with = FALSE]
    # Assigning the total/custom label to a factor column left its level behind;
    # after dropping the synthetic rows those levels are globally unused, so drop
    # them or a test that tabulates on the factor sees a phantom all-zero level
    # (a chi-square then returns NaN) (#53).
    fct_cols <- names(source_dt)[map_lgl(source_dt, is.factor)]
    for (fc in fct_cols) {
      data.table::set(source_dt, j = fc, value = droplevels(source_dt[[fc]]))
    }
  }

  run_one <- function(sub, grp = NULL) {
    raw <- tryCatch(config$fn(as.data.frame(sub)), error = function(e) {
      record_user_fn_error("assoc_test fn", e, grp)
      NA_real_
    })
    format_assoc_return(raw, config$format, label = "assoc_test fn", group = grp)
  }

  if (length(by_data_vars) > 0) {
    res <- source_dt[, list(.assoc_p = run_one(.SD, format_group_label(.BY))),
                     by = by_data_vars]
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

  # The omnibus value belongs on the first row of the layer's FINAL display
  # order, but merge runs before the top-level sort by the ord* columns. Placing
  # it on the current (dcast) first row would strand it on an arbitrary category
  # once the rows are reordered (e.g. order_count_method = "byfactor"), so derive
  # the display order here from the ord* columns (#54).
  ord_cols <- sort_by_numeric_suffix(str_subset(names(wide), "^ord\\d+$"))
  disp_rank <- if (length(ord_cols) > 0 && nrow(wide) > 0) {
    do.call(order, map(ord_cols, function(oc) wide[[oc]]))
  } else {
    seq_len(nrow(wide))
  }

  if (length(by_rl_cols) > 0) {
    # Key each wide row by its by-group (trimmed rowlabel values) and match to
    # the computed results; place the value on the first row of each group in
    # display order.
    wide_key <- do.call(paste, c(map(by_rl_cols, function(c) trimws(wide[[c]])),
                                 sep = "\r"))
    assoc_key <- do.call(paste, c(map(by_data_vars, function(c) trimws(assoc[[c]])),
                                  sep = "\r"))
    lookup <- setNames(assoc$.assoc_p, assoc_key)
    # First appearance of each by-group key when rows are walked in display order
    first_in_disp <- disp_rank[!duplicated(wide_key[disp_rank])]
    wide[first_in_disp, pval1 := lookup[wide_key[first_in_disp]]]
    wide[is.na(pval1), pval1 := ""]
  } else {
    # No by variable: a single result on the first row in display order
    if (nrow(wide) > 0) wide[disp_rank[1], pval1 := assoc$.assoc_p[1]]
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

#' Build one pairwise 2x2 and render its display string
#'
#' Shared by the single-level and nested pairwise paths. Builds
#' \code{matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)}, calls
#' \code{config$fn} on it, and renders the return via
#' \code{\link{format_assoc_return}}. A missing count/denominator or a zero
#' denominator renders a blank (no test).
#'
#' @param n_ref,n_cmp Event counts for the reference and comparison arm.
#' @param N_ref,N_cmp Population denominators for the reference and comparison arm.
#' @param config A \code{tplyr_assoc_test} object (pairwise mode).
#' @param group Optional group identifier used when reporting a failure of
#'   \code{config$fn}.
#' @return A length-1 character display string.
#' @keywords internal
pairwise_cell_disp <- function(n_ref, n_cmp, N_ref, N_cmp, config,
                               group = NULL) {
  if (is.na(n_ref) || is.na(n_cmp) || is.na(N_ref) || is.na(N_cmp) ||
      N_ref == 0 || N_cmp == 0) {
    return("")
  }
  m <- matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)
  raw <- tryCatch(config$fn(m), error = function(e) {
    record_user_fn_error("pairwise assoc_test fn", e, group)
    NA_real_
  })
  format_assoc_return(raw, config$format, label = "pairwise assoc_test fn",
                      group = group)
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

  run_one <- function(n_ref, n_cmp, N_ref, N_cmp, grp = NULL) {
    pairwise_cell_disp(n_ref, n_cmp, N_ref, N_cmp, config, group = grp)
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
              paired$N_ref[r], paired$N_cmp[r],
              format_group_label(as.list(paired[r, row_vars, with = FALSE])))
    })

    cbind(
      paired[, row_vars, with = FALSE],
      data.table::data.table(.comp_idx = ci_idx, .disp = disp_vec)
    )
  })

  data.table::rbindlist(results, fill = TRUE)
}

#' Resolve per-comparison p-value column labels
#'
#' Default is `"<reference> vs <comparison>"`; a single configured label recycles
#' across comparisons; a vector is used as-is.
#' @keywords internal
resolve_pairwise_labels <- function(config, reference) {
  comparisons <- config$comparisons
  if (is.null(config$label)) {
    map_chr(comparisons, function(cmp) str_c(reference, " vs ", cmp))
  } else if (length(config$label) == 1) {
    rep(config$label, length(comparisons))
  } else {
    config$label
  }
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

  labels <- resolve_pairwise_labels(config, reference)

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
  join_cols <- resolve_rowlabel_join_cols(wide, by_labels, by_data_vars)
  if (is.null(join_cols)) return(invisible(wide))
  tv_label_col <- join_cols$tv_col

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
        bv_sub_cols <- intersect(by_data_vars, names(sub))
        bv_wide_cols <- join_cols$by_cols[match(bv_sub_cols, by_data_vars)]
        wide_join_cols <- c(bv_wide_cols, wide_join_cols)
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

# =============================================================================
# Pairwise / per-level association test on a NESTED count layer (#49)
# =============================================================================

#' Compute pairwise per-level association-test p-values for a nested layer
#'
#' Like \code{compute_pairwise_assoc()} but keyed directly by the assembled
#' \code{rowlabel*} columns rather than a single target variable, so it works at
#' every nesting level at once: each inner (e.g. preferred-term) row and each
#' outer (e.g. system-organ-class subtotal) row is one \code{rowlabel} tuple,
#' and its 2x2 is built from that row's own reference/comparison counts and
#' population denominators. The same helper computes the grand-total row's
#' p-value when passed the total-row table (a single \code{rowlabel} tuple).
#'
#' @param long data.table holding the column variable, the assembled
#'   \code{rowlabel*} columns, and the raw \code{n}/\code{total}
#'   (or \code{distinct_n}/\code{distinct_total}) statistics -- the nested
#'   \code{combined} table (category rows) or a total-row table.
#' @param cols Character vector of column variable names from the spec.
#' @param row_label_cols Character vector of the \code{rowlabel*} column names
#'   that jointly identify an output row.
#' @param distinct_by Distinct-by variable name (or NULL); selects the distinct
#'   counts/denominators when non-NULL.
#' @param config A \code{tplyr_assoc_test} object (pairwise mode).
#' @param reference Character(1) resolved reference arm level.
#' @param arm_n Named numeric of population arm sizes (arm level -> N), used to
#'   back-fill the 2x2 denominator for an arm that has no events on a row (or no
#'   events at all). Without it, a zero-event reference or comparison arm would
#'   have a missing denominator and blank the test; with it, an empty arm still
#'   yields a valid \code{0-vs-k} test (issue #49, sparse-table fix).
#'
#' @return A data.table with the \code{row_label_cols} (as character),
#'   \code{.comp_idx}, and the display string \code{.disp}; one row per output
#'   row per comparison.
#' @keywords internal
compute_pairwise_assoc_nested <- function(long, cols, row_label_cols,
                                          distinct_by, config, reference,
                                          arm_n = NULL) {
  comparisons <- config$comparisons
  if (length(cols) == 0) {
    stop("pairwise assoc_test requires at least one column variable (cols)",
         call. = FALSE)
  }
  if (is.null(long) || nrow(long) == 0) return(NULL)

  col_var   <- cols[1]
  n_col     <- if (!is.null(distinct_by)) "distinct_n" else "n"
  total_col <- if (!is.null(distinct_by)) "distinct_total" else "total"

  # Universe of output rows. Every arm is compared on every row, so an arm with
  # no events on a row (or absent from the layer entirely) still contributes an
  # n = 0 cell rather than dropping the whole comparison.
  row_keys <- unique(long[, row_label_cols, with = FALSE])
  for (k in row_label_cols) {
    data.table::set(row_keys, j = k, value = as.character(row_keys[[k]]))
  }

  arm_denom <- function(arm) {
    if (is.null(arm_n)) return(NA_real_)
    v <- arm_n[[as.character(arm)]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }

  # An arm's counts, completed to the full row universe: n zero-filled, and the
  # denominator back-filled from the population arm N wherever the layer left it
  # missing (a zero-event arm never reaches denominator completion upstream).
  arm_counts <- function(arm) {
    sub <- long[get(col_var) == arm, c(row_label_cols, n_col, total_col),
                with = FALSE]
    for (k in row_label_cols) {
      data.table::set(sub, j = k, value = as.character(sub[[k]]))
    }
    m <- merge(row_keys, sub, by = row_label_cols, all.x = TRUE)
    data.table::set(m, which(is.na(m[[n_col]])), n_col, 0)
    Narm <- arm_denom(arm)
    if (!is.na(Narm)) {
      data.table::set(m, which(is.na(m[[total_col]])), total_col, Narm)
    }
    m
  }

  ref_dt <- arm_counts(reference)
  data.table::setnames(ref_dt, c(n_col, total_col), c("n_ref", "N_ref"))

  results <- imap(comparisons, function(cmp_level, ci_idx) {
    cmp_dt <- arm_counts(cmp_level)
    data.table::setnames(cmp_dt, c(n_col, total_col), c("n_cmp", "N_cmp"))

    paired <- merge(ref_dt, cmp_dt, by = row_label_cols, all = TRUE)

    disp_vec <- map_chr(seq_len(nrow(paired)), function(r) {
      pairwise_cell_disp(
        paired$n_ref[r], paired$n_cmp[r], paired$N_ref[r], paired$N_cmp[r],
        config,
        group = format_group_label(as.list(paired[r, row_label_cols, with = FALSE])))
    })

    cbind(
      paired[, row_label_cols, with = FALSE],
      data.table::data.table(.comp_idx = ci_idx, .disp = disp_vec)
    )
  })

  data.table::rbindlist(results, fill = TRUE)
}

#' Attach pairwise association-test columns to a nested wide layer result
#'
#' Places each comparison's display string on every matching output row by an
#' exact join on the \code{rowlabel*} columns (which uniquely identify a wide
#' row across all nesting levels). Rows with no computed value -- special rows
#' such as Missing, or a Total row when \code{total_row = FALSE} -- stay blank.
#'
#' @param wide data.table in wide format (after \code{cast_to_wide()}).
#' @param assoc_data data.table from \code{compute_pairwise_assoc_nested()}.
#' @param config A \code{tplyr_assoc_test} object (pairwise mode).
#' @param row_label_cols Character vector of the \code{rowlabel*} column names.
#' @param reference Character(1) resolved reference arm level.
#' @keywords internal
merge_pairwise_assoc_nested <- function(wide, assoc_data, config, row_label_cols,
                                        reference) {
  comparisons <- config$comparisons

  labels <- resolve_pairwise_labels(config, reference)

  join_cols <- intersect(row_label_cols, names(wide))

  iwalk(comparisons, function(cmp, ci_idx) {
    pcol <- str_c("pval", ci_idx)
    wide[, (pcol) := ""]

    if (!is.null(assoc_data) && nrow(assoc_data) > 0 && length(join_cols) > 0) {
      sub <- assoc_data[.comp_idx == ci_idx]
      if (nrow(sub) > 0) {
        wide[sub, (pcol) := i..disp, on = join_cols]
        wide[is.na(get(pcol)), (pcol) := ""]
      }
    }

    data.table::setattr(wide[[pcol]], "label", labels[ci_idx])
  })

  invisible(wide)
}
