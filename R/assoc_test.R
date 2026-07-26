#' Association-test column for count and shift layers
#'
#' Configures an omnibus association test that runs once per \code{by} group,
#' \emph{across} the treatment columns, and lands its formatted result as a
#' single trailing column beside the n(\%) block. The supplied function receives
#' the raw source-data subset for the \code{by} group (all \code{cols} levels
#' and all target/row levels), so a caller can tabulate and test naturally
#' (e.g. \code{fisher.test(table(.data$TRT, .data$RESP))} or
#' \code{coin::cmh_test(...)}).
#'
#' Attach it to a count or shift layer via
#' \code{layer_settings(assoc_test = assoc_test(...))}. When the layer has no
#' \code{by} variable the test runs once over the whole layer; otherwise once
#' per \code{by} group, with the value placed on that group's first output row.
#'
#' @param fn A function of one argument. It is called with the source-data
#'   subset (a data.frame) for a single \code{by} group and must return a single
#'   numeric value (typically a p-value). Return \code{NA} to render a blank.
#' @param format An \code{\link{f_str}} object formatting the returned value.
#'   The f_str must reference a single variable (any name; the returned scalar
#'   is passed positionally). Defaults to \code{f_str("x.xxx", "p")}.
#' @param label Character string used as the result column's header label.
#'   Defaults to \code{"p-value"}.
#'
#' @return A \code{tplyr_assoc_test} object.
#' @examples
#' at <- assoc_test(
#'   fn = function(.data) fisher.test(table(.data$TRT, .data$RESP))$p.value,
#'   format = f_str("x.xxx", "p"),
#'   label = "p-value [1]"
#' )
#' @export
assoc_test <- function(fn, format = f_str("x.xxx", "p"), label = "p-value") {
  if (!is.function(fn)) {
    stop("`fn` must be a function of one argument (the by-group data subset)",
         call. = FALSE)
  }
  if (!inherits(format, "tplyr_f_str")) {
    stop("`format` must be an f_str() object", call. = FALSE)
  }
  if (length(format$vars) != 1) {
    stop("`format` must reference exactly one variable (the returned scalar)",
         call. = FALSE)
  }
  structure(
    list(fn = fn, format = format, label = label),
    class = "tplyr_assoc_test"
  )
}

#' @export
print.tplyr_assoc_test <- function(x, ...) {
  cat(str_glue("tplyr association test: \"{x$label}\"\n"))
  cat(str_glue("  Format: \"{x$format$format_string}\"\n"))
  invisible(x)
}

#' Compute the association-test result per by-group
#'
#' Runs \code{config$fn} once per \code{by} group over the source-data subset
#' for that group, and returns the formatted scalar keyed by the by variables.
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
    p <- tryCatch(config$fn(as.data.frame(sub)), error = function(e) NA_real_)
    if (length(p) != 1 || !is.numeric(p)) p <- NA_real_
    as.numeric(p)
  }

  if (length(by_data_vars) > 0) {
    res <- source_dt[, list(.p = run_one(.SD)), by = by_data_vars]
    for (bv in by_data_vars) res[, (bv) := as.character(get(bv))]
  } else {
    res <- data.table::data.table(.p = run_one(source_dt))
  }

  res[, .assoc_p := apply_formats(config$format, .p)]
  # Blank out NA results (apply_formats renders NA as spaces)
  res[is.na(.p), .assoc_p := ""]
  res[, .p := NULL]
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
