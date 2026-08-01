# =============================================================================
# Deduplicated reporting of failures in user-supplied code
#
# Custom summaries and assoc_test functions run inside tryCatch() so a bad
# expression cannot abort the whole build, and NA renders as a blank cell.
# That contract stays; what changes is that the *reason* is no longer thrown
# away. A partial failure — real numbers everywhere and one blank cell in the
# group where the expression errored — is otherwise indistinguishable from
# "data legitimately missing".
#
# Pairwise assoc_test calls the user function once per level per comparison, so
# failures are collected, deduplicated by (label, message), and reported as one
# warning at the end of the build rather than hundreds during it.
# =============================================================================

user_fn_errors <- new.env(parent = emptyenv())
user_fn_errors$active <- FALSE
user_fn_errors$entries <- list()

#' Record a failure in user-supplied code
#'
#' A no-op outside \code{collect_user_fn_errors()}, so helpers stay callable
#' from tests and from code paths that run outside a build.
#'
#' @param label What failed, e.g. \code{"custom summary 'geo_mean'"}
#' @param cond The condition caught
#' @param group Optional human-readable group identifier, e.g.
#'   \code{"SEX = F, TRT01P = Placebo"}
#'
#' @return Invisible NULL, called for its side effect
#' @keywords internal
record_user_fn_error <- function(label, cond, group = NULL) {
  if (!isTRUE(user_fn_errors$active)) return(invisible(NULL))

  msg <- conditionMessage(cond)
  key <- str_c(label, "\r", msg)
  entry <- user_fn_errors$entries[[key]]

  if (is.null(entry)) {
    user_fn_errors$entries[[key]] <- list(
      label = label, msg = msg, group = group, count = 1L
    )
  } else {
    entry$count <- entry$count + 1L
    user_fn_errors$entries[[key]] <- entry
  }

  invisible(NULL)
}

#' Evaluate a build, reporting any user-code failures as one warning
#'
#' @param expr Expression to evaluate
#' @return The value of \code{expr}
#' @keywords internal
collect_user_fn_errors <- function(expr) {
  prev_active <- user_fn_errors$active
  prev_entries <- user_fn_errors$entries
  user_fn_errors$active <- TRUE
  user_fn_errors$entries <- list()

  on.exit({
    pending <- user_fn_errors$entries
    user_fn_errors$active <- prev_active
    user_fn_errors$entries <- prev_entries
    if (length(pending) > 0) warn_user_fn_errors(pending)
  }, add = TRUE)

  expr
}

#' Emit the collected user-code failures as a single warning
#'
#' @param entries List of entries recorded by \code{record_user_fn_error()}
#' @return Invisible NULL
#' @keywords internal
warn_user_fn_errors <- function(entries) {
  lines <- map_chr(entries, function(e) {
    where <- if (!is.null(e$group) && nzchar(e$group)) {
      str_c(" at ", e$group)
    } else {
      ""
    }
    more <- if (e$count > 1L) {
      str_c(" (and ", e$count - 1L, " more group",
            if (e$count > 2L) "s" else "", ")")
    } else {
      ""
    }
    str_c("  - ", e$label, where, more, ": ", e$msg)
  })

  warning("User-supplied code failed during the build; affected cells are ",
          "blank.\n", str_c(lines, collapse = "\n"), call. = FALSE)

  invisible(NULL)
}

#' Render a data.table `.BY` group as a human-readable label
#'
#' @param by_values Named list of group values (data.table's \code{.BY})
#' @return Single string, or NULL when there is no grouping
#' @keywords internal
format_group_label <- function(by_values) {
  if (length(by_values) == 0) return(NULL)
  str_c(str_c(names(by_values), " = ",
              map_chr(by_values, function(v) as.character(v)[1])),
        collapse = ", ")
}
