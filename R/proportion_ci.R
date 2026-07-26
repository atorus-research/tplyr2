#' Confidence interval for a single binomial proportion
#'
#' Vectorized computation of a two-sided confidence interval for a single
#' proportion \code{x / n}, using one of five standard methods. All methods are
#' computed in closed form or via \code{\link[stats]{qbeta}} (no per-row
#' \code{binom.test} loop), so the function scales to a full count table's worth
#' of cells at once.
#'
#' The methods and their references:
#' \describe{
#'   \item{\code{clopper_pearson}}{Exact interval based on the beta
#'     distribution. Matches \code{stats::binom.test()} and SAS
#'     \code{PROC FREQ ... EXACT} (the clinical convention). This is the
#'     default.}
#'   \item{\code{wilson}}{Wilson score interval without continuity correction.
#'     Matches \code{stats::prop.test(correct = FALSE)}.}
#'   \item{\code{wald}}{Normal-approximation ("simple asymptotic") interval,
#'     clamped to \code{[0, 1]}.}
#'   \item{\code{agresti_coull}}{Adds \code{z^2/2} pseudo-successes and
#'     pseudo-failures, then applies the Wald interval to the adjusted counts.}
#'   \item{\code{jeffreys}}{Bayesian interval using the Jeffreys
#'     \code{Beta(0.5, 0.5)} prior, with the standard boundary adjustments at
#'     \code{x == 0} and \code{x == n}.}
#' }
#'
#' Edge cases: when \code{n == 0} (or either input is \code{NA}) both bounds are
#' \code{NA}; when \code{x == 0} the lower bound is exactly \code{0}; when
#' \code{x == n} the upper bound is exactly \code{1}.
#'
#' @param x Numeric vector of event counts (numerators).
#' @param n Numeric vector of trial counts (denominators). Recycled against
#'   \code{x} following the usual R rules.
#' @param method One of \code{"clopper_pearson"} (default), \code{"wilson"},
#'   \code{"wald"}, \code{"agresti_coull"}, or \code{"jeffreys"}.
#' @param level Numeric coverage probability (default \code{0.95}).
#'
#' @return A \code{data.table} with two columns, \code{lower} and \code{upper},
#'   giving the interval bounds as proportions in \code{[0, 1]} (multiply by 100
#'   for the percentage scale).
#'
#' @examples
#' proportion_ci(c(0, 12, 40), c(40, 40, 40), method = "clopper_pearson")
#'
#' @export
proportion_ci <- function(x, n,
                          method = c("clopper_pearson", "wilson", "wald",
                                     "agresti_coull", "jeffreys"),
                          level = 0.95) {
  method <- match.arg(method)

  x <- as.double(x)
  n <- as.double(n)

  # Recycle to a common length
  len <- max(length(x), length(n))
  if (len == 0) {
    return(data.table::data.table(lower = numeric(0), upper = numeric(0)))
  }
  x <- rep_len(x, len)
  n <- rep_len(n, len)

  alpha <- 1 - level
  z <- stats::qnorm(1 - alpha / 2)

  lower <- rep(NA_real_, len)
  upper <- rep(NA_real_, len)

  valid <- !is.na(x) & !is.na(n) & n > 0
  if (!any(valid)) {
    return(data.table::data.table(lower = lower, upper = upper))
  }

  xv <- x[valid]
  nv <- n[valid]
  p <- xv / nv

  if (method == "clopper_pearson") {
    lo <- stats::qbeta(alpha / 2, xv, nv - xv + 1)
    up <- stats::qbeta(1 - alpha / 2, xv + 1, nv - xv)
    lo[xv == 0] <- 0
    up[xv == nv] <- 1
  } else if (method == "jeffreys") {
    lo <- stats::qbeta(alpha / 2, xv + 0.5, nv - xv + 0.5)
    up <- stats::qbeta(1 - alpha / 2, xv + 0.5, nv - xv + 0.5)
    lo[xv == 0] <- 0
    up[xv == nv] <- 1
  } else if (method == "wilson") {
    denom <- 1 + z^2 / nv
    center <- (p + z^2 / (2 * nv)) / denom
    half <- (z / denom) * sqrt(p * (1 - p) / nv + z^2 / (4 * nv^2))
    lo <- center - half
    up <- center + half
  } else if (method == "wald") {
    se <- sqrt(p * (1 - p) / nv)
    lo <- p - z * se
    up <- p + z * se
  } else if (method == "agresti_coull") {
    n_tilde <- nv + z^2
    p_tilde <- (xv + z^2 / 2) / n_tilde
    se <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
    lo <- p_tilde - z * se
    up <- p_tilde + z * se
  }

  # Clamp to [0, 1]
  lo <- pmin(pmax(lo, 0), 1)
  up <- pmin(pmax(up, 0), 1)

  lower[valid] <- lo
  upper[valid] <- up

  data.table::data.table(lower = lower, upper = upper)
}

# The four count-layer format keywords backed by proportion_ci()
.ci_stat_keywords <- c("ci_lower", "ci_upper",
                       "distinct_ci_lower", "distinct_ci_upper")

#' Does a count layer's formats reference any CI keyword?
#'
#' Scans the layer's `format_strings` and `stat_columns` f_str `$vars` for one
#' of the four confidence-interval keywords, so the (comparatively expensive)
#' CI computation can be skipped entirely for layers that don't display one.
#'
#' @param settings A tplyr_layer_settings object
#' @return Logical scalar
#' @keywords internal
layer_uses_ci <- function(settings) {
  fmts <- c(settings$format_strings, settings$stat_columns)
  if (is.null(fmts) || length(fmts) == 0) return(FALSE)
  any(map_lgl(fmts, function(f) {
    inherits(f, "tplyr_f_str") && any(f$vars %in% .ci_stat_keywords)
  }))
}

#' Attach single-proportion CI columns to a long count table
#'
#' Adds `ci_lower`/`ci_upper` (from `n`/`total`) and, when the distinct columns
#' are present, `distinct_ci_lower`/`distinct_ci_upper` (from
#' `distinct_n`/`distinct_total`). Bounds are stored on the **percentage scale**
#' (proportion times 100) to match the `pct`/`distinct_pct` statistics.
#'
#' @param dt A long count data.table (or NULL, a no-op)
#' @param settings A tplyr_layer_settings object supplying `ci_method`/`ci_level`
#' @return `dt`, modified in place
#' @keywords internal
add_count_ci <- function(dt, settings) {
  if (is.null(dt)) return(invisible(NULL))
  method <- settings$ci_method %||% "clopper_pearson"
  level <- settings$ci_level %||% 0.95

  if (all(c("n", "total") %in% names(dt))) {
    ci <- proportion_ci(dt[["n"]], dt[["total"]], method = method, level = level)
    dt[, ci_lower := ci$lower * 100]
    dt[, ci_upper := ci$upper * 100]
  }

  if (all(c("distinct_n", "distinct_total") %in% names(dt))) {
    ci <- proportion_ci(dt[["distinct_n"]], dt[["distinct_total"]],
                        method = method, level = level)
    dt[, distinct_ci_lower := ci$lower * 100]
    dt[, distinct_ci_upper := ci$upper * 100]
  }

  invisible(dt)
}
