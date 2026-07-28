# Association-test column(s) for count, shift, and desc layers

Configures an association test and lands its formatted result beside the
n(\\ `comparisons` is supplied:

## Usage

``` r
assoc_test(
  fn,
  format = f_str("x.xxx", "p"),
  label = NULL,
  reference = NULL,
  comparisons = NULL,
  total_row = TRUE
)
```

## Arguments

- fn:

  A function of one argument. In omnibus mode it is called with the
  source-data subset (a data.frame) for a single `by` group; in pairwise
  mode it is called with a 2x2 numeric matrix (see Details). Its return
  is rendered into the cell one of two ways: a **numeric** value (or a
  numeric vector matching the number of variables in `format`) is
  formatted with `format` – a scalar p-value, or several statistics such
  as an odds ratio with its confidence interval mapped positionally onto
  a multi-variable f_str; or a **character** string is passed through
  *verbatim*, letting the function that computes an arbitrary test also
  supply the finished display (a significance flag `"0.031*"`, a
  ceiling/floor `">.99"`/`"<.0001"`, a sentinel `"NE"`). Return `NA`
  (numeric or character) to render a blank.

- format:

  An [`f_str`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  object formatting a **numeric** return; it is ignored when `fn`
  returns a character string. Its variable count sets how many values
  `fn` must return: one variable for a scalar (e.g.
  `f_str("x.xxx", "p")`, the default), or several for a tuple (e.g.
  `f_str("xx.xx (xx.xx, xx.xx)", "or", "lo", "hi")`). The returned
  values are passed positionally, so the variable names are free.

- label:

  Character string used as the result column's header label. In pairwise
  mode it may be a vector with one entry per comparison (or a single
  value recycled across comparisons); `NULL` generates a default
  `"<reference> vs <comparison>"` label per comparison. In omnibus mode
  defaults to `"p-value"`.

- reference:

  Pairwise mode only. Character(1) naming the reference arm level of the
  first `cols` variable. `NULL` (default) uses that variable's first
  level at build time.

- comparisons:

  Pairwise mode only. A character vector (or list of single levels) of
  arm levels, each compared to `reference` (e.g. `c("Low", "High")`).
  Supplying this switches on pairwise/per-level mode; `NULL` (default)
  keeps omnibus mode.

- total_row:

  Pairwise mode only. Logical(1); when the layer also emits a total row
  (`layer_settings(total_row = TRUE)`), `TRUE` (default) computes the
  pairwise p-value on that row too – for a nested AE layer the
  grand-total ("any event anywhere") 2x2 – while `FALSE` leaves it
  blank. Missing rows are always left blank.

## Value

A `tplyr_assoc_test` object.

## Details

**Omnibus mode** (`comparisons = NULL`, the default). Runs `fn` once per
`by` group, *across* the treatment columns, and lands its result as a
single trailing column. The supplied function receives the raw
source-data subset for the `by` group (all `cols` levels and all
target/row levels), so a caller can tabulate and test naturally (e.g.
`fisher.test(table(.data$TRT, .data$RESP))` or `coin::cmh_test(...)`).
When the layer has no `by` variable the test runs once over the whole
layer; otherwise once per `by` group, with the value placed on that
group's first output row. This mode works on **count**, **shift**, and
**desc** layers – on a desc layer it is the natural home for a
continuous-variable comparison across arms (ANOVA / Kruskal-Wallis /
t-test), e.g.
`fn = function(.data) anova(lm(AGE ~ TRT, .data))[["Pr(>F)"]][1]`, with
one p-value per `by` group on that group's first statistic row.

**Pairwise / per-level mode** (`comparisons` non-`NULL`). Count layers
only. Emits one `pval` column per comparison, each comparing an arm
level of the first `cols` variable to `reference`, with a value on
*every* target-level row (like `risk_diff`'s `rdiff` columns). On a
**nested** count layer it emits a value on every row of every level –
each inner (e.g. preferred-term) row and each outer (e.g.
system-organ-class subtotal) row, each row's 2x2 built from that row's
own counts – and, when the layer has a total row, on the grand-total row
too (see `total_row`). Here `fn` receives, for one (row, comparison)
pair, a 2x2 contingency **matrix**
`matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)` – rows
are (reference, comparison) arm, columns are (event, no event) – where
`n` is the cell count and `N` the population denominator for that arm.
When the layer sets `distinct_by`, the distinct counts/denominators are
used. `fn` returns either a numeric value (a scalar, or a vector of
several statistics matching a multi-variable `format`) or a verbatim
character display string (`NA` renders a blank).

Attach it to a layer via `layer_settings(assoc_test = assoc_test(...))`.

## Examples

``` r
# Omnibus
at <- assoc_test(
  fn = function(.data) fisher.test(table(.data$TRT, .data$RESP))$p.value,
  format = f_str("x.xxx", "p"),
  label = "p-value [1]"
)

# Pairwise per-level (count layer): Fisher on an incidence 2x2
at2 <- assoc_test(
  fn = function(m) fisher.test(m)$p.value,
  reference = "Placebo",
  comparisons = c("Low", "High"),
  format = f_str("x.xxx", "p")
)

# Omnibus on a desc layer: continuous comparison across arms (ANOVA)
at3 <- assoc_test(
  fn = function(.data) anova(lm(AGE ~ TRT, .data))[["Pr(>F)"]][1],
  format = f_str("x.xxx", "p")
)

# Multiple statistics in one cell: odds ratio with a confidence interval
at4 <- assoc_test(
  fn = function(m) {
    ft <- fisher.test(m)
    c(ft$estimate, ft$conf.int[1], ft$conf.int[2])
  },
  reference = "Placebo",
  comparisons = c("Low", "High"),
  format = f_str("xx.xx (xx.xx, xx.xx)", "or", "lo", "hi"),
  label = "OR (95% CI)"
)
```
