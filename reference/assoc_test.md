# Association-test column(s) for count and shift layers

Configures an association test and lands its formatted result beside the
n(\\ is supplied:

## Usage

``` r
assoc_test(
  fn,
  format = f_str("x.xxx", "p"),
  label = NULL,
  reference = NULL,
  comparisons = NULL
)
```

## Arguments

- fn:

  A function of one argument. In omnibus mode it is called with the
  source-data subset (a data.frame) for a single `by` group; in pairwise
  mode it is called with a 2x2 numeric matrix (see Details). It must
  return a single numeric value (typically a p-value). Return `NA` to
  render a blank.

- format:

  An [`f_str`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  object formatting the returned value. The f_str must reference a
  single variable (any name; the returned scalar is passed
  positionally). Defaults to `f_str("x.xxx", "p")`.

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
group's first output row.

**Pairwise / per-level mode** (`comparisons` non-`NULL`). Count layers
only. Emits one `pval` column per comparison, each comparing an arm
level of the first `cols` variable to `reference`, with a value on
*every* target-level row (like `risk_diff`'s `rdiff` columns). Here `fn`
receives, for one (target level, comparison) pair, a 2x2 contingency
**matrix**
`matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)` – rows
are (reference, comparison) arm, columns are (event, no event) – where
`n` is the cell count and `N` the population denominator for that arm.
When the layer sets `distinct_by`, the distinct counts/denominators are
used. `fn` must return a scalar p-value (`NA` renders a blank).

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
```
