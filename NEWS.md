# tplyr2 0.2.0

## New features

- New single-proportion confidence-interval statistic for count layers (#44).
  Four `f_str` keywords — `ci_lower`/`ci_upper` (from `n`/`total`) and
  `distinct_ci_lower`/`distinct_ci_upper` (from `distinct_n`/`distinct_total`) —
  are computed per column-by-target-level cell on the percentage scale, so an
  incidence CI drops straight into a count-layer format string, e.g.
  `f_str("xx (xx.x%) [xx.x, xx.x]", "distinct_n", "distinct_pct", "distinct_ci_lower", "distinct_ci_upper")`.
  Two new `layer_settings()` controls choose the method and coverage:
  `ci_method` (`"clopper_pearson"` default / exact, matching SAS `PROC FREQ
  EXACT` and `stats::binom.test()`; plus `"wilson"`, `"wald"`,
  `"agresti_coull"`, `"jeffreys"`) and `ci_level` (default `0.95`). The bounds
  are computed lazily (only when a format references a CI keyword) and appear on
  Total/Missing rows just like `pct`. The underlying vectorized helper,
  `proportion_ci()`, is also exported.
- `apply_formats()` gains `na`, `width`, and `pad` arguments (#41). `na` is a
  string substituted for cells whose format-group inputs are all NA, used
  instead of the blank-width fill (`na = ""` yields a truly empty cell, `nchar`
  0; `na = "NE"` renders `"NE"`), letting `apply_formats()` replace hand-rolled
  fixed-width formatters for externally row-bound statistics. `width` pads each
  formatted token to a fixed total width (`pad = "right"`/`"left"`); when the
  `na` substitution applies, it wins and the cell is not padded. The defaults
  (`NULL`) preserve existing behavior.
- New `as_display()` helper returning a display-ready frame — the `rowlabel*`,
  `res*`, and `rdiff*` columns only, with the internal `ord*` (and `row_id`)
  columns dropped, ready to hand to a table-rendering package (#36). Pass
  `labels = TRUE` to rename the result columns to their column-group header
  labels.
- New `n_records` descriptive statistic keyword for `group_desc()` — the number
  of records assessed (non-missing + missing), for tables that report an `n` of
  records/subjects assessed rather than the non-missing analysis count (#34).
  The existing `n` (non-missing) keyword is unchanged.
- New `denom_row` setting for shift layers — emits the per-baseline-group
  denominator (the `shift_denom = "column"` denominator) as an integer `n` row
  above the shift-to rows, instead of forcing callers to recompute it (#35).
  The label defaults to `"n"` (`denom_row_label`).
- New `assoc_test()` for count and shift layers — an omnibus association-test
  column (#37). It runs a caller-supplied function once per `by` group over the
  raw source-data subset for that group (all `cols` levels), so a Fisher's
  exact or CMH test can tabulate across the treatment columns, and lands the
  formatted result as a single trailing `pval1` column (one value per by-group,
  on the group's first row). Attach via `layer_settings(assoc_test = ...)`.
- `assoc_test()` gains a pairwise / per-level mode for count layers (#40).
  Supplying `comparisons` (with an optional `reference`, defaulting to the first
  `cols` level) compares the reference arm to each named arm and emits one
  `pval` column per comparison, each with a value on every target-level row
  (like `risk_diff`'s `rdiff` columns) — the standard AE-by-SOC/PT layout. In
  this mode the caller-supplied `fn` receives a 2x2 incidence matrix
  `matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)` (distinct
  counts/denominators when `distinct_by` is set) and returns a scalar p-value,
  so any test — `fisher.test`, and beyond — can be used. `label` may be a
  per-comparison vector; the default is `"<reference> vs <comparison>"`.
- `assoc_test()`'s `fn` may now return a **character** string, passed through
  to the `pval` cell verbatim (#47); `format` applies only when `fn` returns a
  numeric. This completes the arbitrary-`fn` design — the function that computes
  the test can also supply the finished display, so conditional p-value
  conventions (a significance flag like `0.031*`, a `>.99`/`<.0001`
  ceiling/floor, an `"NE"`/`"N/A"` sentinel, trailing-space alignment) all live
  in the `fn`. Works in both omnibus and pairwise modes; `NA` (numeric or
  character) still renders a blank. Existing numeric-returning fns are
  unaffected.
- Pairwise `assoc_test()` now works on **nested** count layers (#49), where it
  was previously a no-op. It emits a `pval` column per comparison with a value
  on every row of every level — each inner (e.g. preferred-term) row and each
  outer (e.g. system-organ-class subtotal) row — building each row's 2x2 from
  that row's own distinct counts and the `pop_data` denominators (the outer row
  uses the level's "any event in that group" subject count). This is the
  canonical AE-incidence-by-SOC/PT Fisher layout. A new `total_row` argument to
  `assoc_test()` (default `TRUE`) also lands a p-value on the layer's total
  ("any event anywhere") row; set `total_row = FALSE` to leave it blank.
  Missing rows are always blank. Combined with the character-return display
  (#47), the `fn` can supply the exact `* / >.99 / trailing-space / blank`
  cell text on every nested row. A zero-event arm is handled correctly: its
  2x2 denominator is taken from `pop_data` (subjects at risk), so a sparse or
  empty **reference** arm still yields a valid `0`-vs-`k` test on every row
  instead of blanking the column.
- `assoc_test()` now works on **`group_desc`** layers in omnibus mode (#51),
  giving a continuous-variable comparison across arms — ANOVA, Kruskal-Wallis,
  a t-test — a native home. Same contract as count/shift: `fn` receives the
  by-group's raw source subset (all `cols` levels) and returns a scalar p
  (formatted by `format`) or a verbatim character string (#47), placed on the
  by-group's first statistic row (`NA` → blank). A demographics table can now
  produce its comparison p-values — continuous *and* categorical characteristics
  sharing one `pval` column — entirely through tplyr2 instead of a hand-rolled
  `aov`/`kruskal.test` side pipeline. Pairwise/per-level mode remains count-layer
  only; supplying `comparisons` on a desc layer is now a clear error rather than
  silently ignored.
- `assoc_test()`'s `fn` may now return **multiple values** rendered into one
  cell (#60). When `format` references more than one variable, `fn` returns a
  numeric vector of matching length, mapped positionally onto the format — so a
  procedure that emits a tuple (an odds ratio with its confidence interval, an
  estimate with a p-value) lands as a single formatted cell, e.g.
  `f_str("xx.xx (xx.xx, xx.xx)", "or", "lo", "hi")`. A scalar return with a
  one-variable format is unchanged; an all-`NA` return or an arity mismatch
  renders a blank; the character-return passthrough (#47) still wins for a
  finished display string.
- New `shift_denom` setting for shift layers (#18). `shift_denom = "column"`
  computes percentages column-wise — out of each shift column group (the
  "from"/baseline group) within the treatment arm — the standard
  "% within the from group" shift display, and the header `(N=)` labels then
  reflect those per-column-group denominators. The default `"total"` keeps the
  arm-total denominator.
- New `pct_lt` and `pct_gt` count-layer settings for the regulatory
  "less-than / greater-than" percent conventions (#14). A cell with a nonzero
  count whose percent would display below `pct_lt` renders as `"<"` + the
  threshold (e.g. `1 ( <1%)` instead of `1 (  0%)`); a percent below 100 that
  would display above `pct_gt` renders as `">"` + the threshold (e.g. `>99`).
  The comparison is against the rounded display value, so a percent that
  rounds up to the threshold keeps its number.
- New `zero_count_display` count-layer setting (#14) controlling how cells
  with a zero count render: `"full"` (default, unchanged), `"count_only"`
  (just the count field, e.g. `" 0"`), or `"blank"` (empty string).
- New `stat_columns` layer setting for count layers (#10). Passing a named
  list of `f_str()` objects produces one result column per statistic per
  column group — for example, a distinct-subject "n (%)" column beside a raw
  event-count "E" column under each treatment arm. Column label attributes
  follow the pattern `"<column group> (N=n) | <stat name>"` so renderers can
  span the group over its stat sub-columns. Works with nested count layers,
  by variables, total/missing rows, risk difference, cell metadata,
  JSON/YAML serialization, and ARD conversion.
- `tplyr_meta` objects gain an optional `statistic` field recording which
  statistic a cell displays (populated for `stat_columns` layers).

## Bug fixes

- Count-layer row ordering now honors the sort settings it advertised (#57).
  `order_count_method = "bycount"` actually sorts by descending count (it
  previously fell back to the default order); `ordering_cols` selects which
  column's count drives that sort, and `result_order_var` which statistic;
  `outer_sort_position = "desc"` reverses a nested layer's outer level. Any
  explicit `order_count_method` (and the default) now also keeps `by`-groups
  **blocked** instead of interleaving them, and the default respects the target
  variable's factor levels (previously it ordered the target alphabetically
  even when it was a factor). The target sort key is threaded through the cast
  so all methods compose correctly with by-groups and special (total/missing)
  rows.
- `risk_diff` on a **nested** count layer now errors instead of silently
  emitting an all-blank column (#58). Risk difference is computed only on
  single-level count layers; on a nested SOC/PT layer the setting previously
  produced an empty `rdiff` column with no warning. The error points to
  pairwise `assoc_test()`, which does compute a per-level comparison on nested
  layers.
- `group_shift(denom_row = TRUE)` no longer renders the literal string `"NA"`
  for a baseline (shift-column) group that is absent within a `by` group (#55);
  an absent group's denominator is zero, so the cell now reads `0` (consistent
  with `zero_count_display` on the shift-to rows). A new `denom_row_format`
  setting also lets the denominator row carry its own `f_str` width independent
  of the `n_counts` cells (e.g. `denom_row_format = f_str("xx", "n")` for a plain
  narrow integer) instead of inheriting their padding.
- Omnibus `assoc_test()` no longer lets `total_group()` / `custom_group()`
  duplicate rows leak into the `fn`'s `.data` (#53). Those rows are a display
  construct for the count columns; including them double-counted every subject
  and silently returned a wrong p-value (no error, no warning). The synthetic
  rows — and their now-unused factor levels (e.g. a phantom `"Total"` level that
  made `chisq.test()` return `NaN`) — are dropped before `fn` runs, so it sees
  only the real observations.
- Omnibus `assoc_test()` now places its p-value on the layer's **first display
  row**, not the arbitrary pre-sort (dcast) row (#54). The value was written
  before the `ord*` reorder (e.g. `order_count_method = "byfactor"`), so it could
  strand on the wrong category (landing on `65-80` instead of `<65`, etc.);
  placement is now derived from the ordering columns, per by-group.
- `group_count()` `missing_count` now always emits the Missing row when set,
  zero-filling every column/by group that has no missing values, so the row
  reads `0 ( 0%)` throughout instead of being dropped (when the total missing
  count is zero) or leaving empty cells (when only some columns have missings)
  (#33). Matches classic Tplyr `set_missing_count()`.
- `group_shift(shift_denom = "column")` with a `by` variable now scopes the
  column (from-group) denominator within each by-group instead of pooling it
  across them (#28). A shift-by-visit table now gets per-visit percentages.
  With a `by` variable the header `(N=)` reflects the arm total (the
  per-column-group denominator varies by by-group, so no single header N can
  represent it); the no-`by` behavior (from-group N in the header) is unchanged.
- `group_shift()` now honors the `zero_count_display`, `pct_lt`, and `pct_gt`
  layer settings, applying them the same way `group_count()` does (#31).
  Previously a shift layer ignored them (e.g. a zero cell always rendered as
  `0 (  0%)` even with `zero_count_display = "count_only"`).
- Descriptive statistics that round to negative zero now display as `0.0`
  instead of `-0.0`, matching base R `format()` (#29).
- Result and risk-difference columns are now ordered by their numeric suffix
  when layers are combined and when metadata is built. Previously tables
  with more than 9 result columns sorted them lexicographically
  (`res10` before `res2`), scrambling column order.
- Count layers now order their `res*` columns by the `cols` variable's factor
  levels, matching `group_desc()` (#13). Previously count layers ordered
  result columns alphabetically by the `cols` value, so a spec mixing count
  and desc layers (or any renderer assuming `res1` is the first `cols` level)
  could get inconsistent column order. Shift layers likewise order their
  column dimension by the shift variable's factor levels.
- `group_count()` now orders its `by`-group rows by the `by` variable's factor
  levels (then a VARN companion, then alphabetically) instead of always
  alphabetically (#24). Previously a factor `by` such as visits came out
  mis-ordered (e.g. `Week 12` before `Week 2`), matching `group_shift()` and
  `group_desc()`.
- Fixed `group_count()` total/missing rows with a `by` variable (#24): each
  by-group's `Total` (or `Missing`) row is now labelled with its by-group value
  instead of a blank, and special rows now sort after the normal rows within
  each group (previously e.g. `Total` was interleaved alphabetically among the
  target values, and with a `by` variable the row label was dropped entirely).
- `group_count(order_count_method = "byfactor")` now orders category rows by the
  target variable's factor levels instead of alphabetically (#16). The target
  column is coerced to character while counts are built, so the level order is
  now recovered from the source data (`compute_var_order()`). Nested count
  layers likewise order their outer and inner categories by factor levels
  (previously they fell back to the dcast's alphabetical row order).
- `group_desc(stats_as_columns = TRUE)` combined with a `by` variable no longer
  drops the by-groups and returns only the last group's statistics (#20). It
  now keeps the by-groups as rows and produces one result column per
  treatment x statistic (labelled `"<arm> | <stat>"`). Behavior without a `by`
  variable (treatment groups as rows, statistics as columns) is unchanged.
- `group_desc()` now orders its `by`-group rows by the `by` variable's factor
  levels (then a VARN companion, then alphabetically) instead of always
  alphabetically (#20). Previously a factor `by` such as visits came out
  mis-ordered (e.g. `Week 12` before `Week 2`); this applies to both the
  standard stats-as-rows output and `stats_as_columns = TRUE`.

## Documentation

- New vignette *"Comparative Statistics and Binding External Results"*
  (`vignette("binding-statistics")`) — how to attach cross-arm comparisons
  (`assoc_test()`, `risk_diff`, single-proportion CIs) and how to bind
  externally-computed model results (MMRM/ANCOVA/Cox/logistic p-values,
  LS-means, CIs) onto an assembled table via `apply_formats()` and
  `as_display()`, plus where `group_analyze()` fits.
- Clarified in `?layer_settings` and the denominators vignette that `denoms_by`
  **replaces** (does not augment) the default `cols`-based denominator
  grouping, so you must include the `cols` variable(s) to get per-column
  denominators (#19).
