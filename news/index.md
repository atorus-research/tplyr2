# Changelog

## tplyr2 0.2.0

### Breaking changes

Several settings that were previously accepted and silently ignored now
error. Each of these produced a plausible-looking but wrong table, which
is the wrong default for a clinical reporting package.

- Unknown `...` overrides passed to
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
  error instead of being ignored (#73).
  `tplyr_build(spec, adsl, wher = "SAFFL == 'Y'")` used to build on
  **unfiltered** data; it now errors and lists the valid override names.
- Unknown option names passed to
  [`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md)
  error (#74). A misspelled `IBMrounding` used to set a dead option and
  leave the whole output package on banker’s rounding.
- `result_order_var` naming a statistic the layer does not compute
  errors instead of falling back to `"n"`, and `ordering_cols` matching
  none of the observed column levels errors instead of zeroing every
  sort key (#78). A partially unmatched `ordering_cols` warns and sorts
  on the levels that matched.
- `denoms_by` must name the layer’s own grouping variables — the column
  variables, `by` variables, and (for count and shift layers) the target
  variable (#77). An unrecognized name used to shrink the join key set
  silently, either multiplying table rows or attaching another group’s
  denominator.
- Unrecognized `missing_count` keys error (#80).
- [`tplyr_stats_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_stats_data.md)
  now returns the grouping columns plus the requested statistic, as
  documented, rather than the entire layer frame (#79). Use
  [`tplyr_numeric_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_numeric_data.md)
  for every statistic.
- Count and shift layers now render a percentage with no usable
  denominator as blank rather than `0` (#76), matching what desc layers
  already did. A genuine zero count against a real denominator still
  renders `0.0%`.

### New features

- New single-proportion confidence-interval statistic for count layers
  (#44). Four `f_str` keywords — `ci_lower`/`ci_upper` (from
  `n`/`total`) and `distinct_ci_lower`/`distinct_ci_upper` (from
  `distinct_n`/`distinct_total`) — are computed per
  column-by-target-level cell on the percentage scale, so an incidence
  CI drops straight into a count-layer format string, e.g.
  `f_str("xx (xx.x%) [xx.x, xx.x]", "distinct_n", "distinct_pct", "distinct_ci_lower", "distinct_ci_upper")`.
  Two new
  [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
  controls choose the method and coverage: `ci_method`
  (`"clopper_pearson"` default / exact, matching SAS `PROC FREQ EXACT`
  and [`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html);
  plus `"wilson"`, `"wald"`, `"agresti_coull"`, `"jeffreys"`) and
  `ci_level` (default `0.95`). The bounds are computed lazily (only when
  a format references a CI keyword) and appear on Total/Missing rows
  just like `pct`. The underlying vectorized helper,
  [`proportion_ci()`](https://github.com/mstackhouse/tplyr2/reference/proportion_ci.md),
  is also exported.
- [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)
  gains `na`, `width`, and `pad` arguments (#41). `na` is a string
  substituted for cells whose format-group inputs are all NA, used
  instead of the blank-width fill (`na = ""` yields a truly empty cell,
  `nchar` 0; `na = "NE"` renders `"NE"`), letting
  [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)
  replace hand-rolled fixed-width formatters for externally row-bound
  statistics. `width` pads each formatted token to a fixed total width
  (`pad = "right"`/`"left"`); when the `na` substitution applies, it
  wins and the cell is not padded. The defaults (`NULL`) preserve
  existing behavior.
- New
  [`as_display()`](https://github.com/mstackhouse/tplyr2/reference/as_display.md)
  helper returning a display-ready frame — the `rowlabel*`, `res*`, and
  `rdiff*` columns only, with the internal `ord*` (and `row_id`) columns
  dropped, ready to hand to a table-rendering package (#36). Pass
  `labels = TRUE` to rename the result columns to their column-group
  header labels.
- New `n_records` descriptive statistic keyword for
  [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
  — the number of records assessed (non-missing + missing), for tables
  that report an `n` of records/subjects assessed rather than the
  non-missing analysis count (#34). The existing `n` (non-missing)
  keyword is unchanged.
- New `denom_row` setting for shift layers — emits the
  per-baseline-group denominator (the `shift_denom = "column"`
  denominator) as an integer `n` row above the shift-to rows, instead of
  forcing callers to recompute it (#35). The label defaults to `"n"`
  (`denom_row_label`).
- New
  [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)
  for count and shift layers — an omnibus association-test column (#37).
  It runs a caller-supplied function once per `by` group over the raw
  source-data subset for that group (all `cols` levels), so a Fisher’s
  exact or CMH test can tabulate across the treatment columns, and lands
  the formatted result as a single trailing `pval1` column (one value
  per by-group, on the group’s first row). Attach via
  `layer_settings(assoc_test = ...)`.
- [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)
  gains a pairwise / per-level mode for count layers (#40). Supplying
  `comparisons` (with an optional `reference`, defaulting to the first
  `cols` level) compares the reference arm to each named arm and emits
  one `pval` column per comparison, each with a value on every
  target-level row (like `risk_diff`’s `rdiff` columns) — the standard
  AE-by-SOC/PT layout. In this mode the caller-supplied `fn` receives a
  2x2 incidence matrix
  `matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)`
  (distinct counts/denominators when `distinct_by` is set) and returns a
  scalar p-value, so any test — `fisher.test`, and beyond — can be used.
  `label` may be a per-comparison vector; the default is
  `"<reference> vs <comparison>"`.
- [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)’s
  `fn` may now return a **character** string, passed through to the
  `pval` cell verbatim (#47); `format` applies only when `fn` returns a
  numeric. This completes the arbitrary-`fn` design — the function that
  computes the test can also supply the finished display, so conditional
  p-value conventions (a significance flag like `0.031*`, a
  `>.99`/`<.0001` ceiling/floor, an `"NE"`/`"N/A"` sentinel,
  trailing-space alignment) all live in the `fn`. Works in both omnibus
  and pairwise modes; `NA` (numeric or character) still renders a blank.
  Existing numeric-returning fns are unaffected.
- Pairwise
  [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)
  now works on **nested** count layers (#49), where it was previously a
  no-op. It emits a `pval` column per comparison with a value on every
  row of every level — each inner (e.g. preferred-term) row and each
  outer (e.g. system-organ-class subtotal) row — building each row’s 2x2
  from that row’s own distinct counts and the `pop_data` denominators
  (the outer row uses the level’s “any event in that group” subject
  count). This is the canonical AE-incidence-by-SOC/PT Fisher layout. A
  new `total_row` argument to
  [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)
  (default `TRUE`) also lands a p-value on the layer’s total (“any event
  anywhere”) row; set `total_row = FALSE` to leave it blank. Missing
  rows are always blank. Combined with the character-return display
  (#47), the `fn` can supply the exact
  `* / >.99 / trailing-space / blank` cell text on every nested row. A
  zero-event arm is handled correctly: its 2x2 denominator is taken from
  `pop_data` (subjects at risk), so a sparse or empty **reference** arm
  still yields a valid `0`-vs-`k` test on every row instead of blanking
  the column.
- [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)
  now works on **`group_desc`** layers in omnibus mode (#51), giving a
  continuous-variable comparison across arms — ANOVA, Kruskal-Wallis, a
  t-test — a native home. Same contract as count/shift: `fn` receives
  the by-group’s raw source subset (all `cols` levels) and returns a
  scalar p (formatted by `format`) or a verbatim character string (#47),
  placed on the by-group’s first statistic row (`NA` → blank). A
  demographics table can now produce its comparison p-values —
  continuous *and* categorical characteristics sharing one `pval` column
  — entirely through tplyr2 instead of a hand-rolled
  `aov`/`kruskal.test` side pipeline. Pairwise/per-level mode remains
  count-layer only; supplying `comparisons` on a desc layer is now a
  clear error rather than silently ignored.
- [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)’s
  `fn` may now return **multiple values** rendered into one cell (#60).
  When `format` references more than one variable, `fn` returns a
  numeric vector of matching length, mapped positionally onto the format
  — so a procedure that emits a tuple (an odds ratio with its confidence
  interval, an estimate with a p-value) lands as a single formatted
  cell, e.g. `f_str("xx.xx (xx.xx, xx.xx)", "or", "lo", "hi")`. A scalar
  return with a one-variable format is unchanged; an all-`NA` return or
  an arity mismatch renders a blank; the character-return passthrough
  (#47) still wins for a finished display string.
- New `shift_denom` setting for shift layers (#18).
  `shift_denom = "column"` computes percentages column-wise — out of
  each shift column group (the “from”/baseline group) within the
  treatment arm — the standard “% within the from group” shift display,
  and the header `(N=)` labels then reflect those per-column-group
  denominators. The default `"total"` keeps the arm-total denominator.
- New `pct_lt` and `pct_gt` count-layer settings for the regulatory
  “less-than / greater-than” percent conventions (#14). A cell with a
  nonzero count whose percent would display below `pct_lt` renders as
  `"<"` + the threshold (e.g. `1 ( <1%)` instead of `1 ( 0%)`); a
  percent below 100 that would display above `pct_gt` renders as `">"` +
  the threshold (e.g. `>99`). The comparison is against the rounded
  display value, so a percent that rounds up to the threshold keeps its
  number.
- New `zero_count_display` count-layer setting (#14) controlling how
  cells with a zero count render: `"full"` (default, unchanged),
  `"count_only"` (just the count field, e.g. `" 0"`), or `"blank"`
  (empty string).
- New `stat_columns` layer setting for count layers (#10). Passing a
  named list of
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  objects produces one result column per statistic per column group —
  for example, a distinct-subject “n (%)” column beside a raw
  event-count “E” column under each treatment arm. Column label
  attributes follow the pattern `"<column group> (N=n) | <stat name>"`
  so renderers can span the group over its stat sub-columns. Works with
  nested count layers, by variables, total/missing rows, risk
  difference, cell metadata, JSON/YAML serialization, and ARD
  conversion.
- `tplyr_meta` objects gain an optional `statistic` field recording
  which statistic a cell displays (populated for `stat_columns` layers).
- `missing_count` gains `denom_exclude` (#80), which was previously
  accepted in config and never implemented. With `denom_exclude = TRUE`
  the rows folded into the Missing row (`NA` plus anything in
  `missing_values`) leave the layer’s denominator, so percentages are of
  the non-missing population. Every key `missing_count` accepts is now
  documented.
- Failures in user-supplied code no longer discard their message (#75).
  Custom summaries and `assoc_test` functions still cannot abort a build
  and still render `NA` as a blank cell, but the reasons are now
  collected and reported as one warning per build, deduplicated and
  naming the summary or test and the group affected. Previously a
  partial failure — real numbers everywhere and one blank cell where the
  expression errored — was indistinguishable from data legitimately
  missing. An `assoc_test` function whose return does not match its
  format’s variable count is reported as the caller bug it is.
- Missing and zero denominators are no longer silent (#76). A count with
  `n > 0` against an `NA` or zero denominator warns, naming the layer
  and the affected groups, and
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
  warns when `pop_data` has no rows for a column level present in the
  analysis data.

### Bug fixes

- A layer whose `where` clause left a column group with no rows emitted
  fewer result columns than its sibling layers, and those columns were
  then aligned positionally — putting that layer’s values **under the
  wrong treatment arm**. In `tplyr_adae`, for example, a
  `where = AESEV == "SEVERE"` layer alongside an unfiltered one reported
  Xanomeline Low Dose’s severe events under the Placebo label. The
  column-variable level set is now captured from the table’s full data
  and pinned for every layer, so an empty column group completes with
  zeros in its own position.

- A
  [`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md)
  combined with a
  [`custom_group()`](https://github.com/mstackhouse/tplyr2/reference/custom_group.md)
  on the same column variable double-counted the pooled subjects: the
  total duplicated the custom group’s copies as well as the originals,
  so a 254-subject study reported `Total (N=422)` and a sex count of 233
  where 143 was correct. Duplicated rows now record which column
  variable they were created for, so a total group skips copies made on
  its own variable while still spanning copies made for a different one.

- A shift layer or a `stats_as_columns` desc layer combined with a
  standard layer produced a table whose `res` columns meant different
  things in different row blocks, keeping only the first layer’s column
  labels. Those combinations are now rejected by
  [`validate_spec()`](https://github.com/mstackhouse/tplyr2/reference/validate_spec.md)
  with a message pointing at separate specs. (This replaces a silently
  mislabeled table, so a spec that “worked” before may now error — it
  was not producing a correct table.)

- [`tplyr_meta_subset()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta_subset.md)
  treated an empty filter set as “nothing matches” and returned zero
  rows. A cell can legitimately have no filters — a
  [`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md)
  column crossed with a total row, or with a desc statistic in a layer
  that has no `by` variable — and those cells describe the whole
  dataset. It now returns all rows.

- Cell metadata dropped the `by` filter when a `by` level was an empty
  string, and aborted the entire metadata build with “missing value
  where TRUE/FALSE needed” when a `by` level was `NA`. Both are real
  levels: an empty string now filters on `""` and `NA` filters with
  [`is.na()`](https://rdrr.io/r/base/NA.html). A nested layer’s
  structurally absent inner label still contributes no filter, as
  before.

- Cell metadata compared `by` values against the *trimmed* row label, so
  a `by` variable whose values carry leading or trailing whitespace
  (common in SAS-derived character data) produced filters matching zero
  rows. Filters now use the untrimmed value.

- [`generate_row_ids()`](https://github.com/mstackhouse/tplyr2/reference/generate_row_ids.md)
  silently produced duplicate IDs when row labels had been blanked by
  [`apply_row_masks()`](https://github.com/mstackhouse/tplyr2/reference/apply_row_masks.md)
  or when a target level collided with a `total_row_label`, so metadata
  lookups resolved to the wrong cell. It now warns.

- A missing-subjects row built without `distinct_by` carried filters
  resolving to the subjects that *do* appear — the exact complement of
  what the cell counts. Row-level missing-subjects counting is a
  population-minus-target difference that no filter set can express, so
  no metadata is emitted for it and the build warns.

- Cell metadata for a `stats_as_columns` desc layer resolved to nothing.
  That layout labels its result columns `"<arm> (N=n) | <statistic>"`,
  the same grammar count-layer `stat_columns` uses, but the trailing
  statistic segment was stripped only for count layers — so every filter
  read `TRT == "A | n"` and matched zero rows.
  [`tplyr_meta_subset()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta_subset.md)
  now returns the correct source rows.

- A count layer’s total row rendered a **blank** instead of `0` for any
  column group with no rows in the analysis data, while the category
  rows above it correctly showed `0` (#66). `n` is counted from the raw
  analysis rows, so an empty column group never appears there; the
  denominator join now brings it in from the completed category counts
  and zero-fills it. Nothing errored or warned, so the value silently
  vanished from a delivered table.

- A count layer’s total row counted `n` by summing the category rows
  while `distinct_n` counted from the raw data, so the two disagreed in
  the same cell and `total_row_count_missings` had no effect on `n`.
  Category rows exclude NA target values (data completion drops them)
  and any level folded into the Missing row, so the sum silently omitted
  them. `n` is now counted from the raw rows with the same missing
  handling as `distinct_n`, making `n`, `distinct_n`, and the cell’s
  metadata agree. **A total row over data with missing target values
  will change**: with the default `total_row_count_missings = TRUE` it
  now includes them, as documented.

- `tplyr_build(metadata = TRUE)` now warns when a `stats_as_columns`
  desc layer has no `by` variable. That layout names its result columns
  after the statistics rather than `res1`, `res2`, …, and cell metadata
  is keyed on `res` columns, so none was produced — previously without
  any indication.

- `missing_count`’s `missing_values` no longer double-counts. Values
  named there are folded into the Missing row, but they also kept their
  own category row, so the same records were counted twice and the
  column summed past 100%. They are now removed from the category rows,
  matching Tplyr v1’s `set_missing_count()` and the exclusion
  [`tplyr_meta()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_meta.md)
  already assumed. On a nested count layer, naming an outer-level value
  removes its inner rows along with it. **This changes the numbers in
  any table that used `missing_values`** — previously those tables were
  wrong.

- `f_str(empty = )` now honors its unnamed form. Only
  `c(.overall = "...")` was implemented; an unnamed `empty = "NA"` was
  silently ignored. It now fills each NA format group in place,
  right-justified to that group’s field width, so
  `f_str("xx (xxx)", "n", "pct", empty = "NA")` renders `"NA ( NA)"` and
  a partially missing cell keeps its alignment. This restores v1 parity;
  `.overall` is unchanged and still replaces the whole cell only when
  every group is NA.

- Shift layers now compute the single-proportion confidence-interval
  keywords (`ci_lower`, `ci_upper`, `distinct_ci_lower`,
  `distinct_ci_upper`). They were accepted by validation but never
  computed, so a shift format referencing one rendered an empty field
  with no warning. Bounds follow whichever denominator `shift_denom`
  selects.

- [`as_display()`](https://github.com/mstackhouse/tplyr2/reference/as_display.md)
  no longer discards the result columns of a `stats_as_columns` desc
  layer built without a `by` variable. That layout names its columns
  after the statistics rather than `res1`, `res2`, …, and the whitelist
  dropped them, returning row labels alone. It now removes the internal
  `ord_layer_*`/`row_id` helpers and keeps everything else.

- `stats_as_columns` with no `by` variable now orders its columns by
  format-string order rather than alphabetically by statistic label.

- [`collect_precision()`](https://github.com/mstackhouse/tplyr2/reference/collect_precision.md)
  now warns when `precision_data` does not cover every `precision_by`
  group present in the data (those cells render blank), and when
  `precision_data` omits the `precision_by` columns entirely (its widths
  are applied to every group). Both were silent.

- [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  now warns when a format group requests parenthesis hugging (`X`/`A`)
  but has no literal text in front of it — there is nothing to hug, and
  the number is left-justified with trailing spaces instead.

- Count-layer row ordering now honors the sort settings it advertised
  (#57). `order_count_method = "bycount"` actually sorts by descending
  count (it previously fell back to the default order); `ordering_cols`
  selects which column’s count drives that sort, and `result_order_var`
  which statistic; `outer_sort_position = "desc"` reverses a nested
  layer’s outer level. Any explicit `order_count_method` (and the
  default) now also keeps `by`-groups **blocked** instead of
  interleaving them, and the default respects the target variable’s
  factor levels (previously it ordered the target alphabetically even
  when it was a factor). The target sort key is threaded through the
  cast so all methods compose correctly with by-groups and special
  (total/missing) rows.

- `order_count_method = "bycount"` now also reaches the **inner** level
  of a nested count layer (#64), sorting (e.g.) preferred terms by
  descending count within each system organ class – the AE-by-SOC/PT
  convention – with `result_order_var`/`ordering_cols` honored.
  Previously it only affected single-level layers. The outer level keeps
  its own order (controlled by `outer_sort_position`), so the useful
  “outer alphabetical, inner by descending count” layout comes for free.

- `risk_diff` on a **nested** count layer now errors instead of silently
  emitting an all-blank column (#58). Risk difference is computed only
  on single-level count layers; on a nested SOC/PT layer the setting
  previously produced an empty `rdiff` column with no warning. The error
  points to pairwise
  [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md),
  which does compute a per-level comparison on nested layers.

- `group_shift(denom_row = TRUE)` no longer renders the literal string
  `"NA"` for a baseline (shift-column) group that is absent within a
  `by` group (#55); an absent group’s denominator is zero, so the cell
  now reads `0` (consistent with `zero_count_display` on the shift-to
  rows). A new `denom_row_format` setting also lets the denominator row
  carry its own `f_str` width independent of the `n_counts` cells
  (e.g. `denom_row_format = f_str("xx", "n")` for a plain narrow
  integer) instead of inheriting their padding.

- Omnibus
  [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)
  no longer lets
  [`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md)
  /
  [`custom_group()`](https://github.com/mstackhouse/tplyr2/reference/custom_group.md)
  duplicate rows leak into the `fn`’s `.data` (#53). Those rows are a
  display construct for the count columns; including them double-counted
  every subject and silently returned a wrong p-value (no error, no
  warning). The synthetic rows — and their now-unused factor levels
  (e.g. a phantom `"Total"` level that made
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) return
  `NaN`) — are dropped before `fn` runs, so it sees only the real
  observations.

- Omnibus
  [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md)
  now places its p-value on the layer’s **first display row**, not the
  arbitrary pre-sort (dcast) row (#54). The value was written before the
  `ord*` reorder (e.g. `order_count_method = "byfactor"`), so it could
  strand on the wrong category (landing on `65-80` instead of `<65`,
  etc.); placement is now derived from the ordering columns, per
  by-group.

- [`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
  `missing_count` now always emits the Missing row when set,
  zero-filling every column/by group that has no missing values, so the
  row reads `0 ( 0%)` throughout instead of being dropped (when the
  total missing count is zero) or leaving empty cells (when only some
  columns have missings) (#33). Matches classic Tplyr
  `set_missing_count()`.

- `group_shift(shift_denom = "column")` with a `by` variable now scopes
  the column (from-group) denominator within each by-group instead of
  pooling it across them (#28). A shift-by-visit table now gets
  per-visit percentages. With a `by` variable the header `(N=)` reflects
  the arm total (the per-column-group denominator varies by by-group, so
  no single header N can represent it); the no-`by` behavior (from-group
  N in the header) is unchanged.

- [`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md)
  now honors the `zero_count_display`, `pct_lt`, and `pct_gt` layer
  settings, applying them the same way
  [`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
  does (#31). Previously a shift layer ignored them (e.g. a zero cell
  always rendered as `0 ( 0%)` even with
  `zero_count_display = "count_only"`).

- Descriptive statistics that round to negative zero now display as
  `0.0` instead of `-0.0`, matching base R
  [`format()`](https://rdrr.io/r/base/format.html) (#29).

- Result and risk-difference columns are now ordered by their numeric
  suffix when layers are combined and when metadata is built. Previously
  tables with more than 9 result columns sorted them lexicographically
  (`res10` before `res2`), scrambling column order.

- Count layers now order their `res*` columns by the `cols` variable’s
  factor levels, matching
  [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
  (#13). Previously count layers ordered result columns alphabetically
  by the `cols` value, so a spec mixing count and desc layers (or any
  renderer assuming `res1` is the first `cols` level) could get
  inconsistent column order. Shift layers likewise order their column
  dimension by the shift variable’s factor levels.

- [`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
  now orders its `by`-group rows by the `by` variable’s factor levels
  (then a VARN companion, then alphabetically) instead of always
  alphabetically (#24). Previously a factor `by` such as visits came out
  mis-ordered (e.g. `Week 12` before `Week 2`), matching
  [`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md)
  and
  [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md).

- Fixed
  [`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
  total/missing rows with a `by` variable (#24): each by-group’s `Total`
  (or `Missing`) row is now labelled with its by-group value instead of
  a blank, and special rows now sort after the normal rows within each
  group (previously e.g. `Total` was interleaved alphabetically among
  the target values, and with a `by` variable the row label was dropped
  entirely).

- `group_count(order_count_method = "byfactor")` now orders category
  rows by the target variable’s factor levels instead of alphabetically
  (#16). The target column is coerced to character while counts are
  built, so the level order is now recovered from the source data
  ([`compute_var_order()`](https://github.com/mstackhouse/tplyr2/reference/compute_var_order.md)).
  Nested count layers likewise order their outer and inner categories by
  factor levels (previously they fell back to the dcast’s alphabetical
  row order).

- `group_desc(stats_as_columns = TRUE)` combined with a `by` variable no
  longer drops the by-groups and returns only the last group’s
  statistics (#20). It now keeps the by-groups as rows and produces one
  result column per treatment x statistic (labelled `"<arm> | <stat>"`).
  Behavior without a `by` variable (treatment groups as rows, statistics
  as columns) is unchanged.

- [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
  now orders its `by`-group rows by the `by` variable’s factor levels
  (then a VARN companion, then alphabetically) instead of always
  alphabetically (#20). Previously a factor `by` such as visits came out
  mis-ordered (e.g. `Week 12` before `Week 2`); this applies to both the
  standard stats-as-rows output and `stats_as_columns = TRUE`.

- Risk difference and pairwise `assoc_test` columns came out **entirely
  blank** when `by` led with a string label (#72). Both merge functions
  assumed the `by` data variables occupied the first `rowlabel` columns,
  so with `by = c("Age Group", "SEX")` the join keyed the constant-label
  column against `SEX` values and matched nothing. They now share one
  helper that offsets past the label columns.

- A `where` clause longer than about 60 characters could not be read
  back from a spec file (#70).
  [`rlang::expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.html)
  wraps at that width, and the resulting multi-element array was not
  something `parse_expr()` could accept — so any realistic
  multi-condition ADaM filter broke the spec file in both formats. Files
  written by the old code still read.

- `precision_cap` was silently dropped on a spec-file round trip in
  **both** formats (#69). Both writers dropped the names of the named
  numeric vector, and
  [`apply_precision_cap()`](https://github.com/mstackhouse/tplyr2/reference/apply_precision_cap.md)
  dispatches on those names, so a round-tripped spec rendered different
  numbers than the original with no error or warning.
  [`apply_precision_cap()`](https://github.com/mstackhouse/tplyr2/reference/apply_precision_cap.md)
  now also warns when given a cap carrying neither an `int` nor a `dec`
  name.

- Multi-element character settings (`denoms_by`, `keep_levels`,
  `precision_by`, and friends) deserialized as lists from JSON and broke
  the build (#68); YAML was unaffected only because it auto-simplifies.
  A `denoms_by` list made data.table’s `by=` error out. One field-type
  table now restores every plain setting’s vector type, and a test
  asserts that no
  [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
  parameter is missing from it.

- A `denom_where` expression read back from a spec file was evaluated as
  a call rather than stored, erroring on the first variable name it
  contained.

- [`tplyr_from_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_from_ard.md)
  re-defaulted desc-layer format strings instead of sharing
  [`get_desc_formats()`](https://github.com/mstackhouse/tplyr2/reference/get_desc_formats.md)
  with the build path (#71), reconstructing a 1-row table at a different
  width than the 6-row build. Format-string rows also kept `dcast`’s
  alphabetical order rather than their declared order, which affected
  ARD-reconstructed desc layers and analyze layers alike.

- Unknown keys in a spec file are no longer dropped silently (#81).
  Hand-editing spec files is supported, and a typo’d key such as
  `total_rows` built a table without the requested behavior and said
  nothing. Unknown layer settings and unknown top-level spec keys now
  warn, naming the layer.

- A `pop_data` that renames the column variable —
  `pop_data(c(TRTA = "TRT01P"))` — skipped
  [`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md)
  and
  [`custom_group()`](https://github.com/mstackhouse/tplyr2/reference/custom_group.md)
  on the population side, because the rename ran after those were
  applied. The Total column had no population rows, so every nonzero
  count in it displayed `0.0%`.

- [`compute_risk_diff()`](https://github.com/mstackhouse/tplyr2/reference/compute_risk_diff.md)
  computed the plain difference inside the same
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) as the
  confidence interval, so a
  [`prop.test()`](https://rdrr.io/r/stats/prop.test.html) failure
  blanked the difference along with the CI even though the difference
  needs no test (#76). It also now pre-validates that counts do not
  exceed their denominators instead of letting
  [`prop.test()`](https://rdrr.io/r/stats/prop.test.html)’s error be
  swallowed into an all-`NA` row.

### Documentation

- **The formatting vignettes have been reorganized.** The two previous
  articles, *“General String Formatting”* and *“Advanced Descriptive
  Statistics Formatting”*, are replaced by three organized around what
  the user is trying to do rather than by layer type:
  - [`vignette("format_strings")`](https://github.com/mstackhouse/tplyr2/articles/format_strings.md)
    — the format string grammar, where format strings attach per layer
    type, the complete statistic keyword reference for count/shift/desc
    layers, rounding, missing-value handling, and standalone
    [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md).
  - [`vignette("precision_alignment")`](https://github.com/mstackhouse/tplyr2/articles/precision_alignment.md)
    — auto-precision (`a`/`A`, `+N`, `precision_on`, `precision_by`,
    `precision_cap`, `precision_data`) and parenthesis hugging
    (`X`/`A`).
  - [`vignette("display_conventions")`](https://github.com/mstackhouse/tplyr2/articles/display_conventions.md)
    — the display rules imposed by shells and SAPs: `pct_lt`/`pct_gt`,
    `zero_count_display`, `stat_columns` and `stats_as_columns`,
    `keep_levels`, `missing_count`, shift denominators, and
    indenting/wrapping nested terms.

  The old vignettes duplicated auto-precision, hugging, and `empty` at
  similar depth while disagreeing about what hugging does, and their
  four statistic keyword lists contradicted each other and the source.
  The following were previously documented in no vignette at all and are
  now covered: `pct_lt`, `pct_gt`, `zero_count_display`, `keep_levels`,
  `missing_count`, `total_row_count_missings`, `stats_as_columns`,
  `shift_denom`, `denom_row`, `denom_row_label`, `denom_row_format`, the
  desc-layer `total`/`pct` keywords, and
  [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)’s
  `na`/`width`/`pad`/`lt`/`gt` arguments.
- Corrected the description of parenthesis hugging. tplyr2 moves a
  hugged group’s leading spaces to just inside the trailing literal’s
  last character (`12 (34.5% )`); Tplyr v1 moved them to the left of the
  opening delimiter (`12 (34.5%)`). Two vignettes described v1’s
  behavior. The difference is now stated explicitly for anyone
  reconciling output against v1.
- Corrected the description of `shift_denom = "column"`: it denominates
  by the result column group (arm × post-baseline category), so each
  result *column* sums to 100%.
  [`vignette("shift")`](https://github.com/mstackhouse/tplyr2/articles/shift.md)
  now shows all three shift denominators, including how to get row-wise
  percentages with `denoms_by`.
- Documented that auto-precision (`a`/`A`) resolves against the data
  only in
  [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
  layers; elsewhere it degrades to a fixed width equal to the number of
  characters written.
- Documented several other scope limits that were previously unstated:
  the four confidence-interval keywords are count-layer only (a shift
  layer accepts them and renders them empty); the desc-layer `total`
  keyword is a **record** count, so `pct` is a share of the arm only on
  one-row-per-subject data; `keep_levels` filters after the denominators
  are computed, so the kept percentages do not re-base; `precision_data`
  validates only `max_int`/`max_dec`, rendering a blank cell for any
  group it fails to cover; `pct_lt`/`pct_gt` and `zero_count_display`
  target the first matching format group; and
  [`str_indent_wrap()`](https://github.com/mstackhouse/tplyr2/reference/str_indent_wrap.md)
  charges an existing indent against `width` twice.
- Documented that literal text in a format string cannot contain `x`,
  `X`, `a`, or `A` — those characters are always parsed as format
  groups, so a template like `"xx days"` silently gains a second group.
- [`vignette("sort")`](https://github.com/mstackhouse/tplyr2/articles/sort.md)
  now covers `order_count_method = "bycount"` on nested count layers,
  including that it sorts the inner level only and that
  `outer_sort_position` reverses the outer order rather than ranking it
  by count.
- [`vignette("post_processing")`](https://github.com/mstackhouse/tplyr2/articles/post_processing.md)
  now covers
  [`as_display()`](https://github.com/mstackhouse/tplyr2/reference/as_display.md),
  `collapse_row_labels(nest = TRUE)`, and the
  [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)
  `na`/`width` arguments, and points at the declarative
  `pct_lt`/`zero_count_display` settings before
  [`apply_conditional_format()`](https://github.com/mstackhouse/tplyr2/reference/apply_conditional_format.md).
- [`vignette("riskdiff")`](https://github.com/mstackhouse/tplyr2/articles/riskdiff.md)
  now states that `risk_diff` errors on nested count layers and points
  to pairwise
  [`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md).
- Fixed the IBM-rounding example in
  [`vignette("options")`](https://github.com/mstackhouse/tplyr2/articles/options.md),
  which showed two identical tables under captions promising a
  difference.
- Fixed a broken `vignette("serialize")` cross-reference in
  [`vignette("ard")`](https://github.com/mstackhouse/tplyr2/articles/ard.md),
  and expanded the vignette indexes in the README and
  [`vignette("tplyr2")`](https://github.com/mstackhouse/tplyr2/articles/tplyr2.md),
  which listed 10 and 8 of the 19 articles respectively.
- [`print()`](https://rdrr.io/r/base/print.html) on an `f_str` object no
  longer runs its fields together on one line.
- New vignette *“Comparative Statistics and Binding External Results”*
  ([`vignette("binding-statistics")`](https://github.com/mstackhouse/tplyr2/articles/binding-statistics.md))
  — how to attach cross-arm comparisons
  ([`assoc_test()`](https://github.com/mstackhouse/tplyr2/reference/assoc_test.md),
  `risk_diff`, single-proportion CIs) and how to bind
  externally-computed model results (MMRM/ANCOVA/Cox/logistic p-values,
  LS-means, CIs) onto an assembled table via
  [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)
  and
  [`as_display()`](https://github.com/mstackhouse/tplyr2/reference/as_display.md),
  plus where
  [`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
  fits.
- Clarified in
  [`?layer_settings`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
  and the denominators vignette that `denoms_by` **replaces** (does not
  augment) the default `cols`-based denominator grouping, so you must
  include the `cols` variable(s) to get per-column denominators (#19).
