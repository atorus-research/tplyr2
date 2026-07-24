# tplyr2 0.1.0.9000

## New features

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
- `group_desc(stats_as_columns = TRUE)` combined with a `by` variable no longer
  drops the by-groups and returns only the last group's statistics (#20). It
  now keeps the by-groups as rows and produces one result column per
  treatment x statistic (labelled `"<arm> | <stat>"`). Behavior without a `by`
  variable (treatment groups as rows, statistics as columns) is unchanged.

## Documentation

- Clarified in `?layer_settings` and the denominators vignette that `denoms_by`
  **replaces** (does not augment) the default `cols`-based denominator
  grouping, so you must include the `cols` variable(s) to get per-column
  denominators (#19).
