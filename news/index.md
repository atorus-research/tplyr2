# Changelog

## tplyr2 0.1.0.9000

### New features

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

### Bug fixes

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
