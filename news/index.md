# Changelog

## tplyr2 0.1.0.9000

### New features

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
