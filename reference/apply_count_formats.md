# Apply count format(s) to a long counts table

Legacy mode (single unnamed format) writes a single `formatted` column,
preserving pre-stat_columns behavior exactly. Stat-columns mode (named
formats) writes one `formatted_<i>` column per format;
[`cast_to_wide()`](https://github.com/mstackhouse/tplyr2/reference/cast_to_wide.md)
spreads these into separate res columns per column group. All count
statistics are already present on `dt` (including the special total and
missing row tables), so every format can be applied to every row.

## Usage

``` r
apply_count_formats(dt, fmts)
```

## Arguments

- dt:

  data.table with computed count statistics (or NULL, a no-op)

- fmts:

  List of f_str objects from
  [`get_count_formats()`](https://github.com/mstackhouse/tplyr2/reference/get_count_formats.md)
