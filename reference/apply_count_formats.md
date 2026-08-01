# Apply count format(s) to a long counts table

Single-format mode (unnamed) writes a single `formatted` column.
Stat-columns mode (named formats) writes one `formatted_<i>` column per
format;
[`cast_to_wide()`](https://github.com/mstackhouse/tplyr2/reference/cast_to_wide.md)
spreads these into separate res columns per column group. All count
statistics are already present on `dt` (including the special total and
missing row tables), so every format can be applied to every row.

## Usage

``` r
apply_count_formats(
  dt,
  fmts,
  pct_lt = NULL,
  pct_gt = NULL,
  zero_count_display = "full"
)
```

## Arguments

- dt:

  data.table with computed count statistics (or NULL, a no-op)

- fmts:

  List of f_str objects from
  [`get_count_formats()`](https://github.com/mstackhouse/tplyr2/reference/get_count_formats.md)

- pct_lt:

  Optional numeric less-than threshold for percents (see
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md))

- pct_gt:

  Optional numeric greater-than threshold for percents

- zero_count_display:

  One of "full" (default, unchanged), "count_only" (zero cells show just
  the count field, e.g. " 0"), or "blank" (zero cells render as "")

## Details

The `pct_lt`/`pct_gt` and `zero_count_display` arguments implement the
regulatory display conventions from issue \#14. `pct_lt`/`pct_gt`
retarget the percent statistic (the `pct`/`distinct_pct` format group)
to the "\<1"/"\>99" tokens. `zero_count_display` rewrites cells whose
count is zero.
