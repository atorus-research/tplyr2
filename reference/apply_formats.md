# Apply format strings to numeric values

Vectorized formatting function. Takes an f_str object and numeric
vectors, returns a character vector of formatted strings.

## Usage

``` r
apply_formats(
  fmt,
  ...,
  precision = NULL,
  lt = NULL,
  gt = NULL,
  lt_gt_group = NULL,
  na = NULL,
  width = NULL,
  pad = c("right", "left")
)
```

## Arguments

- fmt:

  An f_str object or character format string

- ...:

  Numeric vectors, one per variable in the f_str (positional matching)

- precision:

  Optional list of resolved precision per group (for auto-precision)

- lt:

  Optional numeric less-than threshold applied to the group named by
  `lt_gt_group`: values in `(0, lt)` render as `"<" lt` (see
  [`format_number_vec()`](https://github.com/mstackhouse/tplyr2/reference/format_number_vec.md)).

- gt:

  Optional numeric greater-than threshold applied to the group named by
  `lt_gt_group`: values in `(gt, 100)` render as `">" gt`.

- lt_gt_group:

  Optional integer index of the format group to which `lt`/`gt` apply
  (used by count layers to target the percent statistic). NULL disables.

- na:

  Optional string substituted for cells whose format-group inputs are
  all NA, used *instead of* the default blank-width fill. `na = ""`
  produces a truly empty cell (`nchar` 0); `na = "NE"` renders `"NE"`.
  The default `NULL` preserves the blank-width fill. This lets
  `apply_formats()` replace hand-rolled fixed-width formatters for
  externally row-bound statistics.

- width:

  Optional integer total width to pad each formatted token to, using
  [`stringr::str_pad()`](https://stringr.tidyverse.org/reference/str_pad.html).
  When the `na` substitution applies to a cell, `na` wins and that cell
  is *not* padded. The default `NULL` leaves tokens at their natural
  format width.

- pad:

  Side to pad on when `width` is set: `"right"` (default, trailing
  spaces) or `"left"` (leading spaces).

## Value

Character vector of formatted values
