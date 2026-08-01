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

  An
  [`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
  object. A bare character format string is rejected, since the variable
  names are what bind `...` to the format groups.

- ...:

  Numeric vectors, one per variable in the f_str (positional matching)

- precision:

  Optional list of resolved precision per group (for auto-precision)

- lt:

  Optional numeric less-than threshold applied to the group named by
  `lt_gt_group`: values in `(0, lt)` render as `"<" lt` (see
  [`format_number_vec()`](https://atorus-research.github.io/tplyr2/reference/format_number_vec.md)).

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

## See also

[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
for the format-string grammar.

## Examples

``` r
# Vectorized: one formatted string per element
apply_formats(f_str("xx (xx.x%)", "n", "pct"),
              n = c(5, 12, 103), pct = c(4.5, 33.333, 99.9))
#> [1] " 5 ( 4.5%)"  "12 (33.3%)"  "103 (99.9%)"

# `na` replaces the default blank-width fill
apply_formats(f_str("xx.x", "mean"), mean = c(1.2, NA))
#> [1] " 1.2" "    "
apply_formats(f_str("xx.x", "mean"), mean = c(1.2, NA), na = "NE")
#> [1] " 1.2" "NE"  
apply_formats(f_str("xx.x", "mean"), mean = c(1.2, NA), na = "")
#> [1] " 1.2" ""    

# lt/gt thresholds, targeting the percent group (index 2)
apply_formats(f_str("xx (xx.x%)", "n", "pct"), n = c(1, 199), pct = c(0.4, 99.7),
              lt = 1, gt = 99, lt_gt_group = 2)
#> [1] " 1 (<1.0%)"   "199 (>99.0%)"

# Pad to a fixed width for row-binding against other output
apply_formats(f_str("xx.x", "mean"), mean = c(1.2, 10.75), width = 10)
#> [1] " 1.2      " "10.8      "
apply_formats(f_str("xx.x", "mean"), mean = c(1.2, 10.75), width = 10, pad = "left")
#> [1] "       1.2" "      10.8"
```
