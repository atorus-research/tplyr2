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
  lt_gt_group = NULL
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

## Value

Character vector of formatted values
