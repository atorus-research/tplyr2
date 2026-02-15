# Get or set tplyr2 package options

View and modify tplyr2-specific options. When called with no arguments,
returns all current tplyr2 options with their defaults. When called with
named arguments, sets those options.

## Usage

``` r
tplyr2_options(...)
```

## Arguments

- ...:

  Named arguments to set (e.g., `IBMRounding = TRUE`). Option names are
  automatically prefixed with `tplyr2.`.

## Value

When called with no arguments, a named list of current option values.
When called with arguments, invisibly returns the previous values.

## Details

Available options:

- tplyr2.IBMRounding:

  Logical. Use round-half-away-from-zero instead of R's default banker's
  rounding. Default: `FALSE`.

- tplyr2.quantile_type:

  Integer. Quantile algorithm type passed to
  [`quantile()`](https://rdrr.io/r/stats/quantile.html). Default: `7`.

- tplyr2.precision_cap:

  Named numeric vector `c(int=, dec=)`. Maximum int/dec widths for
  auto-precision. Default: `NULL`.

- tplyr2.custom_summaries:

  Named list of expressions for global custom summary functions.
  Default: `NULL`.

- tplyr2.scipen:

  Integer. scipen value used during
  [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
  to prevent scientific notation. Default: `9999`.
