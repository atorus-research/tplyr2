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
  [`tplyr_build()`](https://atorus-research.github.io/tplyr2/reference/tplyr_build.md)
  to prevent scientific notation. Default: `9999`.

An unrecognized option name is an error rather than a silent no-op, so a
misspelling cannot quietly leave the build on the default behavior.

## Examples

``` r
# Inspect the current values
tplyr2_options()
#> $tplyr2.IBMRounding
#> [1] FALSE
#> 
#> $tplyr2.quantile_type
#> [1] 7
#> 
#> $tplyr2.precision_cap
#> NULL
#> 
#> $tplyr2.custom_summaries
#> NULL
#> 
#> $tplyr2.scipen
#> [1] 9999
#> 

# Setting returns the previous values, so the change can be undone
old <- tplyr2_options(IBMRounding = TRUE)
getOption("tplyr2.IBMRounding")
#> [1] TRUE
do.call(options, old)
getOption("tplyr2.IBMRounding")
#> NULL

# IBM (half-away-from-zero) rounding vs R's banker's rounding
fmt <- f_str("xx", "n")
apply_formats(fmt, n = 2.5)
#> [1] " 2"
old <- tplyr2_options(IBMRounding = TRUE)
apply_formats(fmt, n = 2.5)
#> [1] " 3"
do.call(options, old)

# A misspelled name errors instead of setting a dead option
try(tplyr2_options(IBMrounding = TRUE))
#> Error : Unknown option: IBMrounding
#> Valid options: IBMRounding, quantile_type, precision_cap, custom_summaries, scipen
```
