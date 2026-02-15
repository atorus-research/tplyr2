# Resolve auto-precision widths for a format group

Given a parsed format group and collected precision values, returns the
effective integer width and decimal width.

## Usage

``` r
resolve_precision(group, max_int, max_dec)
```

## Arguments

- group:

  A parsed format group (from parse_format_group)

- max_int:

  Integer, the collected max integer width

- max_dec:

  Integer, the collected max decimal precision

## Value

list(int_width, dec_width)
