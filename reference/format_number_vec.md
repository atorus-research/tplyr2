# Format a numeric vector to a fixed-width field

Format a numeric vector to a fixed-width field

## Usage

``` r
format_number_vec(values, group, precision = NULL, lt = NULL, gt = NULL)
```

## Arguments

- values:

  Numeric vector to format

- group:

  A parsed format group (from
  [`parse_format_group()`](https://github.com/mstackhouse/tplyr2/reference/parse_format_group.md))

- precision:

  Optional resolved precision (int_width/dec_width) overriding the
  group's static widths

- lt:

  Optional numeric less-than threshold. Values strictly greater than 0
  whose rounded display would fall below `lt` render as `"<" lt` (e.g. a
  percent of 0.4 renders as `<1`), right-justified to the field width.
  Used for the regulatory "\<1%" convention on count-layer percents.

- gt:

  Optional numeric greater-than threshold. Values strictly less than 100
  whose rounded display would exceed `gt` render as `">" gt` (e.g. 99.6
  renders as `>99`).
