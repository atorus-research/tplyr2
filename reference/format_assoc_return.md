# Render an `assoc_test` `fn` return value for display

Turns a single value returned by a caller-supplied `fn` into the string
shown in the `pval` cell:

- a length-1 **numeric** (or logical) is formatted with `config$format`;
  `NA` renders a blank;

- a length-1 **character** is passed through verbatim (issue \#47), so a
  caller computing an arbitrary test can also supply the finished
  display (significance flags, `">.99"`/`"<.0001"` ceilings, `"NE"`
  sentinels, trailing-space alignment); `NA_character_` renders a blank;

- anything else (wrong length, non-atomic) renders a blank.

## Usage

``` r
format_assoc_return(raw, format)
```

## Arguments

- raw:

  The raw value returned by `fn` (already wrapped so errors arrive as
  `NA`).

- format:

  An [`f_str`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  object used for numeric returns.

## Value

A length-1 character display string.
