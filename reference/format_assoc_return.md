# Render an `assoc_test` `fn` return value for display

Turns the value returned by a caller-supplied `fn` into the string shown
in the `pval` cell:

- a **numeric** (or logical) vector whose length equals the number of
  variables in `format` is mapped positionally onto the format and
  rendered into one cell – a scalar with a one-variable format (a
  p-value), or several values with a multi-variable format (issue \#60),
  e.g. an odds ratio with a confidence interval. An all-`NA` return (or
  an arity that does not match the format) renders a blank; a single
  `NA` field within a longer return blanks just that field via the
  `f_str` grammar;

- a length-1 **character** is passed through verbatim (issue \#47), so a
  caller computing an arbitrary test can also supply the finished
  display (significance flags, `">.99"`/`"<.0001"` ceilings, `"NE"`
  sentinels, trailing-space alignment); `NA_character_` renders a blank;

- anything else (non-atomic, mismatched length) renders a blank.

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
  object; its variable count determines how many values a numeric return
  must supply.

## Value

A length-1 character display string.
