# Percentage of a count against its denominator

The one 0-vs-NA convention for the whole package. Without a usable
denominator the percentage is undefined, so it is NA and renders blank —
count and shift layers used to render `0`, which for `n > 0` is an
affirmatively wrong number, while desc layers already used NA.

## Usage

``` r
safe_pct(n, total)
```

## Arguments

- n:

  Numeric vector of counts

- total:

  Numeric vector of denominators

## Value

Numeric vector of percentages, NA where the denominator is NA or 0
