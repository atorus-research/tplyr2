# Round numbers with optional IBM rounding

When `getOption("tplyr2.IBMRounding", FALSE)` is TRUE, uses
round-half-away-from-zero (IBM convention) instead of R's default
banker's rounding (round half to even).

## Usage

``` r
tplyr_round(x, digits = 0)
```

## Arguments

- x:

  Numeric vector

- digits:

  Number of decimal places

## Value

Numeric vector
