# Which values a missing_count setting treats as missing

`NA`, plus anything named in `missing_values`. Shared by the Missing-row
computation and `denom_exclude` so the two cannot disagree about what
"missing" means.

## Usage

``` r
is_missing_value(x, missing_count)
```

## Arguments

- x:

  Vector of target-variable values

- missing_count:

  The layer's `missing_count` setting

## Value

Logical vector the same length as `x`
