# Zero-fill count statistics left NA by grid completion or merges

Counts left NA by grid completion are genuine zeros. Percentages are
not: a percentage is only zero when a usable denominator says so, and
filling one whose denominator is missing or zero would print a number
that was never computed (#76). Those stay NA and render blank.

## Usage

``` r
zero_fill_stats(dt)
```

## Arguments

- dt:

  data.table to modify by reference

## Value

`dt`, invisibly
