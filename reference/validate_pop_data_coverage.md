# Warn when pop_data does not cover the analysis data's column levels

A column level present in the analysis data but absent from the
population (an arm recoded `"Xanomeline High Dose"` vs `"High Dose"`)
yields an NA denominator for every one of its cells.

## Usage

``` r
validate_pop_data_coverage(dt, pop_dt, cols)
```

## Arguments

- dt:

  Analysis data.table

- pop_dt:

  Population data.table, or NULL

- cols:

  Character vector of column variables

## Value

Invisible TRUE
