# Attach single-proportion CI columns to a long count table

Adds `ci_lower`/`ci_upper` (from `n`/`total`) and, when the distinct
columns are present, `distinct_ci_lower`/`distinct_ci_upper` (from
`distinct_n`/`distinct_total`). Bounds are stored on the **percentage
scale** (proportion times 100) to match the `pct`/`distinct_pct`
statistics.

## Usage

``` r
add_count_ci(dt, settings)
```

## Arguments

- dt:

  A long count data.table (or NULL, a no-op)

- settings:

  A tplyr_layer_settings object supplying `ci_method`/`ci_level`

## Value

`dt`, modified in place
