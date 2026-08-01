# Re-attach total/distinct_total denominators after grid completion

Grid completion introduces rows whose denominators come out NA from the
left join; refill them from the pre-completion counts, keyed by
denom_group (or `fallback_cols` when no denom_group applies).

## Usage

``` r
refill_denom_totals(result, counts, denom_group, fallback_cols)
```
