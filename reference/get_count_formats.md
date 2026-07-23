# Resolve the list of count formats for a layer

Returns the `stat_columns` list when set (one result column per format
per column group); otherwise a single-element unnamed list wrapping the
legacy format so callers can treat both modes uniformly. The presence of
names on the result signals stat-columns mode downstream.

## Usage

``` r
get_count_formats(settings)
```
