# Zero-fill synthetic markers after a bind

`rbindlist(fill = TRUE)` sets a marker absent from one side to NA; those
rows are originals for that variable.

## Usage

``` r
fill_synth_markers(dt)
```

## Arguments

- dt:

  data.table

## Value

`dt`
