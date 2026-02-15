# Apply precision caps

Applies layer-level cap first, falls back to global option.

## Usage

``` r
apply_precision_cap(prec, precision_cap = NULL)
```

## Arguments

- prec:

  data.table with max_int and max_dec columns

- precision_cap:

  Named numeric vector c(int=, dec=) or NULL

## Value

Modified data.table
