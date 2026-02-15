# Collect precision from data

Scans a numeric variable to determine the maximum integer width and
maximum decimal precision present in the data, optionally grouped.

## Usage

``` r
collect_precision(
  dt,
  precision_on,
  precision_by = character(0),
  precision_data = NULL,
  precision_cap = NULL
)
```

## Arguments

- dt:

  data.table with the data

- precision_on:

  Character string naming the variable to scan

- precision_by:

  Character vector of grouping variables (can be empty)

- precision_data:

  Optional external data.frame with pre-computed precision

- precision_cap:

  Named numeric vector c(int=, dec=) for capping

## Value

data.table with precision_by columns + "max_int" and "max_dec" columns
