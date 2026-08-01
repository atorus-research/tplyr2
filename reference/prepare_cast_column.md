# Prepare the dcast column variable, respecting factor-level order

For a single column variable, converts it to a factor ordered by
`col_levels` so `dcast()` spreads columns in level order. For multiple
column variables, builds the `" | "`-joined interaction column and, when
any component is a factor, orders it by the cross-product of each
variable's level order (outermost variable varies slowest). When no
component is a factor the interaction is left as a character vector, so
`dcast()` falls back to alphabetical order exactly as before.

## Usage

``` r
prepare_cast_column(dt, cols, col_levels = NULL)
```

## Arguments

- dt:

  Long data.table about to be cast (mutated in place)

- cols:

  Character vector of column variable names

- col_levels:

  Named list from
  [`get_col_levels()`](https://atorus-research.github.io/tplyr2/reference/get_col_levels.md)
  (may be NULL/empty)

## Value

The name of the variable to use on the RHS of the dcast formula
