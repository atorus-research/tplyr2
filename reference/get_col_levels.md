# Ordered factor levels for the column variable(s)

Returns a named list mapping each `cols` variable that is a factor in
`source_dt` to its level order. Non-factor column variables are omitted.
Used to preserve the column variable's factor-level order through the
`dcast()` in
[`cast_to_wide()`](https://atorus-research.github.io/tplyr2/reference/cast_to_wide.md)
(issue \#13), so count/shift/desc layers all order their `res*` columns
by factor levels rather than alphabetically.

## Usage

``` r
get_col_levels(source_dt, cols, complete = FALSE)
```

## Arguments

- source_dt:

  data.table with the original (factor-typed) input data

- cols:

  Character vector of column variable names
