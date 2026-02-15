# Apply build-time overrides to a spec

Merges override parameters into a copy of the spec. Handles special
cases:

- `where`: If character, parsed to an expression via
  [`rlang::parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.html).

- `pop_data`: If a list, sub-fields are merged into the existing
  pop_data config rather than replacing it entirely.

## Usage

``` r
apply_overrides(spec, overrides)
```

## Arguments

- spec:

  A tplyr_spec object

- overrides:

  Named list of override values

## Value

Modified spec (shallow copy)
