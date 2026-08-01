# Build one pairwise 2x2 and render its display string

Shared by the single-level and nested pairwise paths. Builds
`matrix(c(n_ref, n_cmp, N_ref - n_ref, N_cmp - n_cmp), nrow = 2)`, calls
`config$fn` on it, and renders the return via
[`format_assoc_return`](https://atorus-research.github.io/tplyr2/reference/format_assoc_return.md).
A missing count/denominator or a zero denominator renders a blank (no
test).

## Usage

``` r
pairwise_cell_disp(n_ref, n_cmp, N_ref, N_cmp, config, group = NULL)
```

## Arguments

- n_ref, n_cmp:

  Event counts for the reference and comparison arm.

- N_ref, N_cmp:

  Population denominators for the reference and comparison arm.

- config:

  A `tplyr_assoc_test` object (pairwise mode).

- group:

  Optional group identifier used when reporting a failure of
  `config$fn`.

## Value

A length-1 character display string.
