# Record a failure in user-supplied code

A no-op outside
[`collect_user_fn_errors()`](https://atorus-research.github.io/tplyr2/reference/collect_user_fn_errors.md),
so helpers stay callable from tests and from code paths that run outside
a build.

## Usage

``` r
record_user_fn_error(label, cond, group = NULL)
```

## Arguments

- label:

  What failed, e.g. `"custom summary 'geo_mean'"`

- cond:

  The condition caught

- group:

  Optional human-readable group identifier, e.g.
  `"SEX = F, TRT01P = Placebo"`

## Value

Invisible NULL, called for its side effect
