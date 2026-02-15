# Validate a tplyr_spec object structurally

Checks that the spec has the correct class and structure. Called after
overrides are applied but before data is processed.

## Usage

``` r
validate_spec(spec)
```

## Arguments

- spec:

  A tplyr_spec object

## Value

Invisible TRUE if valid, otherwise stops with informative error
