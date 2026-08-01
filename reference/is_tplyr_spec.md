# Check if an object is a tplyr_spec

Check if an object is a tplyr_spec

## Usage

``` r
is_tplyr_spec(x)
```

## Arguments

- x:

  An object to check

## Value

Logical

## Examples

``` r
spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("SEX")))
is_tplyr_spec(spec)
#> [1] TRUE
is_tplyr_spec(mtcars)
#> [1] FALSE
```
