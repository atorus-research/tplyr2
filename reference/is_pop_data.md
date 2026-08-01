# Check if an object is a tplyr_pop_data

Check if an object is a tplyr_pop_data

## Usage

``` r
is_pop_data(x)
```

## Arguments

- x:

  An object to check

## Value

Logical

## Examples

``` r
is_pop_data(pop_data(cols = "TRT01P"))
#> [1] TRUE
is_pop_data("TRT01P")
#> [1] FALSE
```
