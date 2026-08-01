# Check if an object is a tplyr_layer

Check if an object is a tplyr_layer

## Usage

``` r
is_tplyr_layer(x)
```

## Arguments

- x:

  Object to check

## Value

Logical

## Examples

``` r
is_tplyr_layer(group_count("SEX"))
#> [1] TRUE
is_tplyr_layer(group_desc("AGE"))
#> [1] TRUE
is_tplyr_layer("SEX")
#> [1] FALSE
```
