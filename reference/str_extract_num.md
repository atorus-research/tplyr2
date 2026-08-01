# Extract numeric values from formatted strings

Extracts the Nth numeric value from a formatted tplyr2 string.

## Usage

``` r
str_extract_num(x, index = 1L)
```

## Arguments

- x:

  Character vector of formatted strings

- index:

  Integer, which numeric value to extract (1-based)

## Value

Numeric vector

## Examples

``` r
cells <- c(" 22 (25.6%)", "  9 (10.5%)", " 55 (64.0%)")

# Default pulls the count; index = 2 pulls the percent
str_extract_num(cells)
#> [1] 22  9 55
str_extract_num(cells, index = 2)
#> [1] 25.6 10.5 64.0

# Asking past the available numbers gives NA, as does an NA input
str_extract_num(c(" 5", NA), index = 2)
#> [1] NA NA

# Recover a sort key from an already-formatted table
built <- tplyr_build(
  tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("AGEGR1"))),
  tplyr_adsl
)
built[order(-str_extract_num(built$res1)), c("rowlabel1", "res1")]
#>   rowlabel1       res1
#> 2     65-80 42 (48.8%)
#> 3       >80 30 (34.9%)
#> 1       <65 14 (16.3%)
```
