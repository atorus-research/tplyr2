# Create a custom column group configuration

Combines existing column levels into a custom group. Rows matching any
of the source levels are duplicated with the column variable set to the
group name.

## Usage

``` r
custom_group(col_var, ...)
```

## Arguments

- col_var:

  Character string naming the column variable

- ...:

  Named arguments where names are group labels and values are character
  vectors of source levels to combine. Example:
  `"High Dose" = c("Dose 1", "Dose 2")`

## Value

A tplyr_custom_group object
