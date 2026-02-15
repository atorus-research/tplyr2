# Create a population data configuration

Configuration object specifying how population data maps to the spec.
The actual population data.frame is provided at build time via
`tplyr_build(spec, data, pop_data = ...)`.

## Usage

``` r
pop_data(cols, where = NULL)
```

## Arguments

- cols:

  Character vector of column variable names in the population data. If
  named, names are the spec column names and values are the pop_data
  column names (e.g., `c("TRTA" = "TRT01P")`). If unnamed, maps
  positionally to spec cols.

- where:

  Expression for filtering the population data (optional)

## Value

A tplyr_pop_data object
