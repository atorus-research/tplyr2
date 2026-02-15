# Resolve population data column mapping

Maps population data columns to spec columns. Handles named (explicit)
and unnamed (positional) mapping.

## Usage

``` r
resolve_pop_cols(pop_config, spec_cols)
```

## Arguments

- pop_config:

  A tplyr_pop_data object or NULL

- spec_cols:

  Character vector of spec-level column names

## Value

Character vector of column names to use in the population data
