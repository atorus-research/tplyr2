# Translate a group value to filter expressions

When a column value corresponds to a total group or custom group label,
translates back to the appropriate filter. Total groups produce no
filter (all values pass). Custom groups produce a `%in%` filter with
component values.

## Usage

``` r
translate_group_value(value, col_var, total_groups, custom_groups)
```

## Arguments

- value:

  The column value from the output (e.g., "Total", "Active")

- col_var:

  The column variable name

- total_groups:

  List of tplyr_total_group objects

- custom_groups:

  List of tplyr_custom_group objects

## Value

A list with `filters` (list of call expressions) and `is_total`
(logical)
