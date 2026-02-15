# Apply custom column groups to data

Duplicates rows matching source levels with the column variable set to
the custom group name.

## Usage

``` r
apply_custom_groups(dt, custom_groups)
```

## Arguments

- dt:

  data.table

- custom_groups:

  List of tplyr_custom_group objects (or NULL)

## Value

Modified data.table
