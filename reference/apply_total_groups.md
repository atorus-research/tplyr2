# Apply total groups to data

Duplicates all rows with the column variable set to the total group
label, creating a synthetic "Total" column level.

## Usage

``` r
apply_total_groups(dt, total_groups)
```

## Arguments

- dt:

  data.table

- total_groups:

  List of tplyr_total_group objects (or NULL)

## Value

Modified data.table
