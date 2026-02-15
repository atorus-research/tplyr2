# Create a total group configuration

Specifies that a synthetic "Total" column level should be added by
duplicating all rows with the specified column variable set to the
label.

## Usage

``` r
total_group(col_var, label = "Total")
```

## Arguments

- col_var:

  Character string naming the column variable to totalize

- label:

  Character string for the total group label (default: "Total")

## Value

A tplyr_total_group object
