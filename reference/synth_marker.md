# Name of the per-column-variable synthetic-row marker

Records which column variable a duplicated row was created for, so a
total group can skip copies made on its own variable while still
spanning copies made for a different one.

## Usage

``` r
synth_marker(col_var)
```

## Arguments

- col_var:

  Character(1) column variable name

## Value

Character(1) marker column name
