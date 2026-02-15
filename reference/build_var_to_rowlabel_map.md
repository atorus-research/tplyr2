# Map data variable names to their rowlabel columns

Map data variable names to their rowlabel columns

## Usage

``` r
build_var_to_rowlabel_map(layer, by_data_vars, by_labels, col_names)
```

## Arguments

- layer:

  A tplyr_layer object

- by_data_vars:

  Character vector of by-variable data column names

- by_labels:

  Character vector of by-variable label strings

- col_names:

  Character vector of original data column names

## Value

Named list where names are data variables and values are rowlabel column
names (e.g., `list(SEX = "rowlabel1")`)
