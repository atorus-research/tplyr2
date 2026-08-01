# Rename ordering columns to the public output names

Renames `ordindx` to `ord_layer_index` and `ord1`/`ord2`/... to
`ord_layer_1`/`ord_layer_2`/...

## Usage

``` r
rename_ord_columns(result)
```

## Arguments

- result:

  data.table

## Value

Modified data.table (by reference)
