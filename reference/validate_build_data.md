# Validate data compatibility at build time

Checks that the columns referenced in the spec actually exist in the
data. Called after data conversion to data.table.

## Usage

``` r
validate_build_data(spec, dt)
```

## Arguments

- spec:

  A tplyr_spec object

- dt:

  A data.table

## Value

Invisible TRUE if valid
