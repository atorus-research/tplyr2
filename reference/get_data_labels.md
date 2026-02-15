# Extract variable labels from a data.frame

Returns a named character vector of variable labels. Labels are
extracted from the `"label"` attribute of each column (standard for
Haven-imported CDISC data).

## Usage

``` r
get_data_labels(data)
```

## Arguments

- data:

  A data.frame

## Value

Named character vector where names are column names and values are
labels. Columns without labels return `NA_character_`.
