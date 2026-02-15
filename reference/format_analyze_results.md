# Format analyze results using format_strings

For each group combination, takes the first row of numeric values from
the analyze_fn output and creates one formatted row per format_string
entry.

## Usage

``` r
format_analyze_results(fn_combined, format_strings, group_vars)
```

## Arguments

- fn_combined:

  data.table of raw analyze_fn results

- format_strings:

  Named list of f_str objects

- group_vars:

  Character vector of grouping column names

## Value

data.table with row_label and formatted columns
