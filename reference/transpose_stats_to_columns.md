# Transpose stats-as-columns

Transposes the standard wide output so that statistics become columns.
Without a `by` variable, treatment groups become the rows and stat names
become the columns. With a `by` variable, the by groups stay as rows and
each column is a treatment x statistic combination (issue \#20) — e.g.
one "Arm A \| Mean" and "Arm A \| n" column per treatment — so the by
dimension is preserved instead of being collapsed.

## Usage

``` r
transpose_stats_to_columns(wide)
```

## Arguments

- wide:

  data.table from standard desc processing

## Value

Transposed data.table
