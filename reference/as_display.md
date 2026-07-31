# Extract a display-ready frame from a build result

Returns just the display content of a
[`tplyr_build`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
result, dropping the internal ordering helpers (`ord_layer_index`,
`ord_layer_*`) and the `row_id` metadata column, and giving a frame
ready to hand to a table-rendering package (clinify, flextable, gt,
...). The build output is already ordered, so no re-sorting is
performed.

## Usage

``` r
as_display(x, labels = FALSE)
```

## Arguments

- x:

  A data.frame produced by
  [`tplyr_build`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md).

- labels:

  Logical. When `TRUE`, the `res*` / `rdiff*` / `pval*` columns are
  renamed to their header labels (their `label` attribute, as returned
  by
  [`get_data_labels`](https://github.com/mstackhouse/tplyr2/reference/get_data_labels.md));
  the row-label columns keep their names. Defaults to `FALSE`.

## Value

A data.frame of display columns.

## Details

Everything that is not an internal helper is kept. That is normally the
`rowlabel*`, `res*`, `rdiff*`, and `pval*` columns, but a
`stats_as_columns` desc layer with no `by` variable names its result
columns after the statistics themselves, and those are retained too.

## Examples

``` r
spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("SEX")))
b <- tplyr_build(spec, data.frame(TRT = rep(c("A", "B"), 3),
                                  SEX = rep(c("F", "M"), 3)))
as_display(b)
#>   rowlabel1        res1        res2
#> 1         F  3 (100.0%)   0 ( 0.0%)
#> 2         M   0 ( 0.0%)  3 (100.0%)
```
