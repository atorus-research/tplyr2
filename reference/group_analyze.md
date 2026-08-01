# Create a custom analysis layer

Allows a user-defined function to compute summary statistics. The
function receives a data subset and the target variable name for each
group combination, and returns a data.frame of results.

## Usage

``` r
group_analyze(
  target_var,
  by = NULL,
  where = NULL,
  analyze_fn,
  settings = layer_settings()
)
```

## Arguments

- target_var:

  Character string naming the target variable(s)

- by:

  Character string or vector for row grouping

- where:

  Expression for filtering data for this layer

- analyze_fn:

  A function with signature `function(.data, .target_var)` that returns
  a data.frame. See Details.

- settings:

  A layer_settings object

## Value

A tplyr_analyze_layer object

## Details

The `analyze_fn` is called once per group combination (defined by `cols`
and `by` data variables). It receives:

- `.data`: A data.frame subset for the current group

- `.target_var`: Character string with the target variable name(s)

If `format_strings` are provided in settings, `analyze_fn` should return
a single-row data.frame of named numeric values. Each format string
entry becomes one output row, with its name used as the row label.

If no `format_strings` are provided, `analyze_fn` must return a
data.frame with `row_label` and `formatted` columns.

Note that `analyze_fn` is called once per `cols` x `by` combination, so
it only ever sees a single treatment column at a time — it cannot
compute a statistic *across* the treatment columns. For an omnibus
association test that spans the columns (e.g. Fisher's exact or CMH on a
count/shift layer), see
[`assoc_test`](https://atorus-research.github.io/tplyr2/reference/assoc_test.md).

## See also

[`assoc_test`](https://atorus-research.github.io/tplyr2/reference/assoc_test.md)
for cross-column association tests.

## Examples

``` r
# format_strings mode: the function returns one row of named numbers, and
# each format string becomes an output row.
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_analyze("AGE",
      analyze_fn = function(.data, .target_var) {
        v <- .data[[.target_var]]
        data.frame(gmean = exp(mean(log(v))), rng = diff(range(v)))
      },
      settings = layer_settings(format_strings = list(
        "Geometric mean" = f_str("xx.xx", "gmean"),
        "Range"          = f_str("xx", "rng")
      )))
  )
)
tplyr_build(spec, tplyr_adsl)
#>        rowlabel1  res1  res2  res3 ord_layer_1 ord_layer_index
#> 1 Geometric mean 74.70 73.94 75.18           1               1
#> 2          Range    37    32    37           2               1

# Pre-formatted mode: the function supplies row_label and formatted itself.
pre <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_analyze("AGE",
      analyze_fn = function(.data, .target_var) {
        v <- .data[[.target_var]]
        data.frame(
          row_label = "Median [IQR]",
          formatted = sprintf("%.1f [%.1f]", median(v), IQR(v))
        )
      })
  )
)
tplyr_build(pre, tplyr_adsl)
#>      rowlabel1        res1       res2        res3 ord_layer_1 ord_layer_index
#> 1 Median [IQR] 76.0 [12.5] 76.0 [9.2] 77.5 [11.0]           1               1
```
