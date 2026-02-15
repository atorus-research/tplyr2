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
