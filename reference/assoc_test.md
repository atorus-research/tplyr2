# Association-test column for count and shift layers

Configures an omnibus association test that runs once per `by` group,
*across* the treatment columns, and lands its formatted result as a
single trailing column beside the n(\\ the raw source-data subset for
the `by` group (all `cols` levels and all target/row levels), so a
caller can tabulate and test naturally (e.g.
`fisher.test(table(.data$TRT, .data$RESP))` or `coin::cmh_test(...)`).

## Usage

``` r
assoc_test(fn, format = f_str("x.xxx", "p"), label = "p-value")
```

## Arguments

- fn:

  A function of one argument. It is called with the source-data subset
  (a data.frame) for a single `by` group and must return a single
  numeric value (typically a p-value). Return `NA` to render a blank.

- format:

  An [`f_str`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  object formatting the returned value. The f_str must reference a
  single variable (any name; the returned scalar is passed
  positionally). Defaults to `f_str("x.xxx", "p")`.

- label:

  Character string used as the result column's header label. Defaults to
  `"p-value"`.

## Value

A `tplyr_assoc_test` object.

## Details

Attach it to a count or shift layer via
`layer_settings(assoc_test = assoc_test(...))`. When the layer has no
`by` variable the test runs once over the whole layer; otherwise once
per `by` group, with the value placed on that group's first output row.

## Examples

``` r
at <- assoc_test(
  fn = function(.data) fisher.test(table(.data$TRT, .data$RESP))$p.value,
  format = f_str("x.xxx", "p"),
  label = "p-value [1]"
)
```
