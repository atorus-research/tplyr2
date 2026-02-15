# Custom Analysis Layers

## Introduction

tplyr2 provides three built-in layer types –
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md),
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md),
and
[`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md)
– that cover most clinical table patterns. But sometimes you need
computations that do not fit neatly into any of these: geometric means,
custom ratios, composite endpoints, or any analysis where you need full
control over both the calculation and the presentation.

[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
fills this gap. It lets you supply your own function to compute summary
statistics for each group, while preserving tplyr2’s column-based
layout, formatting, and ordering infrastructure. This vignette covers
both of its operating modes: format strings mode (where tplyr2 handles
formatting) and pre-formatted mode (where your function returns
display-ready strings).

## The analyze_fn Contract

Every
[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
layer requires an `analyze_fn` – a function that tplyr2 calls once for
each combination of column and `by` variables. The function signature
is:

``` r
function(.data, .target_var) { ... }
```

where `.data` is a data.frame subset for the current group and
`.target_var` is a character string naming the target variable.

What your function returns determines which of the two modes tplyr2 uses
to process the results.

## Format Strings Mode

In format strings mode, your `analyze_fn` returns a single-row
data.frame of named numeric columns. You then supply `format_strings` in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
to control how each statistic is formatted and labeled in the output.
This mode is useful when you want tplyr2’s formatting system (alignment,
decimal precision, parenthesis hugging) to handle the display.

Here is an example computing a geometric mean and geometric standard
deviation for urate lab values by treatment group:

``` r
geo_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  pos_vals <- vals[!is.na(vals) & vals > 0]
  data.frame(
    geo_mean = exp(mean(log(pos_vals))),
    geo_sd   = exp(sd(log(pos_vals)))
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT == "Baseline",
      analyze_fn = geo_fn,
      settings = layer_settings(
        format_strings = list(
          "Geometric Mean" = f_str("xxx.xx", "geo_mean"),
          "Geometric SD"   = f_str("xxx.xx", "geo_sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1      | rowlabel2      | res1   | res2   | res3   |
|:---------------|:---------------|:-------|:-------|:-------|
| Urate (umol/L) | Geometric Mean | 314.12 | 300.59 | 291.59 |
| Urate (umol/L) | Geometric SD   | 1.29   | 1.19   | 1.32   |

A few things to note:

- The names of the `format_strings` list (“Geometric Mean”, “Geometric
  SD”) become the row labels in the output, just like
  [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
  format strings.
- Each
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  references a column name from the data.frame your function returns.
  The `"xxx.xx"` template means three integer digits and two decimal
  places.
- The `by = "Urate (umol/L)"` string does not match any column in the
  data, so tplyr2 treats it as a text label that appears as an outer row
  label. This is useful for labeling blocks of custom statistics.
- The `where` argument filters the data before your function is called.

### Multiple Statistics Per Row

Format strings can combine multiple statistics into a single row, just
as in
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md).
Here we compute a mean and standard deviation in one row, and a median
in another:

``` r
summary_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  vals <- vals[!is.na(vals)]
  data.frame(
    n      = length(vals),
    mean   = mean(vals),
    sd     = sd(vals),
    median = median(vals)
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT == "Baseline",
      analyze_fn = summary_fn,
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xxx.x (xxx.xx)", "mean", "sd"),
          "Median"    = f_str("xxx.xx", "median")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1      | rowlabel2 | res1           | res2           | res3           |
|:---------------|:----------|:---------------|:---------------|:---------------|
| Urate (umol/L) | Mean (SD) | 323.4 ( 85.66) | 305.1 ( 61.56) | 301.6 ( 85.35) |
| Urate (umol/L) | Median    | 288.48         | 294.43         | 279.56         |
| Urate (umol/L) | n         | 8              | 10             | 7              |

This produces the same style of output you would get from
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md),
but computed by your own function.

## Pre-Formatted Mode

Sometimes you want complete control over the output strings – for
example, when the formatting logic is complex, when you need conditional
formatting, or when the output does not map cleanly to the
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
system. In pre-formatted mode, your `analyze_fn` returns a data.frame
with two columns: `row_label` (character) and `formatted` (character).
No `format_strings` are needed in this case.

``` r
range_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  vals <- vals[!is.na(vals)]
  data.frame(
    row_label = c("Range", "Ratio (Max/Min)"),
    formatted = c(
      sprintf("%.1f - %.1f", min(vals), max(vals)),
      sprintf("%.2f", max(vals) / min(vals))
    )
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT == "Baseline",
      analyze_fn = range_fn
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1      | rowlabel2       | res1          | res2          | res3          |
|:---------------|:----------------|:--------------|:--------------|:--------------|
| Urate (umol/L) | Range           | 232.0 - 469.9 | 249.8 - 469.9 | 190.3 - 428.3 |
| Urate (umol/L) | Ratio (Max/Min) | 2.03          | 1.88          | 2.25          |

Each value in the `row_label` column becomes a row label in the output.
Each corresponding `formatted` value is placed into the appropriate
result column. This mode gives you full control, but it also means you
are responsible for alignment and spacing – tplyr2 will not pad or round
the strings for you.

## Integration with by Variables

The `by` parameter in
[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
works the same way as in other layer types. Strings that match column
names in the data are treated as grouping variables; strings that do not
match are treated as text labels. Your `analyze_fn` is called once for
each unique combination of column variables and `by` data variables.

Here we compute statistics separately for each visit:

``` r
mean_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  vals <- vals[!is.na(vals)]
  data.frame(
    row_label = "Mean (SD)",
    formatted = sprintf("%.1f (%.2f)", mean(vals), sd(vals))
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = c("Urate (umol/L)", "AVISIT"),
      where = AVISIT %in% c("Baseline", "Week 4", "Week 8"),
      analyze_fn = mean_fn
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1      | rowlabel2 | rowlabel3 | res1          | res2          | res3          |
|:---------------|:----------|:----------|:--------------|:--------------|:--------------|
| Urate (umol/L) | Baseline  | Mean (SD) | 323.4 (85.66) | 305.1 (61.56) | 301.6 (85.35) |
| Urate (umol/L) | Week 4    | Mean (SD) | 313.0 (64.76) | 319.7 (72.55) | 319.2 (95.14) |
| Urate (umol/L) | Week 8    | Mean (SD) | 315.2 (60.66) | 290.5 (42.80) | 255.8 (38.55) |

In this output, `rowlabel1` contains the text label “Urate (umol/L)”,
`rowlabel2` contains the visit values from the data, and `rowlabel3`
contains the row labels from the function output. The `by` parameter can
mix labels and data variables freely – tplyr2 sorts them out
automatically.

## Combining with Other Layers

A
[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
layer integrates naturally into a multi-layer spec alongside
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md),
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md),
or
[`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md).
Each layer gets its own `ord_layer_index` value in the output, so layers
stack in the order they are specified.

``` r
geo_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  pos_vals <- vals[!is.na(vals) & vals > 0]
  data.frame(
    geo_mean = exp(mean(log(pos_vals)))
  )
}

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX",
      by = "Gender",
      settings = layer_settings(
        format_strings = list(
          "n (%)" = f_str("xx (xx.x%)", "n", "pct")
        )
      )
    ),
    group_analyze("AGE",
      by = "Age (years)",
      analyze_fn = geo_fn,
      settings = layer_settings(
        format_strings = list(
          "Geometric Mean" = f_str("xx.xx", "geo_mean")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1   | rowlabel2      | res1       | res2       | res3       |
|:------------|:---------------|:-----------|:-----------|:-----------|
| Gender      | F              | 53 (61.6%) | 40 (47.6%) | 50 (59.5%) |
| Gender      | M              | 33 (38.4%) | 44 (52.4%) | 34 (40.5%) |
| Age (years) | Geometric Mean | 74.70      | 73.94      | 75.18      |

The count layer and the analyze layer each occupy their own block of
rows. The `ord_layer_index` column (hidden from the kable output above
but present in the data) keeps them ordered correctly.

## Error Handling

If your `analyze_fn` encounters an error for a particular group – for
example, if a group has no valid data for a log transformation – tplyr2
will surface the error immediately. This is by design: silent failures
in statistical computation can lead to subtle, hard-to-detect problems
in clinical outputs.

To handle edge cases gracefully, build the error handling into your
function:

``` r
safe_geo_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  pos_vals <- vals[!is.na(vals) & vals > 0]

  if (length(pos_vals) < 2) {
    return(data.frame(geo_mean = NA_real_, geo_sd = NA_real_))
  }

  data.frame(
    geo_mean = exp(mean(log(pos_vals))),
    geo_sd   = exp(sd(log(pos_vals)))
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT == "Baseline",
      analyze_fn = safe_geo_fn,
      settings = layer_settings(
        format_strings = list(
          "Geometric Mean" = f_str("xxx.xx", "geo_mean"),
          "Geometric SD"   = f_str("xxx.xx", "geo_sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1      | rowlabel2      | res1   | res2   | res3   |
|:---------------|:---------------|:-------|:-------|:-------|
| Urate (umol/L) | Geometric Mean | 314.12 | 300.59 | 291.59 |
| Urate (umol/L) | Geometric SD   | 1.29   | 1.19   | 1.32   |

When `NA_real_` values pass through format strings, tplyr2 renders them
as blank space of the appropriate width, maintaining alignment in the
output.

## Summary

[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
is the escape hatch for when tplyr2’s built-in layer types are not
enough. It gives you full control over computation while preserving the
structural benefits of the tplyr2 framework: column-based layout,
ordering, multi-layer stacking, and integration with population data and
column headers.

The key points to remember:

- Your `analyze_fn` receives `.data` (a data.frame subset) and
  `.target_var` (a character string) for each group.
- In **format strings mode**, return a single-row numeric data.frame and
  let
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  handle formatting.
- In **pre-formatted mode**, return a data.frame with `row_label` and
  `formatted` columns.
- Use `by` for row grouping, mixing text labels and data variable names
  as needed.
- Build error handling into your function to handle edge cases
  gracefully.
