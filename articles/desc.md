# Descriptive Statistics Layers

## Introduction

Descriptive statistics tables are among the most common outputs in
clinical trial reporting. Whether you are summarizing demographics in a
Table 14.1 or lab parameters across visits, the pattern is the same:
compute summary statistics for a continuous variable, then present them
in a formatted, publication-ready layout grouped by treatment arm.

In tplyr2, descriptive statistics layers are created with
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md).
The core of your control over the output comes from the `format_strings`
parameter within
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
Format strings let you specify exactly which statistics appear, what row
label each statistic gets, and how numbers are formatted – all in one
place.

Let’s start with a typical example. Using the built-in `tplyr_adsl`
dataset, we will summarize age by treatment group.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      by = "Age (years)",
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"     = f_str("xx.x", "median"),
          "Q1, Q3"     = f_str("xx.x, xx.x", "q1", "q3"),
          "Min, Max"   = f_str("xx, xx", "min", "max"),
          "Missing"    = f_str("xx", "missing")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1   | rowlabel2 | res1         | res2         | res3         |
|:------------|:----------|:-------------|:-------------|:-------------|
| Age (years) | n         | 86           | 84           | 84           |
| Age (years) | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Age (years) | Median    | 76.0         | 76.0         | 77.5         |
| Age (years) | Q1, Q3    | 69.2, 81.8   | 70.8, 80.0   | 71.0, 82.0   |
| Age (years) | Min, Max  | 52, 89       | 56, 88       | 51, 88       |
| Age (years) | Missing   | 0            | 0            | 0            |

A few things to note about this example:

- The `format_strings` parameter is a named list. Each **name** becomes
  the row label in the output (e.g., “Mean (SD)”), and each **value** is
  an
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  object that controls the numeric format.
- Inside
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md),
  the first argument is the format template. The characters `x` define
  the display width: `xx.x` means two integer digits and one decimal
  place. The remaining arguments are strings naming the statistics to
  plug into each format group.
- The `by` argument `"Age (years)"` does not match any column in the
  data, so tplyr2 treats it as a text label. It appears as an additional
  `rowlabel` column, which is useful for distinguishing blocks of
  statistics when multiple layers are combined.

## Built-in Summaries

tplyr2 provides a set of built-in summary statistics that cover the most
common needs in clinical reporting. These are computed automatically for
every
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
layer – you simply reference them by name in your format strings.

| Statistic | Description                      | Details                         |
|-----------|----------------------------------|---------------------------------|
| `n`       | Non-missing count                | `length(x[!is.na(x)])`          |
| `mean`    | Arithmetic mean                  | `mean(x, na.rm = TRUE)`         |
| `sd`      | Standard deviation               | `sd(x, na.rm = TRUE)`           |
| `median`  | Median                           | `median(x, na.rm = TRUE)`       |
| `var`     | Variance                         | `var(x, na.rm = TRUE)`          |
| `min`     | Minimum                          | `min(x)` of finite values       |
| `max`     | Maximum                          | `max(x)` of finite values       |
| `iqr`     | Interquartile range              | `IQR(x, type = ...)`            |
| `q1`      | First quartile (25th percentile) | `quantile(x, 0.25, type = ...)` |
| `q3`      | Third quartile (75th percentile) | `quantile(x, 0.75, type = ...)` |
| `missing` | Missing count                    | `sum(is.na(x))`                 |

A few important notes about these built-in summaries:

- All statistics use `na.rm = TRUE` by default, so missing values are
  excluded from calculations (except for `missing` itself, which counts
  them).
- `min` and `max` operate on finite values only. If all values in a
  group are `NA`, the result is `NA_real_` rather than `Inf` or `-Inf`.
  This avoids formatting issues where infinity symbols would appear in
  your output.
- The `n` statistic counts non-missing observations, while `missing`
  counts the `NA` values. Together they sum to the total number of rows
  in that group.

### Quantile Algorithm

By default, tplyr2 uses R’s default quantile algorithm (Type 7) for
computing `q1`, `q3`, and `iqr`. This is fine for many applications, but
clinical trial reporting often needs to match SAS output, which uses a
different algorithm (closest to R’s Type 3).

You can change the quantile algorithm globally using
[`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md):

``` r
# Default Type 7 (R default)
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "Q1, Q3" = f_str("xx.x, xx.x", "q1", "q3")
        )
      )
    )
  )
)

result_type7 <- tplyr_build(spec, tplyr_adsl)
kable(result_type7[, !grepl("^ord", names(result_type7))],
      caption = "Type 7 (R default)")
```

| rowlabel1 | res1       | res2       | res3       |
|:----------|:-----------|:-----------|:-----------|
| Q1, Q3    | 69.2, 81.8 | 70.8, 80.0 | 71.0, 82.0 |

Type 7 (R default)

``` r
# Type 3 (matches SAS PROC UNIVARIATE default)
tplyr2_options(quantile_type = 3)

result_type3 <- tplyr_build(spec, tplyr_adsl)
kable(result_type3[, !grepl("^ord", names(result_type3))],
      caption = "Type 3 (SAS-like)")
```

| rowlabel1 | res1       | res2       | res3       |
|:----------|:-----------|:-----------|:-----------|
| Q1, Q3    | 69.0, 81.0 | 70.0, 80.0 | 71.0, 82.0 |

Type 3 (SAS-like)

``` r

# Reset to default
tplyr2_options(quantile_type = 7)
```

Notice how the quartile values differ between the two algorithms. The
difference is typically small but can matter when you need to produce
outputs that match SAS exactly. Type 3 uses the nearest even order
statistic and is the closest match to SAS’s default behavior.

## Custom Summaries

The built-in summaries cover most standard needs, but clinical reporting
sometimes calls for statistics that are not part of the default set –
geometric means, coefficients of variation, trimmed means, and so on.
tplyr2 handles this through custom summaries.

### Layer-Level Custom Summaries

You can define custom summaries directly in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
using the `custom_summaries` parameter. Each custom summary is a named
expression that uses `.var` as a placeholder for the target variable’s
values.

Here is an example computing a geometric mean alongside the standard
mean:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "n"              = f_str("xx", "n"),
          "Mean (SD)"      = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Geometric Mean" = f_str("xx.xx", "geo_mean")
        ),
        custom_summaries = list(
          geo_mean = quote(exp(mean(log(.var[.var > 0]), na.rm = TRUE)))
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1      | res1         | res2         | res3         |
|:---------------|:-------------|:-------------|:-------------|
| n              | 86           | 84           | 84           |
| Mean (SD)      | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Geometric Mean | 74.70        | 73.94        | 75.18        |

The key points about custom summaries:

- Use [`quote()`](https://rdrr.io/r/base/substitute.html) to wrap the
  expression. This delays evaluation until build time, when `.var` is
  replaced with the actual data vector.
- `.var` refers to the values of the target variable for the current
  group. It behaves like a numeric vector, so you can apply any R
  function to it.
- If a custom summary expression throws an error (e.g., trying to take
  the log of negative values), tplyr2 catches the error and returns
  `NA_real_` for that group, so your table build will not fail.

### Session-Level Custom Summaries

If you find yourself using the same custom summary across many tables in
a study, you can register it at the session level using
[`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md).
Once registered, the custom statistic is available by name in any
`format_strings` specification, just like the built-in summaries.

``` r
# Register a coefficient of variation summary for the session
tplyr2_options(
  custom_summaries = list(
    cv = quote(sd(.var, na.rm = TRUE) / mean(.var, na.rm = TRUE) * 100)
  )
)

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "CV (%)"     = f_str("xx.x", "cv")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | res1         | res2         | res3         |
|:----------|:-------------|:-------------|:-------------|
| n         | 86           | 84           | 84           |
| Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| CV (%)    | 11.4         | 10.6         | 11.0         |

``` r

# Clean up
tplyr2_options(custom_summaries = NULL)
```

### Overriding Built-in Summaries

Custom summaries can even overwrite built-in statistics. If you name a
custom summary `"mean"`, it replaces the built-in mean calculation. This
is useful when your study requires a non-standard definition of a
standard statistic, such as using a trimmed mean instead of the
arithmetic mean.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "Trimmed Mean" = f_str("xx.x", "mean")
        ),
        custom_summaries = list(
          mean = quote(mean(.var, trim = 0.1, na.rm = TRUE))
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1    | res1 | res2 | res3 |
|:-------------|:-----|:-----|:-----|
| Trimmed Mean | 75.7 | 75.0 | 76.6 |

Layer-level custom summaries always take priority over session-level
custom summaries, and both take priority over built-in statistics. This
layered precedence gives you fine-grained control: set sensible defaults
at the session level, then override on a per-layer basis when needed.

## Multi-Variable Descriptive Statistics

It is common to summarize several continuous variables in a single table
– for example, a demographics table that includes age, height, and
weight. Rather than creating separate layers for each variable, you can
pass a character vector of variable names to
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md).

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc(c("AGE", "HEIGHTBL", "WEIGHTBL"),
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"     = f_str("xx.x", "median"),
          "Q1, Q3"     = f_str("xx.x, xx.x", "q1", "q3"),
          "Min, Max"   = f_str("xx, xx", "min", "max"),
          "Missing"    = f_str("xx", "missing")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | rowlabel2 | res1          | res2          | res3          |
|:----------|:----------|:--------------|:--------------|:--------------|
| AGE       | n         | 86            | 84            | 84            |
| AGE       | Mean (SD) | 75.2 ( 8.59)  | 74.4 ( 7.89)  | 75.7 ( 8.29)  |
| AGE       | Median    | 76.0          | 76.0          | 77.5          |
| AGE       | Q1, Q3    | 69.2, 81.8    | 70.8, 80.0    | 71.0, 82.0    |
| AGE       | Min, Max  | 52, 89        | 56, 88        | 51, 88        |
| AGE       | Missing   | 0             | 0             | 0             |
| HEIGHTBL  | n         | 86            | 84            | 84            |
| HEIGHTBL  | Mean (SD) | 162.6 (11.52) | 165.8 (10.13) | 163.4 (10.42) |
| HEIGHTBL  | Median    | 162.6         | 165.1         | 162.6         |
| HEIGHTBL  | Q1, Q3    | 154.0, 171.2  | 157.5, 172.8  | 157.5, 170.2  |
| HEIGHTBL  | Min, Max  | 137, 185      | 146, 190      | 136, 196      |
| HEIGHTBL  | Missing   | 0             | 0             | 0             |
| WEIGHTBL  | n         | 86            | 84            | 83            |
| WEIGHTBL  | Mean (SD) | 62.8 (12.77)  | 70.0 (14.65)  | 67.3 (14.12)  |
| WEIGHTBL  | Median    | 60.5          | 69.2          | 64.9          |
| WEIGHTBL  | Q1, Q3    | 53.6, 74.2    | 57.0, 80.3    | 56.0, 77.4    |
| WEIGHTBL  | Min, Max  | 34, 86        | 42, 108       | 45, 106       |
| WEIGHTBL  | Missing   | 0             | 0             | 1             |

When multiple target variables are specified:

- Each variable gets its own block of summary rows. The variable name
  appears as an additional `rowlabel` column (here, `rowlabel1`), with
  the statistic labels in the next column (`rowlabel2`).
- The same `format_strings` are applied to every variable. Each
  variable’s statistics are computed independently, so differences in
  scale (e.g., age in years vs. height in centimeters) are handled
  naturally.
- Ordering is preserved: the first variable’s rows appear first,
  followed by the second, and so on.

You can also combine multi-variable descriptive layers with the `by`
parameter. If you add a text label through `by`, it becomes the
outermost row label, followed by the variable name, then the statistic
label.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc(c("AGE", "WEIGHTBL"),
      by = "Demographics",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"    = f_str("xx.x", "median")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1    | rowlabel2 | rowlabel3 | res1         | res2         | res3         |
|:-------------|:----------|:----------|:-------------|:-------------|:-------------|
| Demographics | AGE       | n         | 86           | 84           | 84           |
| Demographics | AGE       | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Demographics | AGE       | Median    | 76.0         | 76.0         | 77.5         |
| Demographics | WEIGHTBL  | n         | 86           | 84           | 83           |
| Demographics | WEIGHTBL  | Mean (SD) | 62.8 (12.77) | 70.0 (14.65) | 67.3 (14.12) |
| Demographics | WEIGHTBL  | Median    | 60.5         | 69.2         | 64.9         |

## Where to Go From Here

This vignette covered the fundamentals of descriptive statistics layers
in tplyr2: built-in summaries, custom summaries, quantile algorithms,
and multi-variable analysis. But there is more to explore when it comes
to controlling how your numbers look on the page.

The
[`vignette("desc_layer_formatting")`](https://github.com/mstackhouse/tplyr2/articles/desc_layer_formatting.md)
vignette dives into advanced formatting topics including:

- **Auto-precision**: Dynamically adjusting decimal places based on the
  precision of collected data, using `precision_by`, `precision_on`, and
  `precision_cap` in
  [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
- **Empty value handling**: Controlling what appears when all values in
  a group are missing, using the `empty` parameter of
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md).
- **Parenthesis hugging**: Eliminating the gap between parentheses and
  numbers so that formats like `( 5.2)` become `(5.2 )` using the `X`
  character in format strings.
