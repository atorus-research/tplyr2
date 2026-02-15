# tplyr2 Options

## Introduction

tplyr2 provides a set of session-level options that control how tables
are built. These options let you configure behaviors that apply across
all tables in a session – rounding rules, quantile algorithms, precision
limits, custom summary functions, and scientific notation handling –
without repeating yourself in every spec.

All tplyr2 options are managed through a single function:
[`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md).
When called with no arguments, it returns the current values of all
options. When called with named arguments, it sets those options. Under
the hood, each option is stored as a standard R option with the
`tplyr2.` prefix, so they can also be set with
[`options()`](https://rdrr.io/r/base/options.html) directly if you
prefer.

## Viewing Current Options

To see what every option is currently set to, call
[`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md)
with no arguments:

``` r
tplyr2_options()
#> $tplyr2.IBMRounding
#> [1] FALSE
#> 
#> $tplyr2.quantile_type
#> [1] 7
#> 
#> $tplyr2.precision_cap
#> NULL
#> 
#> $tplyr2.custom_summaries
#> NULL
#> 
#> $tplyr2.scipen
#> [1] 9999
```

The output is a named list. Options you have not changed show their
defaults: `IBMRounding` is `FALSE`, `quantile_type` is `7`,
`precision_cap` and `custom_summaries` are `NULL`, and `scipen` is
`9999`.

## IBM Rounding

R uses “banker’s rounding” (round half to even) by default. This means
that 0.5 rounds to 0, 1.5 rounds to 2, 2.5 rounds to 2, and 3.5 rounds
to 4. The rule minimizes cumulative bias in large datasets, but it does
not match how SAS and many other statistical software packages round.

In clinical trial reporting, you often need to reproduce SAS output
exactly. SAS uses round-half-away-from-zero (sometimes called “IBM
rounding”), where 2.5 always rounds up to 3 and -2.5 always rounds down
to -3. tplyr2 supports this through the `IBMRounding` option.

The formula used when IBM rounding is enabled is:

$$\text{round}(x) = \text{sign}(x) \cdot \left\lfloor |x| \cdot 10^{d} + 0.5 \right\rfloor/10^{d}$$

Let’s see the difference in practice. We will build a simple descriptive
statistics table and compare the mean under both rounding methods.

``` r
# Create data where rounding makes a visible difference
demo_data <- data.frame(
  TRT = rep(c("Drug", "Placebo"), each = 4),
  VAL = c(2.5, 3.5, 4.5, 5.5, 1.5, 2.5, 3.5, 4.5)
)

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_desc("VAL",
      settings = layer_settings(
        format_strings = list(
          "Mean" = f_str("x.x", "mean")
        )
      )
    )
  )
)

# Default: banker's rounding
result_bankers <- tplyr_build(spec, demo_data)
kable(result_bankers[, !grepl("^ord", names(result_bankers))],
      caption = "Banker's rounding (default)")
```

| rowlabel1 | res1 | res2 |
|:----------|:-----|:-----|
| Mean      | 4.0  | 3.0  |

Banker’s rounding (default)

``` r
# IBM rounding
tplyr2_options(IBMRounding = TRUE)

result_ibm <- tplyr_build(spec, demo_data)
kable(result_ibm[, !grepl("^ord", names(result_ibm))],
      caption = "IBM rounding (round-half-away-from-zero)")
```

| rowlabel1 | res1 | res2 |
|:----------|:-----|:-----|
| Mean      | 4.0  | 3.0  |

IBM rounding (round-half-away-from-zero)

``` r

# Reset to default
tplyr2_options(IBMRounding = FALSE)
```

The difference is subtle but consequential for regulatory submissions.
When the mean falls exactly on a rounding boundary (e.g., 4.0 with one
decimal rounds to “4.0” either way, but values like 2.25 rounded to one
decimal show the difference), IBM rounding consistently rounds the 5
away from zero, while banker’s rounding alternates based on the
preceding digit.

## Quantile Algorithm

R’s [`quantile()`](https://rdrr.io/r/stats/quantile.html) function
supports nine different algorithms, numbered Type 1 through Type 9. The
default in R is Type 7, which uses linear interpolation and is widely
used in statistics. However, SAS’s `PROC UNIVARIATE` uses an algorithm
closest to R’s Type 3, which selects the nearest even order statistic.

In tplyr2, the `quantile_type` option controls which algorithm is used
for `q1`, `q3`, and `iqr` in descriptive statistics layers.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "Q1, Q3"  = f_str("xx.xx, xx.xx", "q1", "q3"),
          "IQR"     = f_str("xx.xx", "iqr")
        )
      )
    )
  )
)

# Type 7 (R default)
result_t7 <- tplyr_build(spec, tplyr_adsl)
kable(result_t7[, !grepl("^ord", names(result_t7))],
      caption = "Quantile Type 7 (R default)")
```

| rowlabel1 | res1         | res2         | res3         |
|:----------|:-------------|:-------------|:-------------|
| Q1, Q3    | 69.25, 81.75 | 70.75, 80.00 | 71.00, 82.00 |
| IQR       | 12.50        | 9.25         | 11.00        |

Quantile Type 7 (R default)

``` r
# Type 3 (SAS-compatible)
tplyr2_options(quantile_type = 3)

result_t3 <- tplyr_build(spec, tplyr_adsl)
kable(result_t3[, !grepl("^ord", names(result_t3))],
      caption = "Quantile Type 3 (SAS-compatible)")
```

| rowlabel1 | res1         | res2         | res3         |
|:----------|:-------------|:-------------|:-------------|
| Q1, Q3    | 69.00, 81.00 | 70.00, 80.00 | 71.00, 82.00 |
| IQR       | 12.00        | 10.00        | 11.00        |

Quantile Type 3 (SAS-compatible)

``` r

# Reset to default
tplyr2_options(quantile_type = 7)
```

The differences between Type 7 and Type 3 tend to be small for large
datasets, but they can be meaningful for small groups or when exact SAS
reconciliation is required. If your statistical analysis plan (SAP)
specifies SAS-compatible quantiles, set `quantile_type = 3` at the start
of your session.

## Precision Cap

When using tplyr2’s auto-precision system (format strings with `a` or
`A` characters), the integer and decimal widths are determined
dynamically from the data. This is useful when the same format must
accommodate variables with different scales, but sometimes the data can
drive widths to impractical extremes – a variable recorded to six
decimal places would produce very wide columns.

The `precision_cap` option lets you set a global ceiling on
auto-precision widths. It takes a named numeric vector with `int` and/or
`dec` components.

``` r
# Without a cap, precision is driven entirely by the data
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("WEIGHTBL",
      by = "Weight (kg)",
      settings = layer_settings(
        format_strings = list(
          "Mean (SD)" = f_str("a+1.a+1 (a+2.a+2)", "mean", "sd")
        )
      )
    )
  )
)

result_nocap <- tplyr_build(spec, tplyr_adsl)
kable(result_nocap[, !grepl("^ord", names(result_nocap))],
      caption = "Auto-precision, no cap")
```

| rowlabel1   | rowlabel2 | res1            | res2            | res3            |
|:------------|:----------|:----------------|:----------------|:----------------|
| Weight (kg) | Mean (SD) | 62.76 ( 12.772) | 70.00 ( 14.653) | 67.28 ( 14.124) |

Auto-precision, no cap

``` r
# Apply a global cap: max 3 integer digits, max 1 decimal digit
tplyr2_options(precision_cap = c(int = 3, dec = 1))

result_capped <- tplyr_build(spec, tplyr_adsl)
kable(result_capped[, !grepl("^ord", names(result_capped))],
      caption = "Auto-precision, capped at int=3, dec=1")
```

| rowlabel1   | rowlabel2 | res1            | res2            | res3            |
|:------------|:----------|:----------------|:----------------|:----------------|
| Weight (kg) | Mean (SD) | 62.76 ( 12.772) | 70.00 ( 14.653) | 67.28 ( 14.124) |

Auto-precision, capped at int=3, dec=1

``` r

# Reset to default
tplyr2_options(precision_cap = NULL)
```

Note that the `precision_cap` option serves as a global default. If a
specific layer provides its own `precision_cap` in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md),
the layer-level cap takes priority. This gives you a safety net at the
session level while allowing individual layers to override when needed.

## Custom Summaries

Descriptive statistics layers in tplyr2 come with a set of built-in
summaries (`n`, `mean`, `sd`, `median`, `var`, `min`, `max`, `iqr`,
`q1`, `q3`, `missing`). When your study requires additional statistics
across many tables – geometric means, coefficients of variation, or any
other derived measure – you can register them at the session level so
they are available everywhere.

``` r
# Register session-level custom summaries
tplyr2_options(
  custom_summaries = list(
    geo_mean = quote(exp(mean(log(.var[.var > 0]), na.rm = TRUE))),
    cv       = quote(sd(.var, na.rm = TRUE) / mean(.var, na.rm = TRUE) * 100)
  )
)

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "n"              = f_str("xx", "n"),
          "Mean (SD)"      = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Geometric Mean" = f_str("xx.xx", "geo_mean"),
          "CV (%)"         = f_str("xx.x", "cv")
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
| CV (%)         | 11.4         | 10.6         | 11.0         |

A few things to keep in mind about custom summaries:

- Each expression uses `.var` as a placeholder for the target variable’s
  values within each group. At build time, `.var` is replaced with the
  actual numeric vector.
- Expressions should be wrapped in
  [`quote()`](https://rdrr.io/r/base/substitute.html) to delay
  evaluation.
- If an expression throws an error for a particular group (e.g., trying
  [`log()`](https://rdrr.io/r/base/Log.html) on non-positive values),
  tplyr2 catches the error and returns `NA_real_` rather than stopping
  the build.
- Session-level custom summaries can be overridden by layer-level
  `custom_summaries` in
  [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
  This means you can set study-wide defaults and still customize
  individual layers.
- A custom summary can even share a name with a built-in statistic
  (e.g., `mean`), in which case it replaces the built-in calculation.

``` r
# Reset to default
tplyr2_options(custom_summaries = NULL)
```

## Scientific Notation Control

R may format very large or very small numbers in scientific notation
(e.g., `1e+05` instead of `100000`). This is controlled by R’s `scipen`
option, which sets a penalty against scientific notation – higher values
make it less likely. During
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md),
tplyr2 temporarily overrides `scipen` to prevent scientific notation
from appearing in formatted output.

The default value of `9999` effectively guarantees that scientific
notation will never appear in your tables. In most cases, you will not
need to change this. However, if you have a specific reason to allow
scientific notation (or if you are debugging formatting), you can adjust
it:

``` r
# Check the current value
tplyr2_options()$tplyr2.scipen
#> [1] 9999
```

The `scipen` override is applied only for the duration of
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
and is automatically restored to its previous value when the build
completes. This means it will not affect other code in your session.

## Resetting Options

To return all options to their defaults, set each one explicitly:

``` r
tplyr2_options(
  IBMRounding     = FALSE,
  quantile_type   = 7L,
  precision_cap   = NULL,
  custom_summaries = NULL,
  scipen          = 9999L
)

# Verify
tplyr2_options()
#> $tplyr2.IBMRounding
#> [1] FALSE
#> 
#> $tplyr2.quantile_type
#> [1] 7
#> 
#> $tplyr2.precision_cap
#> NULL
#> 
#> $tplyr2.custom_summaries
#> NULL
#> 
#> $tplyr2.scipen
#> [1] 9999
```

Because tplyr2 options are stored as standard R options (with the
`tplyr2.` prefix), they reset automatically when you start a new R
session. If you want your options to persist, place the
[`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md)
call in your project’s `.Rprofile` or at the top of your analysis
script.
