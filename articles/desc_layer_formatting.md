# Advanced Descriptive Statistics Formatting

``` r
library(tplyr2)
library(knitr)
```

This vignette covers advanced formatting for
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
layers: empty value handling, auto-precision, precision capping,
external precision data, and parenthesis hugging. For fundamentals
(built-in/custom summaries, multi-variable analysis), see
[`vignette("desc")`](https://github.com/mstackhouse/tplyr2/articles/desc.md).

## Empty Value Formatting

When all observations in a group are `NA`, tplyr2 fills the cell with
whitespace by default. The `empty` parameter of
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
overrides this. Use `.overall` to replace the entire formatted string:

``` r
test_data <- data.frame(
  TRT = c(rep("A", 5), rep("B", 5), rep("C", 3)),
  VAL = c(1.5, 2.3, 3.1, 4.0, 2.7,
          5.2, 6.1, 3.8, 4.4, 7.0,
          NA, NA, NA),
  stringsAsFactors = FALSE
)

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_desc("VAL",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd",
                              empty = c(.overall = "---")),
          "Median"    = f_str("xx.x", "median",
                              empty = c(.overall = "NE"))
        )
      )
    )
  )
)

result <- tplyr_build(spec, test_data)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | res1        | res2        | res3 |
|:----------|:------------|:------------|:-----|
| n         | 5           | 5           | 0    |
| Mean (SD) | 2.7 ( 0.93) | 5.3 ( 1.28) | —    |
| Median    | 2.7         | 5.2         | NE   |

Group C has all-NA values, so Mean (SD) shows “—” and Median shows “NE”.
Each
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
can specify its own replacement string independently.

## Auto-Precision

Fixed format strings like `"xx.x"` work when you know the data’s scale.
But lab parameters vary widely in precision. The auto-precision system
lets the data determine how many digits to display.

### Core Settings

Two
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
parameters drive auto-precision:

- `precision_on`: the variable scanned for decimal precision (defaults
  to the target variable)
- `precision_by`: grouping variables that define independent precision
  groups

For each group, tplyr2 computes `max_int` (maximum integer digits) and
`max_dec` (maximum meaningful decimal places) from the data.

### The `a` Character

In format strings, lowercase `a` means “use the data-driven width”:

- `a.a` resolves to `max_int` integer digits and `max_dec` decimal
  places
- `a.a+1` adds one extra decimal place beyond the data’s precision
- `a+2.a` adds two extra integer digits

The `+N` suffix is standard clinical practice – typically the mean gets
`+1` and the SD gets `+2` beyond the raw data’s precision.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"),
          "Median"    = f_str("a.a+1", "median"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max"),
          "Missing"   = f_str("xx", "missing")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | rowlabel2 | res1                 | res2                 | res3                 |
|:----------|:----------|:---------------------|:---------------------|:---------------------|
| URATE     | n         | 75                   | 78                   | 47                   |
| URATE     | Mean (SD) | 322.2230 ( 64.96877) | 298.8489 ( 55.54287) | 287.1492 ( 76.82208) |
| URATE     | Median    | 303.3480             | 300.3740             | 267.6600             |
| URATE     | Min, Max  | 226.024, 469.892     | 178.440, 481.788     | 178.440, 463.944     |
| URATE     | Missing   | 0                    | 0                    | 0                    |

Since `AVAL` in `tplyr_adlb` has three decimal places, `a.a` resolves to
three decimals. The mean (`a.a+1`) displays four, and the SD (`a.a+2`)
displays five.

## Precision Capping

Auto-precision can produce unreasonably wide columns when data has
extreme precision. Capping sets upper bounds on the resolved widths.

### Layer-Level Cap

Set `precision_cap` in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
as a named vector with `int` and/or `dec` components:

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        precision_cap = c(int = 3, dec = 2),
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | rowlabel2 | res1               | res2               | res3               |
|:----------|:----------|:-------------------|:-------------------|:-------------------|
| URATE     | Mean (SD) | 322.223 ( 64.9688) | 298.849 ( 55.5429) | 287.149 ( 76.8221) |
| URATE     | Min, Max  | 226.02, 469.89     | 178.44, 481.79     | 178.44, 463.94     |

With `dec = 2`, the base precision is capped at two decimals. The mean
shows three (`+1`), the SD shows four (`+2`), and Min/Max uses the
capped base of two.

### Global Cap

Use
[`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md)
to set a session-wide cap:

``` r
tplyr2_options(precision_cap = c(int = 3, dec = 1))

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | rowlabel2 | res1               | res2               | res3               |
|:----------|:----------|:-------------------|:-------------------|:-------------------|
| URATE     | Mean (SD) | 322.223 ( 64.9688) | 298.849 ( 55.5429) | 287.149 ( 76.8221) |
| URATE     | Min, Max  | 226.02, 469.89     | 178.44, 481.79     | 178.44, 463.94     |

``` r

tplyr2_options(precision_cap = NULL)
```

A layer-level cap always overrides the global option, so you can set
conservative session defaults and widen specific layers as needed.

## External Precision Data

When precision is predetermined by a statistical analysis plan, supply
it directly via `precision_data` – a data.frame with `max_int` and
`max_dec` columns, plus any `precision_by` grouping columns:

``` r
ext_precision <- data.frame(
  PARAMCD = "URATE",
  max_int = 3L,
  max_dec = 1L,
  stringsAsFactors = FALSE
)

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        precision_data = ext_precision,
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | rowlabel2 | res1             | res2             | res3             |
|:----------|:----------|:-----------------|:-----------------|:-----------------|
| URATE     | Mean (SD) | 322.22 ( 64.969) | 298.85 ( 55.543) | 287.15 ( 76.822) |
| URATE     | Min, Max  | 226.0, 469.9     | 178.4, 481.8     | 178.4, 463.9     |

With `max_dec = 1`, the mean shows two decimals (`+1`) and Min/Max shows
one, regardless of the data’s actual three-decimal precision.

## Parenthesis Hugging

Standard formatting pads numbers with leading spaces for alignment,
which can create gaps like `( 5.2)`. Parenthesis hugging shifts those
leading spaces to after the number, producing `(5.2 )` instead.

### The `X` and `A` Characters

Uppercase characters activate hugging:

- `X` – fixed width with hugging (uppercase `x`)
- `A` – auto-precision with hugging (uppercase `a`)

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "Standard"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Hugged"    = f_str("xx.x (XX.xx)", "mean", "sd")
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
| Standard  | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Hugged    | 75.2 (8.59 ) | 74.4 (7.89 ) | 75.7 (8.29 ) |

In “Standard”, the SD has leading spaces before the number inside the
parentheses. In “Hugged”, those spaces shift after the number so the
parenthesis sits flush against the first digit.

### Hugging with Auto-Precision

Uppercase `A` combines auto-precision with hugging:

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (A.A+2)", "mean", "sd"),
          "Min [Max]" = f_str("a.a [A.a]", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | rowlabel2   | res1                 | res2                 | res3                 |
|:----------|:------------|:---------------------|:---------------------|:---------------------|
| URATE     | Mean (SD)   | 322.2230 (64.96877 ) | 298.8489 (55.54287 ) | 287.1492 (76.82208 ) |
| URATE     | Min \[Max\] | 226.024 \[469.892\]  | 178.440 \[481.788\]  | 178.440 \[463.944\]  |

The mean uses lowercase `a` (standard padding) while the SD uses
uppercase `A` (hugged). The combination of auto-precision for
data-driven width plus hugging for tight delimiters is the standard
approach for publication-quality lab tables.

## Putting It All Together

A complete specification combining all the formatting features covered
here:

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        precision_cap = c(int = 4, dec = 3),
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("a.a+1 (A.A+2)", "mean", "sd",
                              empty = c(.overall = "")),
          "Median"    = f_str("a.a+1", "median",
                              empty = c(.overall = "NE")),
          "Q1, Q3"    = f_str("a.a+1, a.a+1", "q1", "q3"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max"),
          "Missing"   = f_str("xx", "missing")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | rowlabel2 | res1                 | res2                 | res3                 |
|:----------|:----------|:---------------------|:---------------------|:---------------------|
| URATE     | n         | 75                   | 78                   | 47                   |
| URATE     | Mean (SD) | 322.2230 (64.96877 ) | 298.8489 (55.54287 ) | 287.1492 (76.82208 ) |
| URATE     | Median    | 303.3480             | 300.3740             | 267.6600             |
| URATE     | Q1, Q3    | 267.6600, 383.6460   | 255.7640, 321.1920   | 237.9200, 303.3480   |
| URATE     | Min, Max  | 226.024, 469.892     | 178.440, 481.788     | 178.440, 463.944     |
| URATE     | Missing   | 0                    | 0                    | 0                    |

This specification adapts decimal places to the data, caps precision at
three decimals, hugs the SD against its opening parenthesis, and
provides meaningful fill strings for all-NA groups.
