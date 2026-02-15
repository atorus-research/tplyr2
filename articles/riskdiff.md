# Risk Difference

## Introduction

In clinical trials, adverse event tables typically display the frequency
of events by treatment group. While these counts are informative on
their own, regulatory reviewers and study teams often want to see a
direct statistical comparison between groups – specifically, the
difference in proportions and a confidence interval around that
difference. This quantity is the risk difference.

The risk difference answers a straightforward question: how much more
(or less) likely is an event in one group compared to another? A risk
difference of 10% with a 95% confidence interval of (2%, 18%) tells you
the event rate was 10 percentage points higher in the treatment group,
and you can be reasonably confident the true difference falls between 2
and 18 percentage points.

tplyr2 computes risk differences using
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) with no
continuity correction, producing an asymptotic Wald-type confidence
interval. This is configured entirely through the `risk_diff` parameter
in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md),
and the results appear as additional columns in the output alongside the
standard count summaries.

## Basic Risk Difference

To add a risk difference to a count layer, pass a `risk_diff` list
inside
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
At minimum, you need to specify which two treatment levels to compare
via the `comparisons` parameter.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo"))
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1")], 10))
```

| rowlabel1                   | res1      | res2      | res3      | rdiff1           |
|:----------------------------|:----------|:----------|:----------|:-----------------|
| ABDOMINAL PAIN              | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0)  |
| AGITATION                   | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0)  |
| ANXIETY                     | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0)  |
| APPLICATION SITE DERMATITIS | 1 ( 3.1%) | 3 ( 7.0%) | 2 ( 4.0%) | 1.8 (-4.2, 7.7)  |
| APPLICATION SITE ERYTHEMA   | 0 ( 0.0%) | 3 ( 7.0%) | 4 ( 8.0%) | 3.9 (-0.4, 8.2)  |
| APPLICATION SITE IRRITATION | 1 ( 3.1%) | 3 ( 7.0%) | 2 ( 4.0%) | 3.1 (-3.4, 9.5)  |
| APPLICATION SITE PAIN       | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) | 1.3 (-1.2, 3.8)  |
| APPLICATION SITE PRURITUS   | 4 (12.5%) | 6 (14.0%) | 5 (10.0%) | 0.6 (-9.7, 10.8) |
| APPLICATION SITE REACTION   | 1 ( 3.1%) | 1 ( 2.3%) | 0 ( 0.0%) | -0.8 (-5.7, 4.0) |
| APPLICATION SITE URTICARIA  | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0)  |

The risk difference appears in the `rdiff1` column. Each comparison pair
you specify produces one `rdiff` column, numbered sequentially. The
column carries a label attribute indicating which groups were compared.

``` r
attr(result$rdiff1, "label")
#> [1] "Xanomeline High Dose vs Placebo"
```

Note the order of the comparison pair matters: the first element is the
treatment group and the second is the reference. The risk difference is
computed as the treatment proportion minus the reference proportion,
expressed as a percentage.

## Configuring Comparisons

You are not limited to a single comparison. When your study has multiple
active dose groups, you often want to compare each one against placebo.
Pass multiple pairs in the `comparisons` list.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(
            c("Xanomeline High Dose", "Placebo"),
            c("Xanomeline Low Dose", "Placebo")
          ),
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1", "rdiff2")], 8))
```

| rowlabel1                   | res1      | res2      | res3      | rdiff1           | rdiff2            |
|:----------------------------|:----------|:----------|:----------|:-----------------|:------------------|
| ABDOMINAL PAIN              | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0)  | 1.3 (-1.2, 3.9)   |
| AGITATION                   | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0)  | 1.3 (-1.2, 3.9)   |
| ANXIETY                     | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0)  | 1.3 (-1.2, 3.9)   |
| APPLICATION SITE DERMATITIS | 1 ( 3.1%) | 3 ( 7.0%) | 2 ( 4.0%) | 1.8 (-4.2, 7.7)  | 0.5 (-5.0, 6.0)   |
| APPLICATION SITE ERYTHEMA   | 0 ( 0.0%) | 3 ( 7.0%) | 4 ( 8.0%) | 3.9 (-0.4, 8.2)  | 5.3 ( 0.2, 10.3)  |
| APPLICATION SITE IRRITATION | 1 ( 3.1%) | 3 ( 7.0%) | 2 ( 4.0%) | 3.1 (-3.4, 9.5)  | 0.5 (-5.0, 6.0)   |
| APPLICATION SITE PAIN       | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) | 1.3 (-1.2, 3.8)  | 0.0 ( 0.0, 0.0)   |
| APPLICATION SITE PRURITUS   | 4 (12.5%) | 6 (14.0%) | 5 (10.0%) | 0.6 (-9.7, 10.8) | -1.9 (-11.7, 7.8) |

The first comparison (`Xanomeline High Dose` vs `Placebo`) goes into
`rdiff1`, and the second (`Xanomeline Low Dose` vs `Placebo`) goes into
`rdiff2`. Each column gets its own label.

``` r
attr(result$rdiff1, "label")
#> [1] "Xanomeline High Dose vs Placebo"
attr(result$rdiff2, "label")
#> [1] "Xanomeline Low Dose vs Placebo"
```

## Formatting Output

The risk difference format is controlled through the `format` parameter,
which takes an
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
object just like the count format strings. Four variables are available
for use in the format:

- `rdiff` – the risk difference as a percentage
- `lower` – the lower bound of the confidence interval
- `upper` – the upper bound of the confidence interval
- `p_value` – the p-value from
  [`prop.test()`](https://rdrr.io/r/stats/prop.test.html)

When no format is specified, tplyr2 uses the default:
`f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")`.

You can include the p-value in the formatted string if your table
requires it.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          format = f_str("xx.x (xx.x, xx.x) [x.xxxx]", "rdiff", "lower", "upper", "p_value")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1")], 8))
```

| rowlabel1                   | res1      | res2      | res3      | rdiff1                      |
|:----------------------------|:----------|:----------|:----------|:----------------------------|
| ABDOMINAL PAIN              | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0) \[ \]       |
| AGITATION                   | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0) \[ \]       |
| ANXIETY                     | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0) \[ \]       |
| APPLICATION SITE DERMATITIS | 1 ( 3.1%) | 3 ( 7.0%) | 2 ( 4.0%) | 1.8 (-4.2, 7.7) \[0.5887\]  |
| APPLICATION SITE ERYTHEMA   | 0 ( 0.0%) | 3 ( 7.0%) | 4 ( 8.0%) | 3.9 (-0.4, 8.2) \[0.1707\]  |
| APPLICATION SITE IRRITATION | 1 ( 3.1%) | 3 ( 7.0%) | 2 ( 4.0%) | 3.1 (-3.4, 9.5) \[0.3996\]  |
| APPLICATION SITE PAIN       | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) | 1.3 (-1.2, 3.8) \[0.4328\]  |
| APPLICATION SITE PRURITUS   | 4 (12.5%) | 6 (14.0%) | 5 (10.0%) | 0.6 (-9.7, 10.8) \[0.9122\] |

The `x` characters in the format string control field width, just as
they do for count format strings. Each `x` reserves one character
position, so `xx.x` gives one decimal place with room for a two-digit
integer part. You can adjust the precision to match your table shell
requirements.

## Confidence Interval Level

By default, tplyr2 computes a 95% confidence interval. You can change
this with the `ci` parameter.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          ci = 0.90,
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "rdiff1")], 8))
```

| rowlabel1                   | rdiff1          |
|:----------------------------|:----------------|
| ABDOMINAL PAIN              | 0.0 ( 0.0, 0.0) |
| AGITATION                   | 0.0 ( 0.0, 0.0) |
| ANXIETY                     | 0.0 ( 0.0, 0.0) |
| APPLICATION SITE DERMATITIS | 1.8 (-3.2, 6.8) |
| APPLICATION SITE ERYTHEMA   | 3.9 ( 0.3, 7.5) |
| APPLICATION SITE IRRITATION | 3.1 (-2.3, 8.5) |
| APPLICATION SITE PAIN       | 1.3 (-0.8, 3.4) |
| APPLICATION SITE PRURITUS   | 0.6 (-8.0, 9.2) |

A 90% interval is narrower than a 95% interval for the same data. The
confidence level you choose should match your study’s statistical
analysis plan.

## Risk Difference with Distinct Counts

Risk difference calculations naturally work with the `distinct_by`
setting. When `distinct_by` is specified, the proportions used for the
risk difference are based on distinct subject counts rather than event
counts. This is almost always what you want for adverse event tables,
where a single subject can contribute multiple events.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%) [xxx]", "distinct_n", "distinct_pct", "n")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1")], 8))
```

| rowlabel1                   | res1             | res2             | res3             | rdiff1           |
|:----------------------------|:-----------------|:-----------------|:-----------------|:-----------------|
| ABDOMINAL PAIN              | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 2.0%) \[ 1\] | 0.0 ( 0.0, 0.0)  |
| AGITATION                   | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 2.0%) \[ 1\] | 0.0 ( 0.0, 0.0)  |
| ANXIETY                     | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 2.0%) \[ 1\] | 0.0 ( 0.0, 0.0)  |
| APPLICATION SITE DERMATITIS | 1 ( 3.1%) \[ 1\] | 3 ( 7.0%) \[ 3\] | 2 ( 4.0%) \[ 2\] | 1.8 (-4.2, 7.7)  |
| APPLICATION SITE ERYTHEMA   | 0 ( 0.0%) \[ 0\] | 3 ( 7.0%) \[ 3\] | 4 ( 8.0%) \[ 4\] | 3.9 (-0.4, 8.2)  |
| APPLICATION SITE IRRITATION | 1 ( 3.1%) \[ 1\] | 3 ( 7.0%) \[ 4\] | 2 ( 4.0%) \[ 2\] | 3.1 (-3.4, 9.5)  |
| APPLICATION SITE PAIN       | 0 ( 0.0%) \[ 0\] | 1 ( 2.3%) \[ 1\] | 0 ( 0.0%) \[ 0\] | 1.3 (-1.2, 3.8)  |
| APPLICATION SITE PRURITUS   | 4 (12.5%) \[ 4\] | 6 (14.0%) \[ 7\] | 5 (10.0%) \[ 5\] | 0.6 (-9.7, 10.8) |

In this output, the count columns show distinct subjects, their
percentage, and the total event count in brackets. The risk difference
is computed from the distinct subject proportions, which is the
clinically meaningful comparison.

## Interaction with Special Rows

An important detail: risk differences are computed before special rows
(total rows and missing rows) are appended. This means total and missing
rows will have empty risk difference values, which is the expected
behavior – a risk difference on a total row is not meaningful.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        total_row = TRUE,
        total_row_label = "Any adverse event",
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
# Show the last few rows including the total row
tail_rows <- tail(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1")], 5)
kable(tail_rows)
```

|     | rowlabel1                 | res1      | res2      | res3      | rdiff1          |
|:----|:--------------------------|:----------|:----------|:----------|:----------------|
| 85  | URTICARIA                 | 0 ( 0.0%) | 1 ( 2.3%) | 1 ( 2.0%) | 2.6 (-1.0, 6.2) |
| 86  | VENTRICULAR EXTRASYSTOLES | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) | 1.3 (-1.2, 3.8) |
| 87  | VENTRICULAR SEPTAL DEFECT | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) | 1.3 (-1.2, 3.8) |
| 88  | VOMITING                  | 0 ( 0.0%) | 2 ( 4.7%) | 0 ( 0.0%) | 2.6 (-1.0, 6.2) |
| 89  | WOUND                     | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) | 0.0 ( 0.0, 0.0) |

The “Any adverse event” row has an empty `rdiff1` value because it is a
total row added after the risk difference computation.

## Extracting Raw Numbers

The formatted risk difference strings are useful for display, but
sometimes you need the underlying numeric values for further analysis or
custom formatting. The
[`tplyr_numeric_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_numeric_data.md)
function gives you access to the raw counts that were used to compute
the risk differences.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
nd <- tplyr_numeric_data(result, layer = 1)
kable(head(nd, 10))
```

| TRTA    | AEDECOD                     |   n | distinct_n |      pct | distinct_pct | total | distinct_total |
|:--------|:----------------------------|----:|-----------:|---------:|-------------:|------:|---------------:|
| Placebo | ABDOMINAL PAIN              |   0 |          0 | 0.000000 |        0.000 |    47 |             32 |
| Placebo | AGITATION                   |   0 |          0 | 0.000000 |        0.000 |    47 |             32 |
| Placebo | ANXIETY                     |   0 |          0 | 0.000000 |        0.000 |    47 |             32 |
| Placebo | APPLICATION SITE DERMATITIS |   1 |          1 | 2.127660 |        3.125 |    47 |             32 |
| Placebo | APPLICATION SITE ERYTHEMA   |   0 |          0 | 0.000000 |        0.000 |    47 |             32 |
| Placebo | APPLICATION SITE IRRITATION |   1 |          1 | 2.127660 |        3.125 |    47 |             32 |
| Placebo | APPLICATION SITE PAIN       |   0 |          0 | 0.000000 |        0.000 |    47 |             32 |
| Placebo | APPLICATION SITE PRURITUS   |   4 |          4 | 8.510638 |       12.500 |    47 |             32 |
| Placebo | APPLICATION SITE REACTION   |   1 |          1 | 2.127660 |        3.125 |    47 |             32 |
| Placebo | APPLICATION SITE URTICARIA  |   0 |          0 | 0.000000 |        0.000 |    47 |             32 |

This data.frame contains the raw counts, percentages, and denominators
per treatment group and preferred term. These are the values that feed
into the [`prop.test()`](https://rdrr.io/r/stats/prop.test.html) calls.

You can also extract numeric values directly from the formatted risk
difference strings using
[`str_extract_num()`](https://github.com/mstackhouse/tplyr2/reference/str_extract_num.md).
This function pulls the nth number from a formatted string.

``` r
# Extract the risk difference value (1st number)
result$rdiff_value <- str_extract_num(result$rdiff1, 1)

# Extract the lower CI bound (2nd number)
result$rdiff_lower <- str_extract_num(result$rdiff1, 2)

# Extract the upper CI bound (3rd number)
result$rdiff_upper <- str_extract_num(result$rdiff1, 3)

kable(head(result[, c("rowlabel1", "rdiff1", "rdiff_value", "rdiff_lower", "rdiff_upper")], 8))
```

| rowlabel1                   | rdiff1           | rdiff_value | rdiff_lower | rdiff_upper |
|:----------------------------|:-----------------|------------:|------------:|------------:|
| ABDOMINAL PAIN              | 0.0 ( 0.0, 0.0)  |         0.0 |         0.0 |         0.0 |
| AGITATION                   | 0.0 ( 0.0, 0.0)  |         0.0 |         0.0 |         0.0 |
| ANXIETY                     | 0.0 ( 0.0, 0.0)  |         0.0 |         0.0 |         0.0 |
| APPLICATION SITE DERMATITIS | 1.8 (-4.2, 7.7)  |         1.8 |        -4.2 |         7.7 |
| APPLICATION SITE ERYTHEMA   | 3.9 (-0.4, 8.2)  |         3.9 |        -0.4 |         8.2 |
| APPLICATION SITE IRRITATION | 3.1 (-3.4, 9.5)  |         3.1 |        -3.4 |         9.5 |
| APPLICATION SITE PAIN       | 1.3 (-1.2, 3.8)  |         1.3 |        -1.2 |         3.8 |
| APPLICATION SITE PRURITUS   | 0.6 (-9.7, 10.8) |         0.6 |        -9.7 |        10.8 |

This approach is useful when you need numeric risk difference values for
downstream tasks like sorting, filtering, or creating forest plots.

## Summary

Risk difference in tplyr2 is configured entirely through the `risk_diff`
parameter in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
The key points to remember:

- **Comparisons** are specified as pairs of treatment levels, with the
  first element as the treatment and the second as the reference.
- **Multiple comparisons** each produce a separate `rdiff` column
  (`rdiff1`, `rdiff2`, etc.).
- **Formatting** uses
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  with the variables `rdiff`, `lower`, `upper`, and `p_value`.
- **Confidence level** defaults to 0.95 and is adjustable via the `ci`
  parameter.
- Risk differences are computed **before** special rows (total,
  missing), so those rows have empty risk difference values.
- Raw count data is accessible through
  [`tplyr_numeric_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_numeric_data.md),
  and formatted values can be parsed with
  [`str_extract_num()`](https://github.com/mstackhouse/tplyr2/reference/str_extract_num.md).
