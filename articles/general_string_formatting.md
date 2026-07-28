# General String Formatting

## Introduction

Clinical tables live and die by their alignment. When a reviewer scans a
column of numbers, the decimal points must line up, the parentheses must
sit in the same position from row to row, and the whitespace must be
consistent. One misaligned digit and the table looks unprofessional – or
worse, it raises questions about the numbers themselves.

tplyr2 solves this with **format strings**: a compact notation that
describes exactly how wide each number field should be, where decimals
fall, and how padding works. The
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
function is the entry point. You give it a template string and the names
of the statistics that fill each slot, and tplyr2 takes care of the
rest.

## How Format Strings Work

A format string is a character template containing one or more **format
groups** separated by **literal text**. Each format group corresponds to
one statistic.

``` r

# Two format groups separated by the literal " ("  and closing ")"
fmt <- f_str("xx.x (xx.xx)", "mean", "sd")
fmt
#> tplyr format string: "xx.x (xx.xx)"Variables: mean, sd
```

In this example:

- `xx.x` is the first format group (for `mean`): two integer digits, one
  decimal digit.
- `(` is a literal separator between the first and second groups.
- `xx.xx` is the second format group (for `sd`): two integer digits, two
  decimal digits.
- `)` is a trailing literal.

The number of `x` characters determines the field width. Each `x`
reserves exactly one character position in the output, so numbers
narrower than the field are left-padded with spaces to maintain
alignment across rows.

## Available Variables by Layer Type

Each layer type computes a specific set of statistics that you can
reference in format strings.

| Layer Type | Variable | Description |
|:---|:---|:---|
| Count | `n` | Number of observations |
| Count | `pct` | Percentage of observations |
| Count | `total` | Denominator for percentage |
| Count | `distinct_n` | Number of distinct subjects (requires `distinct_by`) |
| Count | `distinct_pct` | Percentage of distinct subjects |
| Count | `distinct_total` | Distinct denominator |
| Desc | `n` | Non-missing observation count |
| Desc | `n_records` | Records assessed (non-missing + missing) |
| Desc | `mean` | Arithmetic mean |
| Desc | `sd` | Standard deviation |
| Desc | `median` | Median |
| Desc | `var` | Variance |
| Desc | `min` | Minimum |
| Desc | `max` | Maximum |
| Desc | `iqr` | Interquartile range |
| Desc | `q1` | First quartile |
| Desc | `q3` | Third quartile |
| Desc | `missing` | Count of missing values |
| Shift | `n` | Number of observations |
| Shift | `pct` | Percentage of observations |
| Shift | `total` | Denominator for percentage |
| Shift | `distinct_n` | Distinct subjects (requires `distinct_by`) |
| Shift | `distinct_pct` | Percentage of distinct subjects |
| Shift | `distinct_total` | Distinct denominator |
| Analyze | *(user-defined)* | Names returned by `analyze_fn` |

For count layers, format strings are provided as a named list with the
key `n_counts`. For descriptive statistics layers, each named entry in
the `format_strings` list becomes a separate output row, with the name
used as the row label.

## Lowercase x: Fixed-Width Fields

The `x` character is the workhorse of format strings. Each `x` reserves
one character position. If a number has fewer digits than the number of
`x` characters, the output is left-padded with spaces. If a number has
more digits than available positions, the number prints in full (it is
never truncated).

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
          "Min, Max"   = f_str("xx, xx", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")])
```

| rowlabel1   | rowlabel2 | res1         | res2         | res3         |
|:------------|:----------|:-------------|:-------------|:-------------|
| Age (years) | n         | 86           | 84           | 84           |
| Age (years) | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Age (years) | Median    | 76.0         | 76.0         | 77.5         |
| Age (years) | Q1, Q3    | 69.2, 81.8   | 70.8, 80.0   | 71.0, 82.0   |
| Age (years) | Min, Max  | 52, 89       | 56, 88       | 51, 88       |

Notice how the numbers align within each column. The `xx.x` format for
`mean` creates a field three characters wide before the decimal and one
after, so a mean of `75.2` prints as `75.2` while a mean of `8.3` would
print as `8.3` (with a leading space). This consistent field width is
what makes clinical tables scannable.

## Uppercase X: Parenthesis Hugging

Standard `x` formatting pads numbers inside their surrounding
delimiters. This means a parenthesis or bracket can end up separated
from the number it encloses by one or more spaces, which can look
awkward:

     14 ( 16.3%)
      7 (  8.1%)

Uppercase `X` solves this with **parenthesis hugging**. When a format
group uses `X` characters, any leading spaces that would normally appear
inside the number field are shifted *outside* the preceding delimiter.
The result is that the opening parenthesis (or bracket) always sits
immediately next to the first significant digit.

``` r

# Standard formatting: spaces inside parentheses
spec_standard <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        format_strings = list(
          n_counts = f_str("xxx (xxx.x%)", "n", "pct")
        )
      )
    )
  )
)

result_standard <- tplyr_build(spec_standard, tplyr_adsl)

# Parenthesis hugging: spaces shift outside the delimiter
spec_hugged <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        format_strings = list(
          n_counts = f_str("xxx (XXX.x%)", "n", "pct")
        )
      )
    )
  )
)

result_hugged <- tplyr_build(spec_hugged, tplyr_adsl)
```

Here is the standard formatting, where padding sits between the
parenthesis and the number:

``` r

kable(result_standard[1:6, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1          | res1        | res2        | res3        |
|:-------------------|:------------|:------------|:------------|
| ADVERSE EVENT      | 8 ( 9.3%)   | 40 ( 47.6%) | 44 ( 52.4%) |
| COMPLETED          | 58 ( 67.4%) | 27 ( 32.1%) | 25 ( 29.8%) |
| DEATH              | 2 ( 2.3%)   | 0 ( 0.0%)   | 1 ( 1.2%)   |
| LACK OF EFFICACY   | 3 ( 3.5%)   | 1 ( 1.2%)   | 0 ( 0.0%)   |
| LOST TO FOLLOW-UP  | 1 ( 1.2%)   | 0 ( 0.0%)   | 1 ( 1.2%)   |
| PHYSICIAN DECISION | 1 ( 1.2%)   | 2 ( 2.4%)   | 0 ( 0.0%)   |

And here is the same data with parenthesis hugging applied to the
percentage group. The opening parenthesis now hugs the number, and the
displaced spaces move to the left of the parenthesis:

``` r

kable(result_hugged[1:6, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1          | res1        | res2        | res3        |
|:-------------------|:------------|:------------|:------------|
| ADVERSE EVENT      | 8 (9.3% )   | 40 (47.6% ) | 44 (52.4% ) |
| COMPLETED          | 58 (67.4% ) | 27 (32.1% ) | 25 (29.8% ) |
| DEATH              | 2 (2.3% )   | 0 (0.0% )   | 1 (1.2% )   |
| LACK OF EFFICACY   | 3 (3.5% )   | 1 (1.2% )   | 0 (0.0% )   |
| LOST TO FOLLOW-UP  | 1 (1.2% )   | 0 (0.0% )   | 1 (1.2% )   |
| PHYSICIAN DECISION | 1 (1.2% )   | 2 (2.4% )   | 0 (0.0% )   |

The total string width stays the same – the spaces do not disappear,
they just relocate. This preserves column alignment while giving the
output a cleaner look.

Parenthesis hugging works with any delimiter that appears as a literal
in the format string, including square brackets `[` and other
characters.

## Auto-Precision with a and A

In descriptive statistics tables, the appropriate number of decimal
places often depends on the data itself. A lab parameter measured to one
decimal place should be summarized with one or two decimal digits, while
one measured to three decimal places needs more. Hardcoding widths for
every parameter is tedious and error-prone.

The `a` and `A` characters enable **auto-precision**: the field width is
determined at build time from the actual data. Specifically, tplyr2
scans the target variable, finds the maximum number of decimal places
present, and uses that as the base precision.

- `a` – auto-precision digit (like `x`, but width comes from data)
- `A` – auto-precision with parenthesis hugging (like `X`, but width
  comes from data)
- `+N` suffix – adds N to the auto-determined width (e.g., `a+1` means
  data precision plus one extra decimal place)

Auto-precision is controlled by three settings in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md):

- `precision_by`: character vector of grouping variables (precision
  computed per group)
- `precision_on`: character name of the variable to scan for precision
  (defaults to target variable)
- `precision_cap`: named numeric vector `c(int = , dec = )` to cap the
  maximum widths

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT %in% c("Baseline", "Week 12", "Week 24"),
      settings = layer_settings(
        precision_on = "AVAL",
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("a+1.a+1 (a+2.a+2)", "mean", "sd"),
          "Median"     = f_str("a+1.a+1", "median"),
          "Q1, Q3"     = f_str("a+1.a+1, a+1.a+1", "q1", "q3"),
          "Min, Max"   = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")])
```

| rowlabel1 | rowlabel2 | res1 | res2 | res3 |
|:---|:---|:---|:---|:---|
| Urate (umol/L) | n | 20 | 21 | 11 |
| Urate (umol/L) | Mean (SD) | 324.7608 ( 74.75026) | 298.8162 ( 50.36185) | 282.2596 ( 77.20328) |
| Urate (umol/L) | Median | 306.3220 | 297.4000 | 273.6080 |
| Urate (umol/L) | Q1, Q3 | 266.1730, 394.0550 | 273.6080, 315.2440 | 240.8940, 297.4000 |
| Urate (umol/L) | Min, Max | 226.024, 469.892 | 231.972, 469.892 | 178.440, 428.256 |

In this example, `precision_on = "AVAL"` tells tplyr2 to scan the `AVAL`
column to determine the number of decimal places present in the data.
The `+1` and `+2` suffixes add one and two extra decimal places beyond
what the data contains. For `Min, Max`, `a.a` uses the raw data
precision with no extra digits.

When `precision_by` is also set, precision is computed separately for
each group – useful when a single spec covers multiple lab parameters,
each measured at a different scale. This means you can write one spec
that handles dozens of parameters, each rendered at the precision
appropriate to its measurement.

Auto-precision (`a`/`A`) is a descriptive-layer feature – it is driven
by scanning a continuous variable’s decimal precision, which count
layers do not do. In a count format, `x`/`X` are fixed-width fields;
simply reserve enough `x` characters for the largest count you expect.
See
[`vignette("desc_layer_formatting")`](https://github.com/mstackhouse/tplyr2/articles/desc_layer_formatting.md)
for the full auto-precision system.

## The empty Argument

When a statistic is `NA` (for example, standard deviation when `n = 1`),
format strings produce blank space by default to preserve alignment. You
can override this with the `empty` argument to
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md),
which specifies a replacement string when all values in a format group
are missing.

``` r

fmt_with_empty <- f_str(
  "xx.x (xx.xx)",
  "mean", "sd",
  empty = c(.overall = "   -")
)
fmt_with_empty
#> tplyr format string: "xx.x (xx.xx)"Variables: mean, sdEmpty: c(.overall = "   -")
```

The `.overall` key means the replacement applies when *all* statistics
in the string are `NA`. This is useful for displaying a dash or other
placeholder in rows where a summary cannot be computed.

## Putting It All Together

Let us build a more complete example: an adverse event summary that
combines parenthesis hugging with fixed-width fields, similar to what
you would see in a clinical study report.

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (XXX.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2", indent = "   ")
kable(head(collapsed[, c("row_label", "res1", "res2", "res3")], 15))
```

| row_label                                  | res1      | res2      | res3      |
|:-------------------------------------------|:----------|:----------|:----------|
| CARDIAC DISORDERS                          |           |           |           |
|                                            | 4 (4.7% ) | 6 (7.1% ) | 5 (6.0% ) |
| ATRIAL FIBRILLATION                        | 0 (0.0% ) | 0 (0.0% ) | 1 (1.2% ) |
| ATRIAL FLUTTER                             | 0 (0.0% ) | 1 (1.2% ) | 0 (0.0% ) |
| ATRIAL HYPERTROPHY                         | 1 (1.2% ) | 0 (0.0% ) | 0 (0.0% ) |
| BUNDLE BRANCH BLOCK RIGHT                  | 1 (1.2% ) | 0 (0.0% ) | 0 (0.0% ) |
| CARDIAC FAILURE CONGESTIVE                 | 1 (1.2% ) | 0 (0.0% ) | 0 (0.0% ) |
| MYOCARDIAL INFARCTION                      | 0 (0.0% ) | 1 (1.2% ) | 2 (2.4% ) |
| SINUS BRADYCARDIA                          | 0 (0.0% ) | 3 (3.6% ) | 1 (1.2% ) |
| SUPRAVENTRICULAR EXTRASYSTOLES             | 1 (1.2% ) | 0 (0.0% ) | 1 (1.2% ) |
| SUPRAVENTRICULAR TACHYCARDIA               | 0 (0.0% ) | 0 (0.0% ) | 1 (1.2% ) |
| TACHYCARDIA                                | 1 (1.2% ) | 0 (0.0% ) | 0 (0.0% ) |
| VENTRICULAR EXTRASYSTOLES                  | 0 (0.0% ) | 1 (1.2% ) | 0 (0.0% ) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS |           |           |           |
|                                            | 0 (0.0% ) | 1 (1.2% ) | 0 (0.0% ) |

In this table:

- `xxx` gives a three-digit fixed field for distinct subject counts.
- `XXX.x` uses parenthesis hugging so the `(` sits right next to the
  percentage value.
- The `%` sign is literal text that appears after the percentage in
  every cell.
- `distinct_n` and `distinct_pct` compute subject-level (not
  event-level) summaries – and because this is an adverse event table,
  [`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md)
  supplies the safety-population denominator from `ADSL` so the
  percentages are correct.
- Nested counts display body system totals alongside preferred term
  detail.

The format string system in tplyr2 is designed so that you declare the
*shape* of each number field once, and the package handles padding,
alignment, and precision across every cell in the table. Whether you
need fixed widths, data-driven precision, or delimiter-hugging
alignment, the same
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
interface covers all three.

## Formatting Values Outside a Layer

[`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)
also works standalone, to format a numeric vector with an `f_str`
outside of any layer – useful when you compute a statistic yourself (an
ANCOVA estimate, a model p-value) and want to row-bind it onto an
assembled table with matching alignment. For that use it gains three
extra arguments: `na` (a string to substitute when a cell’s inputs are
all missing, e.g. `na = "NE"` or `na = ""`), and `width`/`pad` (to pad
each token to a fixed width). See
[`vignette("binding-statistics")`](https://github.com/mstackhouse/tplyr2/articles/binding-statistics.md)
for the binding workflow.

## See Also

- [`vignette("desc_layer_formatting")`](https://github.com/mstackhouse/tplyr2/articles/desc_layer_formatting.md)
  – auto-precision and the descriptive-layer formatting features.
- [`vignette("binding-statistics")`](https://github.com/mstackhouse/tplyr2/articles/binding-statistics.md)
  –
  [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)
  for row-binding external statistics.
- [`vignette("count")`](https://github.com/mstackhouse/tplyr2/articles/count.md)
  and
  [`vignette("desc")`](https://github.com/mstackhouse/tplyr2/articles/desc.md)
  – format strings in the context of each layer type.
