# Post-Processing

``` r
library(tplyr2)
library(knitr)
```

## Introduction

[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
produces a data.frame with `rowlabel*`, `res*`, and `ord_layer_*`
columns. This output is complete and correct, but it is not yet ready
for a polished report. Row labels still repeat across consecutive rows.
Multiple label columns need collapsing into one. And depending on the
output format, leading whitespace may need special handling.

tplyr2 provides a set of post-processing functions that transform build
output into display-ready tables. This vignette walks through each of
them, starting with the most common operations and ending with utility
functions for targeted tasks.

## Building the Example Data

We will use a multi-layer table throughout this vignette so that the
effect of each post-processing step is visible. The spec combines a
demographics count layer with a descriptive statistics layer on age.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      by = c(label("Disposition"), "EOSSTT"),
      settings = layer_settings(
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "n", "pct")
        )
      )
    ),
    group_desc("AGE",
      by = label("Age (years)"),
      settings = layer_settings(
        format_strings = list(
          "Mean (SD)" = f_str("xxx.x (xxx.xx)", "mean", "sd"),
          "Median"    = f_str("xxx.x", "median"),
          "Min, Max"  = f_str("xxx, xxx", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 12))
```

| rowlabel1   | rowlabel2    | res1       | res2       | res3       |
|:------------|:-------------|:-----------|:-----------|:-----------|
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | COMPLETED    | 58 (67.4%) | 27 (32.1%) | 25 (29.8%) |
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | DISCONTINUED | 8 ( 9.3%)  | 40 (47.6%) | 44 (52.4%) |
| Disposition | DISCONTINUED | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Disposition | DISCONTINUED | 2 ( 2.3%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |

Notice that `rowlabel1` repeats across rows within each grouping, and
that the two layers are stacked without any visual separation. The
post-processing functions address both of these issues.

## Row Masks

[`apply_row_masks()`](https://github.com/mstackhouse/tplyr2/reference/apply_row_masks.md)
walks each `rowlabel*` column top-to-bottom and blanks any value that is
identical to the row above it. This deduplication respects layer
boundaries, so a label that appears at the end of one layer and the
beginning of another is never accidentally blanked.

``` r
masked <- apply_row_masks(result)
kable(head(masked[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 12))
```

| rowlabel1   | rowlabel2    | res1       | res2       | res3       |
|:------------|:-------------|:-----------|:-----------|:-----------|
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 58 (67.4%) | 27 (32.1%) | 25 (29.8%) |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             | DISCONTINUED | 8 ( 9.3%)  | 40 (47.6%) | 44 (52.4%) |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 2 ( 2.3%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |

### Adding Row Breaks Between Layers

When `row_breaks = TRUE`, a blank row is inserted at every layer
boundary. Combined with masking, this gives the table a clean, sectioned
appearance.

``` r
masked_breaks <- apply_row_masks(result, row_breaks = TRUE)
kable(head(masked_breaks[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 14))
```

| rowlabel1   | rowlabel2    | res1       | res2       | res3       |
|:------------|:-------------|:-----------|:-----------|:-----------|
| Disposition | COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 58 (67.4%) | 27 (32.1%) | 25 (29.8%) |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             | DISCONTINUED | 8 ( 9.3%)  | 40 (47.6%) | 44 (52.4%) |
|             |              | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
|             |              | 2 ( 2.3%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
|             |              | 3 ( 3.5%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
|             |              | 1 ( 1.2%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |

## Collapsing Row Labels

Many display formats expect a single row label column rather than
separate `rowlabel1`, `rowlabel2`, etc.
[`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md)
takes the specified columns and collapses them into one column.
Repeating parent values are split into their own rows, and each nesting
level receives progressively more indentation.

``` r
collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2", indent = "   ")
kable(head(collapsed[, c("row_label", "res1", "res2", "res3")], 12))
```

| row_label    | res1       | res2       | res3       |
|:-------------|:-----------|:-----------|:-----------|
| Disposition  |            |            |            |
| COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| COMPLETED    | 58 (67.4%) | 27 (32.1%) | 25 (29.8%) |
| COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| COMPLETED    | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| DISCONTINUED | 8 ( 9.3%)  | 40 (47.6%) | 44 (52.4%) |
| DISCONTINUED | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |

The columns to collapse are provided as character strings (at least two
are required). The `indent` parameter accepts any string — it is
repeated at each nesting level. The output column name defaults to
`"row_label"` but can be changed via `target_col`.

## Extracting Numeric Values

Formatted tplyr2 strings like `" 5 ( 6.1%)"` are useful for display, but
sometimes you need the underlying numbers for programmatic comparisons,
sorting, or conditional formatting.
[`str_extract_num()`](https://github.com/mstackhouse/tplyr2/reference/str_extract_num.md)
pulls out the Nth numeric value from each string.

``` r
# Extract the count (first number) from the first result column
counts <- str_extract_num(result$res1, index = 1)
head(counts, 8)
#> [1]  0 58  0  0  0  0  0  0

# Extract the percentage (second number)
pcts <- str_extract_num(result$res1, index = 2)
head(pcts, 8)
#> [1]  0.0 67.4  0.0  0.0  0.0  0.0  0.0  0.0
```

This function handles negative numbers, decimals, and missing values
gracefully. It returns `NA` when the requested index exceeds the number
of numeric values in a cell.

## Conditional Formatting

[`apply_conditional_format()`](https://github.com/mstackhouse/tplyr2/reference/apply_conditional_format.md)
allows you to conditionally re-format a string of numbers based on a
numeric value within the string itself. By selecting a “format group”
(targeting a specific number within the string, numbered left to right),
you can establish a condition upon which a replacement string is used.
Either the replacement can replace the entire string, or it can refill
just the format group while preserving the original width and alignment.

``` r
string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")

# Replace the full string when the percentage (2nd format group) is 0
apply_conditional_format(string, 2, x == 0, " 0        ", full_string = TRUE)
#> [1] " 0        " " 8  (9.3%)" "78 (90.7%)"

# Replace within the format group when the percentage is less than 1
apply_conditional_format(string, 2, x < 1, "(<1%)")
#> [1] " 0   (<1%)" " 8  (9.3%)" "78 (90.7%)"
```

The `format_group` parameter selects which numeric value in the string
to evaluate (1st number, 2nd number, etc.). The `condition` is an
expression using the variable name `x` that tests the selected number.
When `full_string = FALSE` (the default), the replacement is padded to
preserve column alignment within the format group’s character space.

## Replacing Leading Whitespace

tplyr2 uses leading spaces to align numbers within format fields. This
works in fixed-width contexts (PDFs, monospaced fonts), but HTML
collapses consecutive spaces.
[`replace_leading_whitespace()`](https://github.com/mstackhouse/tplyr2/reference/replace_leading_whitespace.md)
swaps each leading space for a non-breaking space (`\u00a0`), preserving
alignment in web-based output.

``` r
original <- c("  5 ( 6.1%)", " 12 (14.6%)", "  3 ( 3.7%)")
replaced <- replace_leading_whitespace(original)

# Show the difference (non-breaking spaces are invisible but present)
nchar(original)
#> [1] 11 11 11
nchar(replaced)
#> [1] 11 11 11
```

The `replace_with` parameter defaults to `"\u00a0"` but can be set to
any string. For example, you might use `"&nbsp;"` for raw HTML output.

## Standalone Format Application

The
[`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)
function is the engine behind all of tplyr2’s string formatting, but it
can also be used on its own. Given an `f_str` object and matching
numeric vectors, it returns formatted character strings.

``` r
fmt <- f_str("xxx.x (xxx.xx)", "mean", "sd")
apply_formats(fmt, c(75.3, 68.1, 80.5), c(8.21, 7.55, 9.03))
#> [1] " 75.3 (  8.21)" " 68.1 (  7.55)" " 80.5 (  9.03)"
```

This is useful when you need to format numbers from external data
sources using the same format strings that drive your tplyr2 tables. The
precision system is also available through the `precision` parameter for
auto-precision formatting.

## Text Wrapping

[`str_indent_wrap()`](https://github.com/mstackhouse/tplyr2/reference/str_indent_wrap.md)
wraps long text strings to a specified width while automatically
preserving any existing indentation and applying hyphenation to words
that exceed the column width.

``` r
ex_text <- c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS")
cat(paste(str_indent_wrap(ex_text, width = 8), collapse = "\n\n"), "\n")
#> RENAL
#> AND
#> URINARY
#> DISORDE-
#> RS
#> 
#>    NEPHROL-
#>    ITHIASI-
#>    S
```

The function automatically detects leading whitespace in each element
and preserves it on wrapped continuation lines. Long words that exceed
the column width are split with hyphens. Tabs are converted to spaces
using the `tab_width` parameter (default 5).

## Putting It All Together

In practice, you will chain several post-processing steps together. Here
is a complete pipeline that takes raw build output and produces a
display-ready table.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        total_row = TRUE,
        total_row_label = "Any adverse event"
      )
    )
  )
)

output <- tplyr_build(spec, tplyr_adae)

# Post-processing pipeline
display <- output |>
  collapse_row_labels("rowlabel1", "rowlabel2", indent = "   ")

kable(head(display[, c("row_label", "res1", "res2", "res3")], 20))
```

| row_label                                  | res1        | res2        | res3        |
|:-------------------------------------------|:------------|:------------|:------------|
| Any adverse event                          |             |             |             |
|                                            | 32 (100.0%) | 43 (100.0%) | 50 (100.0%) |
| CARDIAC DISORDERS                          |             |             |             |
|                                            | 4 (12.5%)   | 6 (14.0%)   | 5 (10.0%)   |
| ATRIAL FIBRILLATION                        | 0 ( 0.0%)   | 0 ( 0.0%)   | 1 ( 2.0%)   |
| ATRIAL FLUTTER                             | 0 ( 0.0%)   | 1 ( 2.3%)   | 0 ( 0.0%)   |
| ATRIAL HYPERTROPHY                         | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| BUNDLE BRANCH BLOCK RIGHT                  | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| CARDIAC FAILURE CONGESTIVE                 | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| MYOCARDIAL INFARCTION                      | 0 ( 0.0%)   | 1 ( 2.3%)   | 2 ( 4.0%)   |
| SINUS BRADYCARDIA                          | 0 ( 0.0%)   | 3 ( 7.0%)   | 1 ( 2.0%)   |
| SUPRAVENTRICULAR EXTRASYSTOLES             | 1 ( 3.1%)   | 0 ( 0.0%)   | 1 ( 2.0%)   |
| SUPRAVENTRICULAR TACHYCARDIA               | 0 ( 0.0%)   | 0 ( 0.0%)   | 1 ( 2.0%)   |
| TACHYCARDIA                                | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| VENTRICULAR EXTRASYSTOLES                  | 0 ( 0.0%)   | 1 ( 2.3%)   | 0 ( 0.0%)   |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS |             |             |             |
|                                            | 0 ( 0.0%)   | 1 ( 2.3%)   | 0 ( 0.0%)   |
| VENTRICULAR SEPTAL DEFECT                  | 0 ( 0.0%)   | 1 ( 2.3%)   | 0 ( 0.0%)   |
| GASTROINTESTINAL DISORDERS                 |             |             |             |
|                                            | 6 (18.8%)   | 4 ( 9.3%)   | 3 ( 6.0%)   |

When combining
[`apply_row_masks()`](https://github.com/mstackhouse/tplyr2/reference/apply_row_masks.md)
and
[`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md),
note that
[`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md)
inserts its own stub rows and removes the original label columns, so it
typically replaces the need for
[`apply_row_masks()`](https://github.com/mstackhouse/tplyr2/reference/apply_row_masks.md).
For deeply nested tables (3+ label columns), applying
[`apply_row_masks()`](https://github.com/mstackhouse/tplyr2/reference/apply_row_masks.md)
on the raw output first can still be useful.

### Adding Conditional Formatting to the Pipeline

You can apply conditional formatting to the result columns before
collapsing labels. Here we replace the percentage with `(<1%)` in the
placebo arm when the percentage rounds to 0 but the count is non-zero.

``` r
# Apply conditional formatting before collapsing labels
output$res1 <- apply_conditional_format(
  output$res1, 2, x == 0, "(<1%)"
)

display_formatted <- output |>
  collapse_row_labels("rowlabel1", "rowlabel2", indent = "   ")

kable(head(display_formatted[, c("row_label", "res1", "res2", "res3")], 15))
```

| row_label                      | res1        | res2        | res3        |
|:-------------------------------|:------------|:------------|:------------|
| Any adverse event              |             |             |             |
|                                | 32 (100.0%) | 43 (100.0%) | 50 (100.0%) |
| CARDIAC DISORDERS              |             |             |             |
|                                | 4 (12.5%)   | 6 (14.0%)   | 5 (10.0%)   |
| ATRIAL FIBRILLATION            | 0 (\<1%)    | 0 ( 0.0%)   | 1 ( 2.0%)   |
| ATRIAL FLUTTER                 | 0 (\<1%)    | 1 ( 2.3%)   | 0 ( 0.0%)   |
| ATRIAL HYPERTROPHY             | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| BUNDLE BRANCH BLOCK RIGHT      | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| CARDIAC FAILURE CONGESTIVE     | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| MYOCARDIAL INFARCTION          | 0 (\<1%)    | 1 ( 2.3%)   | 2 ( 4.0%)   |
| SINUS BRADYCARDIA              | 0 (\<1%)    | 3 ( 7.0%)   | 1 ( 2.0%)   |
| SUPRAVENTRICULAR EXTRASYSTOLES | 1 ( 3.1%)   | 0 ( 0.0%)   | 1 ( 2.0%)   |
| SUPRAVENTRICULAR TACHYCARDIA   | 0 (\<1%)    | 0 ( 0.0%)   | 1 ( 2.0%)   |
| TACHYCARDIA                    | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| VENTRICULAR EXTRASYSTOLES      | 0 (\<1%)    | 1 ( 2.3%)   | 0 ( 0.0%)   |

## Summary

The post-processing functions in tplyr2 serve distinct purposes but are
designed to work together:

| Function                                                                                                        | Purpose                                                            |
|:----------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------|
| [`apply_row_masks()`](https://github.com/mstackhouse/tplyr2/reference/apply_row_masks.md)                       | Blank repeated row labels, optionally insert row breaks            |
| [`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md)               | Merge label columns into one with indentation                      |
| [`str_extract_num()`](https://github.com/mstackhouse/tplyr2/reference/str_extract_num.md)                       | Pull numeric values from formatted strings                         |
| [`apply_conditional_format()`](https://github.com/mstackhouse/tplyr2/reference/apply_conditional_format.md)     | Conditionally reformat strings based on numeric values within them |
| [`replace_leading_whitespace()`](https://github.com/mstackhouse/tplyr2/reference/replace_leading_whitespace.md) | Swap leading spaces for non-breaking spaces                        |
| [`apply_formats()`](https://github.com/mstackhouse/tplyr2/reference/apply_formats.md)                           | Format numeric vectors using f_str objects                         |
| [`str_indent_wrap()`](https://github.com/mstackhouse/tplyr2/reference/str_indent_wrap.md)                       | Wrap long text with hyphenation and indentation preservation       |

A typical pipeline runs
[`apply_row_masks()`](https://github.com/mstackhouse/tplyr2/reference/apply_row_masks.md)
first, then
[`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md).
Conditional formatting and whitespace replacement can be inserted
wherever they make sense for your output format.
