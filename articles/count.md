# Count Layers

``` r
library(tplyr2)
library(knitr)
```

## Introduction

Counting things seems simple enough. You look at the data, tally up how
many observations fall into each category, and move on. But in clinical
reporting, counting is deceptively nuanced. A demographics table needs
simple frequencies. An adverse events table needs both the number of
subjects who experienced an event and the total number of events. A
disposition table might need all of those plus custom column groupings
and sorting by frequency.

tplyr2 handles all of these scenarios through
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md).
In this vignette, we will walk through the most common counting patterns
you will encounter when building clinical tables, starting with the
basics and building up to nested hierarchical summaries.

## A Simple Count

The simplest use of
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
is to count the levels of a single categorical variable. Here we build a
basic disposition table showing the reasons subjects discontinued from
the study, broken out by planned treatment group.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1                   | res1       | res2       | res3       |
|:----------------------------|:-----------|:-----------|:-----------|
| ADVERSE EVENT               | 8 ( 9.3%)  | 40 (47.6%) | 44 (52.4%) |
| COMPLETED                   | 58 (67.4%) | 27 (32.1%) | 25 (29.8%) |
| DEATH                       | 2 ( 2.3%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| LACK OF EFFICACY            | 3 ( 3.5%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| PHYSICIAN DECISION          | 1 ( 1.2%)  | 2 ( 2.4%)  | 0 ( 0.0%)  |
| PROTOCOL VIOLATION          | 2 ( 2.3%)  | 3 ( 3.6%)  | 1 ( 1.2%)  |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)  | 3 ( 3.6%)  | 2 ( 2.4%)  |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)  | 8 ( 9.5%)  | 10 (11.9%) |

By default, tplyr2 formats counts as `n (xx.x%)`, where the denominator
is the total number of observations within each treatment column. Every
level present in the data gets a row, and zero counts are filled in
automatically so that no treatment arm is left with a gap.

### Adding a Total Column

In many tables, you need a “Total” column that pools all treatment
groups. You can add one at the spec level with
[`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md).

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  total_groups = list(total_group("TRT01P", label = "Total")),
  layers = tplyr_layers(
    group_count("DCDECOD")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3", "res4")])
```

| rowlabel1                   | res1       | res2        | res3       | res4       |
|:----------------------------|:-----------|:------------|:-----------|:-----------|
| ADVERSE EVENT               | 8 ( 9.3%)  | 92 (36.2%)  | 40 (47.6%) | 44 (52.4%) |
| COMPLETED                   | 58 (67.4%) | 110 (43.3%) | 27 (32.1%) | 25 (29.8%) |
| DEATH                       | 2 ( 2.3%)  | 3 ( 1.2%)   | 0 ( 0.0%)  | 1 ( 1.2%)  |
| LACK OF EFFICACY            | 3 ( 3.5%)  | 4 ( 1.6%)   | 1 ( 1.2%)  | 0 ( 0.0%)  |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)  | 2 ( 0.8%)   | 0 ( 0.0%)  | 1 ( 1.2%)  |
| PHYSICIAN DECISION          | 1 ( 1.2%)  | 3 ( 1.2%)   | 2 ( 2.4%)  | 0 ( 0.0%)  |
| PROTOCOL VIOLATION          | 2 ( 2.3%)  | 6 ( 2.4%)   | 3 ( 3.6%)  | 1 ( 1.2%)  |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)  | 7 ( 2.8%)   | 3 ( 3.6%)  | 2 ( 2.4%)  |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)  | 27 (10.6%)  | 8 ( 9.5%)  | 10 (11.9%) |

The
[`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md)
function works by duplicating every row in the data with the treatment
variable set to the label you provide (here, “Total”). This means the
total column is computed through the same pipeline as every other
column, keeping the logic consistent.

### Custom Column Groups

Sometimes you need to combine existing treatment arms into a new group.
For example, you might want a “Treated” column that pools all active
dose groups. The
[`custom_group()`](https://github.com/mstackhouse/tplyr2/reference/custom_group.md)
function handles this.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  custom_groups = list(
    custom_group("TRT01P",
      "Treated" = c("Xanomeline High Dose", "Xanomeline Low Dose")
    )
  ),
  layers = tplyr_layers(
    group_count("DCDECOD")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3", "res4")])
```

| rowlabel1                   | res1       | res2       | res3       | res4       |
|:----------------------------|:-----------|:-----------|:-----------|:-----------|
| ADVERSE EVENT               | 8 ( 9.3%)  | 84 (50.0%) | 40 (47.6%) | 44 (52.4%) |
| COMPLETED                   | 58 (67.4%) | 52 (31.0%) | 27 (32.1%) | 25 (29.8%) |
| DEATH                       | 2 ( 2.3%)  | 1 ( 0.6%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| LACK OF EFFICACY            | 3 ( 3.5%)  | 1 ( 0.6%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)  | 1 ( 0.6%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| PHYSICIAN DECISION          | 1 ( 1.2%)  | 2 ( 1.2%)  | 2 ( 2.4%)  | 0 ( 0.0%)  |
| PROTOCOL VIOLATION          | 2 ( 2.3%)  | 4 ( 2.4%)  | 3 ( 3.6%)  | 1 ( 1.2%)  |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)  | 5 ( 3.0%)  | 3 ( 3.6%)  | 2 ( 2.4%)  |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)  | 18 (10.7%) | 8 ( 9.5%)  | 10 (11.9%) |

Like
[`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md),
custom groups duplicate the matching rows with the treatment variable
set to the new label. Custom groups and total groups can be combined in
a single spec when you need both.

### Custom Format Strings

The default `n (xx.x%)` format works for many tables, but you have full
control over how counts are displayed through the
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
function. Format strings use `x` characters to define field widths: each
`x` reserves one character position.

``` r
spec <- tplyr_spec(
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

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1                   | res1        | res2        | res3        |
|:----------------------------|:------------|:------------|:------------|
| ADVERSE EVENT               | 8 ( 9.3%)   | 40 ( 47.6%) | 44 ( 52.4%) |
| COMPLETED                   | 58 ( 67.4%) | 27 ( 32.1%) | 25 ( 29.8%) |
| DEATH                       | 2 ( 2.3%)   | 0 ( 0.0%)   | 1 ( 1.2%)   |
| LACK OF EFFICACY            | 3 ( 3.5%)   | 1 ( 1.2%)   | 0 ( 0.0%)   |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)   | 0 ( 0.0%)   | 1 ( 1.2%)   |
| PHYSICIAN DECISION          | 1 ( 1.2%)   | 2 ( 2.4%)   | 0 ( 0.0%)   |
| PROTOCOL VIOLATION          | 2 ( 2.3%)   | 3 ( 3.6%)   | 1 ( 1.2%)   |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)   | 3 ( 3.6%)   | 2 ( 2.4%)   |
| WITHDRAWAL BY SUBJECT       | 9 ( 10.5%)  | 8 ( 9.5%)   | 10 ( 11.9%) |

The available count statistics are:

- `n` – the number of observations
- `pct` – the percentage of observations (using the column total as
  denominator)
- `total` – the denominator used for the percentage calculation
- `distinct_n` – the number of distinct subjects (requires
  `distinct_by`)
- `distinct_pct` – the percentage of distinct subjects
- `distinct_total` – the distinct denominator

You can combine any of these in a single format string. We will use the
distinct variants in the next section.

### Total Rows

Many tables require a “Total” row at the bottom of each count block. Set
`total_row = TRUE` in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
to add one.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        total_row = TRUE,
        total_row_label = "Overall Total"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1                   | res1        | res2        | res3        |
|:----------------------------|:------------|:------------|:------------|
| ADVERSE EVENT               | 8 ( 9.3%)   | 40 (47.6%)  | 44 (52.4%)  |
| COMPLETED                   | 58 (67.4%)  | 27 (32.1%)  | 25 (29.8%)  |
| DEATH                       | 2 ( 2.3%)   | 0 ( 0.0%)   | 1 ( 1.2%)   |
| LACK OF EFFICACY            | 3 ( 3.5%)   | 1 ( 1.2%)   | 0 ( 0.0%)   |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)   | 0 ( 0.0%)   | 1 ( 1.2%)   |
| Overall Total               | 86 (100.0%) | 84 (100.0%) | 84 (100.0%) |
| PHYSICIAN DECISION          | 1 ( 1.2%)   | 2 ( 2.4%)   | 0 ( 0.0%)   |
| PROTOCOL VIOLATION          | 2 ( 2.3%)   | 3 ( 3.6%)   | 1 ( 1.2%)   |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)   | 3 ( 3.6%)   | 2 ( 2.4%)   |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)   | 8 ( 9.5%)   | 10 (11.9%)  |

## Distinct Versus Event Counts

When summarizing adverse events, the distinction between events and
subjects matters enormously. A single subject might experience the same
event multiple times. If you simply count rows, you are counting events.
If you want the number of subjects who experienced each event at least
once, you need distinct counts.

The `distinct_by` parameter in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
tells tplyr2 which variable identifies unique subjects. Once set, the
`distinct_n` and `distinct_pct` statistics become available for use in
format strings.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%) [xxx]", "distinct_n", "distinct_pct", "n")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3")], 10))
```

| rowlabel1                   | res1             | res2             | res3             |
|:----------------------------|:-----------------|:-----------------|:-----------------|
| ABDOMINAL PAIN              | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 2.0%) \[ 1\] |
| AGITATION                   | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 2.0%) \[ 1\] |
| ANXIETY                     | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 2.0%) \[ 1\] |
| APPLICATION SITE DERMATITIS | 1 ( 3.1%) \[ 1\] | 3 ( 7.0%) \[ 3\] | 2 ( 4.0%) \[ 2\] |
| APPLICATION SITE ERYTHEMA   | 0 ( 0.0%) \[ 0\] | 3 ( 7.0%) \[ 3\] | 4 ( 8.0%) \[ 4\] |
| APPLICATION SITE IRRITATION | 1 ( 3.1%) \[ 1\] | 3 ( 7.0%) \[ 4\] | 2 ( 4.0%) \[ 2\] |
| APPLICATION SITE PAIN       | 0 ( 0.0%) \[ 0\] | 1 ( 2.3%) \[ 1\] | 0 ( 0.0%) \[ 0\] |
| APPLICATION SITE PRURITUS   | 4 (12.5%) \[ 4\] | 6 (14.0%) \[ 7\] | 5 (10.0%) \[ 5\] |
| APPLICATION SITE REACTION   | 1 ( 3.1%) \[ 1\] | 1 ( 2.3%) \[ 1\] | 0 ( 0.0%) \[ 0\] |
| APPLICATION SITE URTICARIA  | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 2.0%) \[ 1\] |

In this output, the first number is the count of distinct subjects, the
percentage is based on distinct subjects, and the number in brackets is
the total event count. This pattern – `xxx (xx.x%) [xxx]` for distinct
subjects, percent, and events – is one of the most common formats for
adverse event tables.

### A Note on Parenthesis Hugging

You may notice that tplyr2 carefully aligns numbers within their format
fields, padding with leading spaces so that decimal points and
parentheses line up across rows. For even tighter alignment, tplyr2
supports “parenthesis hugging” using uppercase `X` characters in format
strings. When you use `X` instead of `x`, leading spaces shift outside
the parenthesis so the opening delimiter sits right next to the number.
This is a more advanced feature covered in detail in the format strings
documentation.

## Nested Count Summaries

Adverse event tables in clinical reports almost always use a
hierarchical structure: body system (SOC) as the outer grouping and
preferred term (PT) as the inner grouping. tplyr2 supports this directly
by passing a vector of two variable names to
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md).

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 15))
```

| rowlabel1                                  | rowlabel2                      | res1      | res2      | res3      |
|:-------------------------------------------|:-------------------------------|:----------|:----------|:----------|
| CARDIAC DISORDERS                          |                                | 4 (12.5%) | 6 (14.0%) | 5 (10.0%) |
| CARDIAC DISORDERS                          | ATRIAL FIBRILLATION            | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) |
| CARDIAC DISORDERS                          | ATRIAL FLUTTER                 | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) |
| CARDIAC DISORDERS                          | ATRIAL HYPERTROPHY             | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS                          | BUNDLE BRANCH BLOCK RIGHT      | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS                          | CARDIAC FAILURE CONGESTIVE     | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS                          | MYOCARDIAL INFARCTION          | 0 ( 0.0%) | 1 ( 2.3%) | 2 ( 4.0%) |
| CARDIAC DISORDERS                          | SINUS BRADYCARDIA              | 0 ( 0.0%) | 3 ( 7.0%) | 1 ( 2.0%) |
| CARDIAC DISORDERS                          | SUPRAVENTRICULAR EXTRASYSTOLES | 1 ( 3.1%) | 0 ( 0.0%) | 1 ( 2.0%) |
| CARDIAC DISORDERS                          | SUPRAVENTRICULAR TACHYCARDIA   | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) |
| CARDIAC DISORDERS                          | TACHYCARDIA                    | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS                          | VENTRICULAR EXTRASYSTOLES      | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS |                                | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS | VENTRICULAR SEPTAL DEFECT      | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) |
| GASTROINTESTINAL DISORDERS                 |                                | 6 (18.8%) | 4 ( 9.3%) | 3 ( 6.0%) |

The first variable in the vector (`AEBODSYS`) becomes the outer level,
and the second (`AEDECOD`) becomes the inner level. In the output,
`rowlabel1` holds the body system name and `rowlabel2` holds the
preferred term. Outer-level summary rows have an empty `rowlabel2`,
while inner-level rows have their preferred term indented within
`rowlabel2`.

The outer-level counts represent the number of distinct subjects with
any event in that body system, and the inner-level counts represent
subjects with each specific preferred term. A subject who experienced
multiple different preferred terms within the same body system is
counted once at the outer level but once for each preferred term at the
inner level.

### Collapsing Row Labels

For display purposes, you often want a single row label column with
indentation indicating the hierarchy rather than two separate columns.
The
[`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md)
function merges the `rowlabel` columns into one.

``` r
collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2", indent = "   ")
kable(head(collapsed[, c("row_label", "res1", "res2", "res3")], 15))
```

| row_label                                  | res1      | res2      | res3      |
|:-------------------------------------------|:----------|:----------|:----------|
| CARDIAC DISORDERS                          |           |           |           |
|                                            | 4 (12.5%) | 6 (14.0%) | 5 (10.0%) |
| ATRIAL FIBRILLATION                        | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) |
| ATRIAL FLUTTER                             | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) |
| ATRIAL HYPERTROPHY                         | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| BUNDLE BRANCH BLOCK RIGHT                  | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC FAILURE CONGESTIVE                 | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| MYOCARDIAL INFARCTION                      | 0 ( 0.0%) | 1 ( 2.3%) | 2 ( 4.0%) |
| SINUS BRADYCARDIA                          | 0 ( 0.0%) | 3 ( 7.0%) | 1 ( 2.0%) |
| SUPRAVENTRICULAR EXTRASYSTOLES             | 1 ( 3.1%) | 0 ( 0.0%) | 1 ( 2.0%) |
| SUPRAVENTRICULAR TACHYCARDIA               | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) |
| TACHYCARDIA                                | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| VENTRICULAR EXTRASYSTOLES                  | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS |           |           |           |
|                                            | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) |

The `indent` parameter controls the string used for each level of
nesting. Here we use three spaces, but you can use any string that suits
your output format.

### Controlling Indentation at the Layer Level

You can also control the indentation of inner-level labels directly in
the layer settings using the `indentation` parameter. This sets the
prefix applied to `rowlabel2` values before any post-processing.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        indentation = "    ",
        format_strings = list(
          n_counts = f_str("xxx", "n")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 12))
```

| rowlabel1         | rowlabel2                      | res1 | res2 | res3 |
|:------------------|:-------------------------------|:-----|:-----|:-----|
| CARDIAC DISORDERS |                                | 5    | 6    | 6    |
| CARDIAC DISORDERS | ATRIAL FIBRILLATION            | 0    | 0    | 1    |
| CARDIAC DISORDERS | ATRIAL FLUTTER                 | 0    | 1    | 0    |
| CARDIAC DISORDERS | ATRIAL HYPERTROPHY             | 1    | 0    | 0    |
| CARDIAC DISORDERS | BUNDLE BRANCH BLOCK RIGHT      | 1    | 0    | 0    |
| CARDIAC DISORDERS | CARDIAC FAILURE CONGESTIVE     | 1    | 0    | 0    |
| CARDIAC DISORDERS | MYOCARDIAL INFARCTION          | 0    | 1    | 2    |
| CARDIAC DISORDERS | SINUS BRADYCARDIA              | 0    | 3    | 1    |
| CARDIAC DISORDERS | SUPRAVENTRICULAR EXTRASYSTOLES | 1    | 0    | 1    |
| CARDIAC DISORDERS | SUPRAVENTRICULAR TACHYCARDIA   | 0    | 0    | 1    |
| CARDIAC DISORDERS | TACHYCARDIA                    | 1    | 0    | 0    |
| CARDIAC DISORDERS | VENTRICULAR EXTRASYSTOLES      | 0    | 1    | 0    |

### Nested Counts with Total Rows

Total rows work with nested counts as well. When `total_row = TRUE` is
set on a nested layer, the total row reflects the overall count across
all body systems.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        total_row = TRUE,
        total_row_label = "Any adverse event",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2", indent = "   ")
kable(head(collapsed[, c("row_label", "res1", "res2", "res3")], 15))
```

| row_label                      | res1        | res2        | res3        |
|:-------------------------------|:------------|:------------|:------------|
| Any adverse event              |             |             |             |
|                                | 32 (100.0%) | 43 (100.0%) | 50 (100.0%) |
| CARDIAC DISORDERS              |             |             |             |
|                                | 4 (12.5%)   | 6 (14.0%)   | 5 (10.0%)   |
| ATRIAL FIBRILLATION            | 0 ( 0.0%)   | 0 ( 0.0%)   | 1 ( 2.0%)   |
| ATRIAL FLUTTER                 | 0 ( 0.0%)   | 1 ( 2.3%)   | 0 ( 0.0%)   |
| ATRIAL HYPERTROPHY             | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| BUNDLE BRANCH BLOCK RIGHT      | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| CARDIAC FAILURE CONGESTIVE     | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| MYOCARDIAL INFARCTION          | 0 ( 0.0%)   | 1 ( 2.3%)   | 2 ( 4.0%)   |
| SINUS BRADYCARDIA              | 0 ( 0.0%)   | 3 ( 7.0%)   | 1 ( 2.0%)   |
| SUPRAVENTRICULAR EXTRASYSTOLES | 1 ( 3.1%)   | 0 ( 0.0%)   | 1 ( 2.0%)   |
| SUPRAVENTRICULAR TACHYCARDIA   | 0 ( 0.0%)   | 0 ( 0.0%)   | 1 ( 2.0%)   |
| TACHYCARDIA                    | 1 ( 3.1%)   | 0 ( 0.0%)   | 0 ( 0.0%)   |
| VENTRICULAR EXTRASYSTOLES      | 0 ( 0.0%)   | 1 ( 2.3%)   | 0 ( 0.0%)   |

## Where to Go from Here

Count layers in tplyr2 cover a wide range of clinical table patterns,
but there is much more to explore. Here are some related topics covered
in other vignettes:

- **Denominators**: Control how percentages are calculated with
  `denoms_by` and `denom_ignore` in
  [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
- **Missing counts**: Add rows for missing values with the
  `missing_count` parameter.
- **Shift tables**: Use
  [`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md)
  for cross-tabulation of baseline versus post-baseline categories.
- **Descriptive statistics**: Use
  [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
  for continuous variable summaries like mean, median, and standard
  deviation.
- **Post-processing**: Apply row masks, conditional formatting, and row
  label collapsing to polish your final output.
