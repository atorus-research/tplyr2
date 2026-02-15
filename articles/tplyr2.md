# Getting Started with tplyr2

## How tplyr2 Works

If you have worked with clinical data long enough, you know that summary
tables – demographics tables, adverse event tables, lab shift tables –
all share a common structural pattern. Each section of the table
represents some kind of summary: a set of counts, a block of descriptive
statistics, or a cross-tabulation.

tplyr2 is built around this idea. Rather than writing bespoke data
manipulation code for every table, you describe *what* the table should
contain using a declarative specification, and tplyr2 handles the
computing, formatting, and assembly.

The key concepts are:

- **Spec**: A
  [`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md)
  object is pure configuration. It describes the column structure,
  global filters, and a list of layers. No data processing happens when
  you create a spec.
- **Layers**: Each layer is one summary block. Count layers
  ([`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md))
  produce frequencies. Descriptive layers
  ([`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md))
  compute means, medians, and other summaries. Shift layers
  ([`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md))
  create cross-tabulations. Custom layers
  ([`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md))
  accept user-defined functions.
- **Build**: `tplyr_build(spec, data)` executes the spec against a
  dataset and returns a formatted data frame.

This separation of configuration from execution is intentional. A spec
can be saved to disk, reviewed, reused across studies, or applied to
different datasets at build time. If you are familiar with the PHUSE
[Analyses & Displays White
Paper](https://phuse.s3.eu-central-1.amazonaws.com/Deliverables/Standard+Analyses+and+Code+Sharing/Analyses+%26+Displays+Associated+with+Demographics%2C+Disposition%2C+and+Medications+in+Phase+2-4+Clinical+Trials+and+Integrated+Summary+Documents.pdf),
the layer model maps naturally to the summary blocks described there.

## The tplyr_spec() Object

Every table starts with a
[`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md).
At minimum, you need the column variable (typically the treatment arm)
and at least one layer.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX"),
    group_desc("AGE")
  )
)
spec
#> tplyr2 table specification
#> Column variables: TRT01PLayers: 2[1] count: SEX (Layer 1)[2] desc: AGE (Layer 2)
```

The `cols` parameter determines which variable defines the output
columns. Each unique value becomes a result column. The function also
accepts optional parameters:

- `where`: A global filter expression (e.g., `where = SAFFL == "Y"`).
- `pop_data`: A
  [`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md)
  configuration for population-based denominators.
- `total_groups`: A list of
  [`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md)
  objects for “Total” columns.
- `custom_groups`: A list of
  [`custom_group()`](https://github.com/mstackhouse/tplyr2/reference/custom_group.md)
  objects for combined treatment arms.
- `layers`: Layer objects wrapped in
  [`tplyr_layers()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_layers.md).

The spec contains no data. Data is supplied at build time, so the same
spec can be reused across datasets.

## The tplyr_layer() Object

Layers are the building blocks of a tplyr2 table. You create them with
the `group_*()` family:
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md),
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md),
[`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md),
and
[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md).
Every layer constructor accepts:

- `target_var`: The variable(s) being summarized.
- `by`: Optional row grouping. Strings matching column names become
  grouping variables; other strings become text labels in the output.
  Use
  [`label()`](https://github.com/mstackhouse/tplyr2/reference/label.md)
  for explicit disambiguation.
- `where`: An optional layer-level filter expression.
- `settings`: A
  [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
  object for detailed configuration.

Multiple layers are collected with
[`tplyr_layers()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_layers.md):

``` r
layers <- tplyr_layers(
  group_count("SEX", by = "Sex n (%)"),
  group_count("RACE", by = "Race n (%)"),
  group_desc("AGE", by = "Age (Years)")
)
```

Layers are processed in order and stacked vertically in the output.

## Building a Table

Once you have a spec and a dataset, building is a single function call.
Let’s look at a demographics-style table with both count and descriptive
layers:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count("SEX", by = "Sex n (%)"),
    group_desc(
      "AGE",
      by = "Age (Years)",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"    = f_str("xx.x", "median"),
          "Min, Max"  = f_str("xx, xx", "min", "max")
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
| Sex n (%)   | F         | 53 (61.6%)   | 40 (47.6%)   | 50 (59.5%)   |
| Sex n (%)   | M         | 33 (38.4%)   | 44 (52.4%)   | 34 (40.5%)   |
| Age (Years) | n         | 86           | 84           | 84           |
| Age (Years) | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Age (Years) | Median    | 76.0         | 76.0         | 77.5         |
| Age (Years) | Min, Max  | 52, 89       | 56, 88       | 51, 88       |

The output is a standard data frame. Here is what the columns mean:

- **rowlabel1, rowlabel2, …**: Row label columns. A `by` label adds one
  column; a `by` data variable adds another; and the target variable
  adds one more.
- **res1, res2, …**: Result columns, one per unique level of `cols`.
  Each carries a `label` attribute with the column name and, when
  population data is available, an `(N=n)` suffix.
- **ord_layer_index, ord_layer_1, …**: Ordering columns for sorting.
  These can be dropped for display but are useful for programmatic
  reordering.

## String Formatting in tplyr2

One of the most important features of tplyr2 is the format string
system. Clinical tables have specific alignment and precision
requirements. The
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
function lets you declare exactly how numbers should appear.

An [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
has two parts: a template string that defines the layout, and variable
names that map into the template’s format groups. A format group is a
sequence of `x` characters (with optional decimal point) that determines
width and precision.

``` r
# Two decimal places for mean, two for SD, with parentheses
f_str("xx.xx (xx.xx)", "mean", "sd")
#> tplyr format string: "xx.xx (xx.xx)"Variables: mean, sd

# Integer count only
f_str("xxx", "n")
#> tplyr format string: "xxx"Variables: n

# Count with percentage
f_str("xx (xx.x%)", "n", "pct")
#> tplyr format string: "xx (xx.x%)"Variables: n, pct
```

The number of `x` characters in the integer part sets the total width
(including leading spaces for alignment). Characters after the decimal
point set decimal precision. Everything between format groups is literal
text.

For **descriptive statistics layers**, format strings are a named list.
Each name becomes a row label. Built-in statistics available are: `n`,
`mean`, `sd`, `median`, `var`, `min`, `max`, `iqr`, `q1`, `q3`,
`missing`, `pct`, and `total`.

For **count layers**, the format string uses `n`, `pct`, `distinct_n`,
and `distinct_pct`:

``` r
# n (pct%) -- the default
f_str("xx (xx.x%)", "n", "pct")

# Distinct count and percentage
f_str("xx (xx.x%)", "distinct_n", "distinct_pct")
```

Because the format and source variables are declared together, the
package knows exactly which numbers produced each cell – this is the
foundation of the metadata and traceability system.

## Layer Types

### Descriptive Statistics Layers

Descriptive statistics layers summarize continuous variables. Without
custom format strings, the default produces six rows: n, Mean (SD),
Median, Q1/Q3, Min/Max, and Missing.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | res1         | res2         | res3         |
|:----------|:-------------|:-------------|:-------------|
| n         | 86           | 84           | 84           |
| Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Median    | 76.0         | 76.0         | 77.5         |
| Q1, Q3    | 69.2, 81.8   | 70.8, 80.0   | 71.0, 82.0   |
| Min, Max  | 52, 89       | 56, 88       | 51, 88       |
| Missing   | 0            | 0            | 0            |

#### Multiple Target Variables

When you need to summarize more than one variable in the same layer –
for example, Age and Average Daily Dose – pass a vector of names to
`target_var`. Each variable gets its own block of rows, with the
variable name as an additional row label.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc(
      c("AGE", "AVGDD"),
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Min, Max"  = f_str("xx, xx", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | rowlabel2 | res1         | res2         | res3         |
|:----------|:----------|:-------------|:-------------|:-------------|
| AGE       | n         | 86           | 84           | 84           |
| AGE       | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| AGE       | Min, Max  | 52, 89       | 56, 88       | 51, 88       |
| AVGDD     | n         | 86           | 84           | 84           |
| AVGDD     | Mean (SD) | 0.0 ( 0.00)  | 71.6 ( 8.11) | 54.0 ( 0.00) |
| AVGDD     | Min, Max  | 0, 0         | 54, 79       | 54, 54       |

### Count Layers

Count layers tabulate the frequency of categorical variable levels. By
default, they produce counts and percentages formatted as
`"xx (xx.x%)"`:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1                        | res1       | res2       | res3       |
|:---------------------------------|:-----------|:-----------|:-----------|
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)  | 9 (10.7%)  | 6 ( 7.1%)  |
| WHITE                            | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |

#### Total Rows

A total row sums the counts across all levels. Enable it with
`total_row = TRUE` in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md):

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(
      "RACE",
      settings = layer_settings(total_row = TRUE)
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1                        | res1        | res2        | res3        |
|:---------------------------------|:------------|:------------|:------------|
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)   | 1 ( 1.2%)   | 0 ( 0.0%)   |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)   | 9 (10.7%)   | 6 ( 7.1%)   |
| Total                            | 86 (100.0%) | 84 (100.0%) | 84 (100.0%) |
| WHITE                            | 78 (90.7%)  | 74 (88.1%)  | 78 (92.9%)  |

#### Distinct Counting

In adverse event tables, you often need to count the number of
*subjects* who experienced an event, not the number of event records.
Use `distinct_by` to specify the subject identifier:

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(
      "AEBODSYS",
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
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1                                            | res1      | res2       | res3       |
|:-----------------------------------------------------|:----------|:-----------|:-----------|
| CARDIAC DISORDERS                                    | 4 (12.5%) | 6 (14.0%)  | 5 (10.0%)  |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS           | 0 ( 0.0%) | 1 ( 2.3%)  | 0 ( 0.0%)  |
| GASTROINTESTINAL DISORDERS                           | 6 (18.8%) | 4 ( 9.3%)  | 3 ( 6.0%)  |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | 9 (28.1%) | 15 (34.9%) | 18 (36.0%) |
| IMMUNE SYSTEM DISORDERS                              | 0 ( 0.0%) | 0 ( 0.0%)  | 1 ( 2.0%)  |
| INFECTIONS AND INFESTATIONS                          | 5 (15.6%) | 4 ( 9.3%)  | 3 ( 6.0%)  |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS       | 2 ( 6.2%) | 1 ( 2.3%)  | 2 ( 4.0%)  |
| INVESTIGATIONS                                       | 3 ( 9.4%) | 1 ( 2.3%)  | 1 ( 2.0%)  |
| METABOLISM AND NUTRITION DISORDERS                   | 2 ( 6.2%) | 2 ( 4.7%)  | 0 ( 0.0%)  |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS      | 1 ( 3.1%) | 2 ( 4.7%)  | 0 ( 0.0%)  |
| NERVOUS SYSTEM DISORDERS                             | 1 ( 3.1%) | 5 (11.6%)  | 6 (12.0%)  |
| PSYCHIATRIC DISORDERS                                | 3 ( 9.4%) | 2 ( 4.7%)  | 3 ( 6.0%)  |
| RENAL AND URINARY DISORDERS                          | 1 ( 3.1%) | 0 ( 0.0%)  | 0 ( 0.0%)  |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS      | 0 ( 0.0%) | 3 ( 7.0%)  | 1 ( 2.0%)  |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS               | 5 (15.6%) | 18 (41.9%) | 19 (38.0%) |
| VASCULAR DISORDERS                                   | 0 ( 0.0%) | 0 ( 0.0%)  | 1 ( 2.0%)  |

#### Nested Counts

Many clinical tables require hierarchical counts – for example, adverse
events by body system and preferred term. Pass a vector of variable
names to `target_var`:

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(
      c("AEBODSYS", "AEDECOD"),
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
kable(head(result[, !grepl("^ord", names(result))], 15))
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

The outer level (body system) appears in `rowlabel1`, and the inner
level (preferred term) appears indented in `rowlabel2`. Outer rows
contain aggregate counts for the body system; inner rows contain
per-term counts. Indentation defaults to two spaces and can be changed
via the `indentation` parameter in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).

### Shift Layers

Shift tables display a cross-tabulation of a baseline value against a
post-baseline value within each treatment arm. The
[`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md)
function requires a named character vector with `row` and `column`
elements:

``` r
set.seed(12345)
shift_data <- data.frame(
  USUBJID = paste0("SUBJ", 1:30),
  TRTA = rep(c("Placebo", "Active"), each = 15),
  BNRIND = factor(
    sample(c("LOW", "NORMAL", "HIGH"), 30, replace = TRUE, prob = c(0.2, 0.6, 0.2)),
    levels = c("LOW", "NORMAL", "HIGH")
  ),
  ANRIND = factor(
    sample(c("LOW", "NORMAL", "HIGH"), 30, replace = TRUE, prob = c(0.15, 0.5, 0.35)),
    levels = c("LOW", "NORMAL", "HIGH")
  )
)

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_shift(
      c(row = "BNRIND", column = "ANRIND"),
      settings = layer_settings(
        format_strings = list(n_counts = f_str("xxx", "n"))
      )
    )
  )
)

result <- tplyr_build(spec, shift_data)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | res1 | res2 | res3 | res4 | res5 | res6 |
|:----------|:-----|:-----|:-----|:-----|:-----|:-----|
| LOW       | 1    | 0    | 1    | 1    | 0    | 2    |
| NORMAL    | 1    | 1    | 8    | 2    | 2    | 4    |
| HIGH      | 2    | 0    | 1    | 2    | 1    | 1    |

In the output, `rowlabel1` contains the baseline (row) variable values,
and the result columns represent each combination of treatment arm and
post-baseline (column) level. Factor ordering is respected, so setting
levels appropriately (e.g., `c("LOW", "NORMAL", "HIGH")`) ensures the
intended display order.

## Numeric Data

Formatted cells look good in a final report, but sometimes you need the
raw numbers – for validation, further computation, or archiving. Every
tplyr2 build retains unformatted numeric data as an attribute. Use
[`tplyr_numeric_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_numeric_data.md)
to retrieve it:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX"),
    group_desc(
      "AGE",
      settings = layer_settings(
        format_strings = list(
          "n" = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)

# Raw numeric data for the count layer (layer 1)
kable(tplyr_numeric_data(result, layer = 1))
```

| TRT01P               | SEX |   n |      pct | total |
|:---------------------|:----|----:|---------:|------:|
| Placebo              | F   |  53 | 61.62791 |    86 |
| Placebo              | M   |  33 | 38.37209 |    86 |
| Xanomeline High Dose | F   |  40 | 47.61905 |    84 |
| Xanomeline High Dose | M   |  44 | 52.38095 |    84 |
| Xanomeline Low Dose  | F   |  50 | 59.52381 |    84 |
| Xanomeline Low Dose  | M   |  34 | 40.47619 |    84 |

``` r
# Raw numeric data for the desc layer (layer 2)
kable(tplyr_numeric_data(result, layer = 2))
```

| TRT01P               |   n |     mean |       sd | median |      var | min | max |   iqr |    q1 |    q3 | missing | total | pct |
|:---------------------|----:|---------:|---------:|-------:|---------:|----:|----:|------:|------:|------:|--------:|------:|----:|
| Placebo              |  86 | 75.20930 | 8.590167 |   76.0 | 73.79097 |  52 |  89 | 12.50 | 69.25 | 81.75 |       0 |    86 | 100 |
| Xanomeline High Dose |  84 | 74.38095 | 7.886094 |   76.0 | 62.19048 |  56 |  88 |  9.25 | 70.75 | 80.00 |       0 |    84 | 100 |
| Xanomeline Low Dose  |  84 | 75.66667 | 8.286051 |   77.5 | 68.65863 |  51 |  88 | 11.00 | 71.00 | 82.00 |       0 |    84 | 100 |

This gives you the exact counts, percentages, means, and every other
computed statistic in raw numeric form, keyed by the same grouping
variables used during the build.

## Where to Go from Here

This vignette covers the fundamentals. Here are the topics covered in
other vignettes:

- [`vignette("count")`](https://github.com/mstackhouse/tplyr2/articles/count.md)
  – Denominator control, missing value handling, risk differences,
  ordering.
- [`vignette("desc")`](https://github.com/mstackhouse/tplyr2/articles/desc.md)
  – Custom summaries, auto-precision, stats-as-columns.
- [`vignette("shift")`](https://github.com/mstackhouse/tplyr2/articles/shift.md)
  – Shift layers and cross-tabulation options.
- [`vignette("denom")`](https://github.com/mstackhouse/tplyr2/articles/denom.md)
  – Population data, header N, total groups, custom groups.
- [`vignette("post_processing")`](https://github.com/mstackhouse/tplyr2/articles/post_processing.md)
  – Row masks, row label collapsing, conditional formatting, and text
  wrapping.
- [`vignette("metadata")`](https://github.com/mstackhouse/tplyr2/articles/metadata.md)
  – Cell-level metadata and traceability.
- [`vignette("serialization")`](https://github.com/mstackhouse/tplyr2/articles/serialization.md)
  – Saving and loading specs as JSON or YAML.
- [`vignette("analyze")`](https://github.com/mstackhouse/tplyr2/articles/analyze.md)
  – Custom analysis layers with user-defined functions.

## References

- [PHUSE Analyses & Displays White Paper – Demographics, Disposition,
  and
  Medications](https://phuse.s3.eu-central-1.amazonaws.com/Deliverables/Standard+Analyses+and+Code+Sharing/Analyses+%26+Displays+Associated+with+Demographics%2C+Disposition%2C+and+Medications+in+Phase+2-4+Clinical+Trials+and+Integrated+Summary+Documents.pdf)
- [CDISC Analysis Data Model
  (ADaM)](https://www.cdisc.org/standards/foundational/adam)
