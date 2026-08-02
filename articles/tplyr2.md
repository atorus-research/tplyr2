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
  [`tplyr_spec()`](https://atorus-research.github.io/tplyr2/reference/tplyr_spec.md)
  object is pure configuration. It describes the column structure,
  global filters, and a list of layers. No data processing happens when
  you create a spec.
- **Layers**: Each layer is one summary block. Count layers
  ([`group_count()`](https://atorus-research.github.io/tplyr2/reference/group_count.md))
  produce frequencies. Descriptive layers
  ([`group_desc()`](https://atorus-research.github.io/tplyr2/reference/group_desc.md))
  compute means, medians, and other summaries. Shift layers
  ([`group_shift()`](https://atorus-research.github.io/tplyr2/reference/group_shift.md))
  create cross-tabulations. Custom layers
  ([`group_analyze()`](https://atorus-research.github.io/tplyr2/reference/group_analyze.md))
  accept user-defined functions.
- **Build**: `tplyr_build(spec, data)` executes the spec against a
  dataset and returns a formatted data frame.

This separation of configuration from execution is intentional. A spec
can be saved to disk, reviewed, reused across studies, or applied to
different datasets at build time. If you are familiar with the PHUSE
white paper *Analyses & Displays Associated with Demographics,
Disposition, and Medications in Phase 2-4 Clinical Trials and Integrated
Summary Documents*, the layer model maps naturally to the summary blocks
described there.

## The tplyr_spec() Object

Every table starts with a
[`tplyr_spec()`](https://atorus-research.github.io/tplyr2/reference/tplyr_spec.md).
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
  [`pop_data()`](https://atorus-research.github.io/tplyr2/reference/pop_data.md)
  configuration for population-based denominators.
- `total_groups`: A list of
  [`total_group()`](https://atorus-research.github.io/tplyr2/reference/total_group.md)
  objects for “Total” columns.
- `custom_groups`: A list of
  [`custom_group()`](https://atorus-research.github.io/tplyr2/reference/custom_group.md)
  objects for combined treatment arms.
- `layers`: Layer objects wrapped in
  [`tplyr_layers()`](https://atorus-research.github.io/tplyr2/reference/tplyr_layers.md).

The spec contains no data. Data is supplied at build time, so the same
spec can be reused across datasets.

## The tplyr_layer() Object

Layers are the building blocks of a tplyr2 table. You create them with
the `group_*()` family:
[`group_count()`](https://atorus-research.github.io/tplyr2/reference/group_count.md),
[`group_desc()`](https://atorus-research.github.io/tplyr2/reference/group_desc.md),
[`group_shift()`](https://atorus-research.github.io/tplyr2/reference/group_shift.md),
and
[`group_analyze()`](https://atorus-research.github.io/tplyr2/reference/group_analyze.md).
Every layer constructor accepts:

- `target_var`: The variable(s) being summarized.
- `by`: Optional row grouping. Strings matching column names become
  grouping variables; other strings become text labels in the output.
  Use
  [`label()`](https://atorus-research.github.io/tplyr2/reference/label.md)
  for explicit disambiguation.
- `where`: An optional layer-level filter expression.
- `settings`: A
  [`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md)
  object for detailed configuration.

Multiple layers are collected with
[`tplyr_layers()`](https://atorus-research.github.io/tplyr2/reference/tplyr_layers.md):

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
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
function lets you declare exactly how numbers should appear.

An
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
has two parts: a template string that defines the layout, and variable
names that map into the template’s format groups. A format group is a
sequence of `x` characters (with optional decimal point) that determines
width and precision.

``` r

# Two decimal places for mean, two for SD, with parentheses
f_str("xx.xx (xx.xx)", "mean", "sd")
#> tplyr format string: "xx.xx (xx.xx)"
#>   Variables: mean, sd

# Integer count only
f_str("xxx", "n")
#> tplyr format string: "xxx"
#>   Variables: n

# Count with percentage
f_str("xx (xx.x%)", "n", "pct")
#> tplyr format string: "xx (xx.x%)"
#>   Variables: n, pct
```

The number of `x` characters in the integer part sets the total width
(including leading spaces for alignment). Characters after the decimal
point set decimal precision. Everything between format groups is literal
text.

For **descriptive statistics layers**, format strings are a named list
and each name becomes a row label. For **count and shift layers**, a
single format string goes under the reserved key `n_counts`:

``` r

# count layer: n (pct%) -- the default
list(n_counts = f_str("xx (xx.x%)", "n", "pct"))

# count layer: distinct subjects rather than records
list(n_counts = f_str("xx (xx.x%)", "distinct_n", "distinct_pct"))
```

Each layer type computes its own set of statistics, and
[`vignette("format_strings")`](https://atorus-research.github.io/tplyr2/articles/format_strings.md)
carries the complete reference. Two more characters extend the template:
`a` lets the *data* determine the decimal width, and uppercase `X`
closes the gap between a number and its delimiter – both covered in
[`vignette("precision_alignment")`](https://atorus-research.github.io/tplyr2/articles/precision_alignment.md).

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
| WHITE                            | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)  | 9 (10.7%)  | 6 ( 7.1%)  |
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |

#### Total Rows

A total row sums the counts across all levels. Enable it with
`total_row = TRUE` in
[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md):

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
| WHITE                            | 78 (90.7%)  | 74 (88.1%)  | 78 (92.9%)  |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)   | 9 (10.7%)   | 6 ( 7.1%)   |
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)   | 1 ( 1.2%)   | 0 ( 0.0%)   |
| Total                            | 86 (100.0%) | 84 (100.0%) | 84 (100.0%) |

#### Distinct Counting

In adverse event tables, you often need to count the number of
*subjects* who experienced an event, not the number of event records.
Use `distinct_by` to specify the subject identifier. Because `ADAE`
contains only subjects with events, the denominator must come from the
full population (`ADSL`), supplied through
[`pop_data()`](https://atorus-research.github.io/tplyr2/reference/pop_data.md)
– otherwise the percentages are computed against subjects-with-events
and come out too large (see
[`vignette("count")`](https://atorus-research.github.io/tplyr2/articles/count.md)
and
[`vignette("denom")`](https://atorus-research.github.io/tplyr2/articles/denom.md)):

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
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

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | res1 | res2 | res3 |
|:---|:---|:---|:---|
| CARDIAC DISORDERS | 4 ( 4.7%) | 6 ( 7.1%) | 5 ( 6.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| GASTROINTESTINAL DISORDERS | 6 ( 7.0%) | 4 ( 4.8%) | 3 ( 3.6%) |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | 9 (10.5%) | 15 (17.9%) | 18 (21.4%) |
| IMMUNE SYSTEM DISORDERS | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| INFECTIONS AND INFESTATIONS | 5 ( 5.8%) | 4 ( 4.8%) | 3 ( 3.6%) |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS | 2 ( 2.3%) | 1 ( 1.2%) | 2 ( 2.4%) |
| INVESTIGATIONS | 3 ( 3.5%) | 1 ( 1.2%) | 1 ( 1.2%) |
| METABOLISM AND NUTRITION DISORDERS | 2 ( 2.3%) | 2 ( 2.4%) | 0 ( 0.0%) |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS | 1 ( 1.2%) | 2 ( 2.4%) | 0 ( 0.0%) |
| NERVOUS SYSTEM DISORDERS | 1 ( 1.2%) | 5 ( 6.0%) | 6 ( 7.1%) |
| PSYCHIATRIC DISORDERS | 3 ( 3.5%) | 2 ( 2.4%) | 3 ( 3.6%) |
| RENAL AND URINARY DISORDERS | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS | 0 ( 0.0%) | 3 ( 3.6%) | 1 ( 1.2%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | 5 ( 5.8%) | 18 (21.4%) | 19 (22.6%) |
| VASCULAR DISORDERS | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |

#### Nested Counts

Many clinical tables require hierarchical counts – for example, adverse
events by body system and preferred term. Pass a vector of variable
names to `target_var`:

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
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

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, !grepl("^ord", names(result))], 15))
```

| rowlabel1 | rowlabel2 | res1 | res2 | res3 |
|:---|:---|:---|:---|:---|
| CARDIAC DISORDERS |  | 4 ( 4.7%) | 6 ( 7.1%) | 5 ( 6.0%) |
| CARDIAC DISORDERS | ATRIAL FIBRILLATION | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| CARDIAC DISORDERS | ATRIAL FLUTTER | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | ATRIAL HYPERTROPHY | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | BUNDLE BRANCH BLOCK RIGHT | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | CARDIAC FAILURE CONGESTIVE | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | MYOCARDIAL INFARCTION | 0 ( 0.0%) | 1 ( 1.2%) | 2 ( 2.4%) |
| CARDIAC DISORDERS | SINUS BRADYCARDIA | 0 ( 0.0%) | 3 ( 3.6%) | 1 ( 1.2%) |
| CARDIAC DISORDERS | SUPRAVENTRICULAR EXTRASYSTOLES | 1 ( 1.2%) | 0 ( 0.0%) | 1 ( 1.2%) |
| CARDIAC DISORDERS | SUPRAVENTRICULAR TACHYCARDIA | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| CARDIAC DISORDERS | TACHYCARDIA | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | VENTRICULAR EXTRASYSTOLES | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS |  | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS | VENTRICULAR SEPTAL DEFECT | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| GASTROINTESTINAL DISORDERS |  | 6 ( 7.0%) | 4 ( 4.8%) | 3 ( 3.6%) |

The outer level (body system) appears in `rowlabel1`, and the inner
level (preferred term) appears in `rowlabel2`. Outer rows contain
aggregate counts for the body system; inner rows contain per-term
counts. Use `collapse_row_labels(result, nest = TRUE)` to merge these
into a single indented column (see the Count Layer vignette for
details).

### Shift Layers

Shift tables display a cross-tabulation of a baseline value against a
post-baseline value within each treatment arm. The
[`group_shift()`](https://atorus-research.github.io/tplyr2/reference/group_shift.md)
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
| LOW       | 0    | 1    | 1    | 0    | 2    | 1    |
| NORMAL    | 1    | 8    | 1    | 2    | 4    | 2    |
| HIGH      | 0    | 1    | 2    | 1    | 1    | 2    |

In the output, `rowlabel1` contains the baseline (row) variable values,
and the result columns represent each combination of treatment arm and
post-baseline (column) level. Factor ordering is respected, so setting
levels appropriately (e.g., `c("LOW", "NORMAL", "HIGH")`) ensures the
intended display order.

## Numeric Data

Formatted cells look good in a final report, but sometimes you need the
raw numbers – for validation, further computation, or archiving. Every
tplyr2 build retains unformatted numeric data as an attribute. Use
[`tplyr_numeric_data()`](https://atorus-research.github.io/tplyr2/reference/tplyr_numeric_data.md)
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

| TRT01P | n | n_records | mean | sd | median | var | min | max | iqr | q1 | q3 | missing | total | pct |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Placebo | 86 | 86 | 75.20930 | 8.590167 | 76.0 | 73.79097 | 52 | 89 | 12.50 | 69.25 | 81.75 | 0 | 86 | 100 |
| Xanomeline High Dose | 84 | 84 | 74.38095 | 7.886094 | 76.0 | 62.19048 | 56 | 88 | 9.25 | 70.75 | 80.00 | 0 | 84 | 100 |
| Xanomeline Low Dose | 84 | 84 | 75.66667 | 8.286051 | 77.5 | 68.65863 | 51 | 88 | 11.00 | 71.00 | 82.00 | 0 | 84 | 100 |

This gives you the exact counts, percentages, means, and every other
computed statistic in raw numeric form, keyed by the same grouping
variables used during the build.

## Where to Go from Here

This vignette covers the fundamentals. Here are the topics covered in
other vignettes:

- [`vignette("table")`](https://atorus-research.github.io/tplyr2/articles/table.md)
  – Spec-level structure: column variables, table filters, total/custom
  groups, and population data.
- [`vignette("count")`](https://atorus-research.github.io/tplyr2/articles/count.md)
  – Count layers in depth: population-based denominators, distinct
  counts, nested summaries, stat columns, missing values.
- [`vignette("desc")`](https://atorus-research.github.io/tplyr2/articles/desc.md)
  – Descriptive layers: custom summaries, auto-precision,
  stats-as-columns.
- [`vignette("shift")`](https://atorus-research.github.io/tplyr2/articles/shift.md)
  – Shift layers, shift denominators, and the denominator row.
- [`vignette("denom")`](https://atorus-research.github.io/tplyr2/articles/denom.md)
  – Denominator control: population data, header N, `denoms_by`,
  `denom_where`, single-proportion confidence intervals.
- [`vignette("adverse-events")`](https://atorus-research.github.io/tplyr2/articles/adverse-events.md)
  – A full adverse event table built end to end.
- [`vignette("riskdiff")`](https://atorus-research.github.io/tplyr2/articles/riskdiff.md)
  and
  [`vignette("binding-statistics")`](https://atorus-research.github.io/tplyr2/articles/binding-statistics.md)
  – Risk differences and association-test p-value columns.
- [`vignette("sort")`](https://atorus-research.github.io/tplyr2/articles/sort.md)
  – Row ordering by frequency, factor levels, or a VARN companion.
- [`vignette("format_strings")`](https://atorus-research.github.io/tplyr2/articles/format_strings.md)
  – The
  [`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
  format-string system: the grammar, the complete statistic keyword
  reference per layer type, rounding, and missing-value handling.
- [`vignette("precision_alignment")`](https://atorus-research.github.io/tplyr2/articles/precision_alignment.md)
  – Data-driven decimal precision (`a`/`A`) and parenthesis hugging
  (`X`/`A`).
- [`vignette("display_conventions")`](https://atorus-research.github.io/tplyr2/articles/display_conventions.md)
  – `<1%` and `>99%` percents, zero-count suppression, statistics as
  columns, missing rows.
- [`vignette("post_processing")`](https://atorus-research.github.io/tplyr2/articles/post_processing.md)
  – Row masks, row label collapsing, conditional formatting,
  [`as_display()`](https://atorus-research.github.io/tplyr2/reference/as_display.md),
  and text wrapping.
- [`vignette("metadata")`](https://atorus-research.github.io/tplyr2/articles/metadata.md)
  – Cell-level metadata and traceability.
- [`vignette("serialization")`](https://atorus-research.github.io/tplyr2/articles/serialization.md),
  [`vignette("ard")`](https://atorus-research.github.io/tplyr2/articles/ard.md)
  – Saving/loading specs and Analysis Results Data conversion.
- [`vignette("analyze")`](https://atorus-research.github.io/tplyr2/articles/analyze.md)
  – Custom analysis layers with user-defined functions.
- [`vignette("options")`](https://atorus-research.github.io/tplyr2/articles/options.md)
  – Package options (rounding, quantiles, precision).
- [`vignette("migration")`](https://atorus-research.github.io/tplyr2/articles/migration.md)
  – Moving from Tplyr v1 to tplyr2.

## References

- PHUSE, *Analyses & Displays Associated with Demographics, Disposition,
  and Medications in Phase 2-4 Clinical Trials and Integrated Summary
  Documents*. Available from the PHUSE deliverables catalogue.
- [CDISC Analysis Data Model
  (ADaM)](https://www.cdisc.org/standards/foundational/adam)
