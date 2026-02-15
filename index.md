# tplyr2

**{tplyr2}** is a grammar of clinical summary tables. Clinical reports –
demographics tables, adverse event summaries, lab shift tables – all
share a common structural pattern. Each section is some kind of summary:
a set of counts, a block of descriptive statistics, or a
cross-tabulation. Rather than writing bespoke data manipulation code for
every table, **{tplyr2}** lets you describe *what* the table should
contain using a declarative specification, and handles the computing,
formatting, and assembly for you.

**{tplyr2}** is a ground-up rewrite of
[**{Tplyr}**](https://github.com/atorus-research/Tplyr), built on
[data.table](https://rdatatable.gitlab.io/data.table/) for performance.
The spec-based API separates configuration from data, making specs
portable, reusable, and serializable to JSON/YAML.

## Installation

You can install the development version of **{tplyr2}** from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("atorus-research/tplyr2")
```

## Example

Every table starts with a
[`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md)
that declares the column structure and layers. Data is supplied at build
time with
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md).

``` r
library(tplyr2)

spec <- tplyr_spec(
  cols = "TRT01P",
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
knitr::kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1   | rowlabel2 | res1         | res2         | res3         |
|:------------|:----------|:-------------|:-------------|:-------------|
| Sex n (%)   | F         | 53 (61.6%)   | 40 (47.6%)   | 50 (59.5%)   |
| Sex n (%)   | M         | 33 (38.4%)   | 44 (52.4%)   | 34 (40.5%)   |
| Age (Years) | n         | 86           | 84           | 84           |
| Age (Years) | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Age (Years) | Median    | 76.0         | 76.0         | 77.5         |
| Age (Years) | Min, Max  | 52, 89       | 56, 88       | 51, 88       |

## Key Concepts

- **Spec**: A
  [`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md)
  object is pure configuration – no data, no side effects. It describes
  the column variable, filters, population data, and layers.
- **Layers**: Each layer is a summary block created with
  [`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md),
  [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md),
  [`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md),
  or
  [`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md).
- **Build**: `tplyr_build(spec, data)` executes the spec against a
  dataset and returns a formatted data frame.
- **Format strings**:
  [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
  declarations control numeric precision and alignment (e.g.,
  `f_str("xx.x (xx.xx)", "mean", "sd")`).

## Layer Types

### Count Layers

Tabulate frequencies of categorical variables, with support for nested
counts, distinct subject counting, and total rows.

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
knitr::kable(head(result[, !grepl("^ord", names(result))], 10))
```

| rowlabel1         | rowlabel2                      | res1      | res2      | res3      |
|:------------------|:-------------------------------|:----------|:----------|:----------|
| CARDIAC DISORDERS |                                | 4 (12.5%) | 6 (14.0%) | 5 (10.0%) |
| CARDIAC DISORDERS | ATRIAL FIBRILLATION            | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) |
| CARDIAC DISORDERS | ATRIAL FLUTTER                 | 0 ( 0.0%) | 1 ( 2.3%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | ATRIAL HYPERTROPHY             | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | BUNDLE BRANCH BLOCK RIGHT      | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | CARDIAC FAILURE CONGESTIVE     | 1 ( 3.1%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | MYOCARDIAL INFARCTION          | 0 ( 0.0%) | 1 ( 2.3%) | 2 ( 4.0%) |
| CARDIAC DISORDERS | SINUS BRADYCARDIA              | 0 ( 0.0%) | 3 ( 7.0%) | 1 ( 2.0%) |
| CARDIAC DISORDERS | SUPRAVENTRICULAR EXTRASYSTOLES | 1 ( 3.1%) | 0 ( 0.0%) | 1 ( 2.0%) |
| CARDIAC DISORDERS | SUPRAVENTRICULAR TACHYCARDIA   | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 2.0%) |

### Descriptive Statistics Layers

Summarize continuous variables with built-in statistics (`n`, `mean`,
`sd`, `median`, `min`, `max`, `q1`, `q3`, `var`, `iqr`, `missing`) or
custom summary functions.

### Shift Layers

Cross-tabulate a baseline value against a post-baseline value within
each treatment arm using
[`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md).

### Analyze Layers

Run user-defined analysis functions with
[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
for full flexibility.

## Features

- **Population data**: Control denominators with
  [`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md)
  and display `(N=n)` header counts with
  [`tplyr_header_n()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_header_n.md)
- **Total & custom groups**: Add “Total” columns or combine treatment
  arms with
  [`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md)
  and
  [`custom_group()`](https://github.com/mstackhouse/tplyr2/reference/custom_group.md)
- **Auto-precision**: Dynamically set decimal places based on collected
  precision with the precision cap system
- **Risk difference**: Compute risk differences and confidence intervals
  on count layers
- **Post-processing**: Row masks, row label collapsing, conditional
  formatting, and text wrapping helpers
- **Metadata**: Cell-level traceability to trace any result back to its
  source records
- **Numeric data**: Access raw unformatted results via
  [`tplyr_numeric_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_numeric_data.md)
- **Serialization**: Save and load specs as JSON or YAML with
  [`tplyr_write_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_write_spec.md)
  /
  [`tplyr_read_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_read_spec.md)
- **ARD**: Convert to and from Analysis Results Data format with
  [`tplyr_to_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_to_ard.md)
  /
  [`tplyr_from_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_from_ard.md)

## Learning More

- [`vignette("tplyr2")`](https://github.com/mstackhouse/tplyr2/articles/tplyr2.md)
  – Getting started
- [`vignette("count")`](https://github.com/mstackhouse/tplyr2/articles/count.md)
  – Count layers in depth
- [`vignette("desc")`](https://github.com/mstackhouse/tplyr2/articles/desc.md)
  – Descriptive statistics layers
- [`vignette("shift")`](https://github.com/mstackhouse/tplyr2/articles/shift.md)
  – Shift layers
- [`vignette("denom")`](https://github.com/mstackhouse/tplyr2/articles/denom.md)
  – Population data, header N, total and custom groups
- [`vignette("general_string_formatting")`](https://github.com/mstackhouse/tplyr2/articles/general_string_formatting.md)
  – Format string system
- [`vignette("metadata")`](https://github.com/mstackhouse/tplyr2/articles/metadata.md)
  – Cell-level metadata and traceability
- [`vignette("serialization")`](https://github.com/mstackhouse/tplyr2/articles/serialization.md)
  – Saving and loading specs
- [`vignette("analyze")`](https://github.com/mstackhouse/tplyr2/articles/analyze.md)
  – Custom analyze layers
- [`vignette("post_processing")`](https://github.com/mstackhouse/tplyr2/articles/post_processing.md)
  – Post-processing helpers
