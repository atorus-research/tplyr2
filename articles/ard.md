# Analysis Results Data

## Introduction

Clinical summary tables are typically built for human consumption:
aligned columns, formatted numbers, parenthesized percentages. But
increasingly, regulatory and industry workflows require the underlying
results in a machine-readable, standards-compliant format. The Analysis
Results Data (ARD) model – part of the broader CDISC ecosystem –
addresses this by defining a long-format structure where each row
represents a single statistic for a single group combination.

tplyr2 supports this through two functions:

- [`tplyr_to_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_to_ard.md)
  converts a built result into ARD long format.
- [`tplyr_from_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_from_ard.md)
  reconstructs a formatted table from ARD data and a spec.

Together, these functions let you separate the *computed values* from
their *presentation*, enabling workflows where results are archived,
exchanged, or validated independently of formatting.

## Converting to ARD

Any result from
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
can be converted to ARD format. The raw numeric data is already attached
to every build result as an attribute;
[`tplyr_to_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_to_ard.md)
melts that data into one-row-per-statistic long format.

Here is a complete example using a demographics table with both count
and descriptive statistics layers:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX"),
    group_desc(
      "AGE",
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
```

The formatted output looks like a typical clinical table:

``` r
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1 | res1         | res2         | res3         |
|:----------|:-------------|:-------------|:-------------|
| F         | 53 (61.6%)   | 40 (47.6%)   | 50 (59.5%)   |
| M         | 33 (38.4%)   | 44 (52.4%)   | 34 (40.5%)   |
| n         | 86           | 84           | 84           |
| Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Median    | 76.0         | 76.0         | 77.5         |
| Min, Max  | 52, 89       | 56, 88       | 51, 88       |

Converting to ARD is a single function call:

``` r
ard <- tplyr_to_ard(result)
kable(head(ard, 15))
```

| analysis_id | TRT01P               | SEX | stat_name | stat_value |
|------------:|:---------------------|:----|:----------|-----------:|
|           1 | Placebo              | F   | n         |   53.00000 |
|           1 | Placebo              | M   | n         |   33.00000 |
|           1 | Xanomeline High Dose | F   | n         |   40.00000 |
|           1 | Xanomeline High Dose | M   | n         |   44.00000 |
|           1 | Xanomeline Low Dose  | F   | n         |   50.00000 |
|           1 | Xanomeline Low Dose  | M   | n         |   34.00000 |
|           1 | Placebo              | F   | pct       |   61.62791 |
|           1 | Placebo              | M   | pct       |   38.37209 |
|           1 | Xanomeline High Dose | F   | pct       |   47.61905 |
|           1 | Xanomeline High Dose | M   | pct       |   52.38095 |
|           1 | Xanomeline Low Dose  | F   | pct       |   59.52381 |
|           1 | Xanomeline Low Dose  | M   | pct       |   40.47619 |
|           1 | Placebo              | F   | total     |   86.00000 |
|           1 | Placebo              | M   | total     |   86.00000 |
|           1 | Xanomeline High Dose | F   | total     |   84.00000 |

## ARD Structure

The ARD output is a data frame in long format. Each row represents one
statistic for one group combination. The columns are:

- **analysis_id**: An integer identifying which layer produced the row.
  Layer 1 is the first layer in the spec, layer 2 is the second, and so
  on.
- **Grouping columns**: The original data variables that define the
  groups. For a count layer with `cols = "TRT01P"` and
  `target_var = "SEX"`, you will see `TRT01P` and `SEX` columns.
- **stat_name**: The name of the statistic – for example, `"n"`,
  `"pct"`, `"total"`, `"mean"`, `"sd"`, `"median"`, `"min"`, `"max"`.
- **stat_value**: The numeric value of the statistic.

Let us examine the count layer (analysis_id 1) and descriptive
statistics layer (analysis_id 2) separately.

### Count Layer ARD

The count layer produces statistics like `n`, `pct`, and `total` for
each combination of treatment arm and target variable level:

``` r
count_ard <- ard[ard$analysis_id == 1, ]
kable(count_ard)
```

| analysis_id | TRT01P               | SEX | stat_name | stat_value |
|------------:|:---------------------|:----|:----------|-----------:|
|           1 | Placebo              | F   | n         |   53.00000 |
|           1 | Placebo              | M   | n         |   33.00000 |
|           1 | Xanomeline High Dose | F   | n         |   40.00000 |
|           1 | Xanomeline High Dose | M   | n         |   44.00000 |
|           1 | Xanomeline Low Dose  | F   | n         |   50.00000 |
|           1 | Xanomeline Low Dose  | M   | n         |   34.00000 |
|           1 | Placebo              | F   | pct       |   61.62791 |
|           1 | Placebo              | M   | pct       |   38.37209 |
|           1 | Xanomeline High Dose | F   | pct       |   47.61905 |
|           1 | Xanomeline High Dose | M   | pct       |   52.38095 |
|           1 | Xanomeline Low Dose  | F   | pct       |   59.52381 |
|           1 | Xanomeline Low Dose  | M   | pct       |   40.47619 |
|           1 | Placebo              | F   | total     |   86.00000 |
|           1 | Placebo              | M   | total     |   86.00000 |
|           1 | Xanomeline High Dose | F   | total     |   84.00000 |
|           1 | Xanomeline High Dose | M   | total     |   84.00000 |
|           1 | Xanomeline Low Dose  | F   | total     |   84.00000 |
|           1 | Xanomeline Low Dose  | M   | total     |   84.00000 |

Each sex-by-treatment combination has three rows: the raw count (`n`),
the percentage (`pct`), and the denominator (`total`).

### Descriptive Statistics Layer ARD

The descriptive layer produces a richer set of statistics. Even if the
format strings only reference `n`, `mean`, `sd`, `median`, `min`, and
`max`, the numeric data snapshot captures all computed statistics:

``` r
desc_ard <- ard[ard$analysis_id == 2, ]
kable(head(desc_ard, 20))
```

|     | analysis_id | TRT01P               | SEX | stat_name | stat_value |
|:----|------------:|:---------------------|:----|:----------|-----------:|
| 19  |           2 | Placebo              | NA  | n         |  86.000000 |
| 20  |           2 | Xanomeline High Dose | NA  | n         |  84.000000 |
| 21  |           2 | Xanomeline Low Dose  | NA  | n         |  84.000000 |
| 22  |           2 | Placebo              | NA  | mean      |  75.209302 |
| 23  |           2 | Xanomeline High Dose | NA  | mean      |  74.380952 |
| 24  |           2 | Xanomeline Low Dose  | NA  | mean      |  75.666667 |
| 25  |           2 | Placebo              | NA  | sd        |   8.590167 |
| 26  |           2 | Xanomeline High Dose | NA  | sd        |   7.886094 |
| 27  |           2 | Xanomeline Low Dose  | NA  | sd        |   8.286051 |
| 28  |           2 | Placebo              | NA  | median    |  76.000000 |
| 29  |           2 | Xanomeline High Dose | NA  | median    |  76.000000 |
| 30  |           2 | Xanomeline Low Dose  | NA  | median    |  77.500000 |
| 31  |           2 | Placebo              | NA  | var       |  73.790971 |
| 32  |           2 | Xanomeline High Dose | NA  | var       |  62.190476 |
| 33  |           2 | Xanomeline Low Dose  | NA  | var       |  68.658635 |
| 34  |           2 | Placebo              | NA  | min       |  52.000000 |
| 35  |           2 | Xanomeline High Dose | NA  | min       |  56.000000 |
| 36  |           2 | Xanomeline Low Dose  | NA  | min       |  51.000000 |
| 37  |           2 | Placebo              | NA  | max       |  89.000000 |
| 38  |           2 | Xanomeline High Dose | NA  | max       |  88.000000 |

This is one of the key advantages of the ARD format: it preserves every
computed value, not just those that appear in the formatted output.

## Reconstructing from ARD

Given an ARD data frame and the original spec,
[`tplyr_from_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_from_ard.md)
applies the spec’s formatting rules to rebuild the display table. This
completes the round-trip:

``` r
rebuilt <- tplyr_from_ard(ard, spec)
kable(rebuilt[, !grepl("^ord", names(rebuilt))])
```

| rowlabel1 | res1         | res2         | res3         |
|:----------|:-------------|:-------------|:-------------|
| F         | 53 (61.6%)   | 40 (47.6%)   | 50 (59.5%)   |
| M         | 33 (38.4%)   | 44 (52.4%)   | 34 (40.5%)   |
| Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Median    | 76.0         | 76.0         | 77.5         |
| Min, Max  | 52, 89       | 56, 88       | 51, 88       |
| n         | 86           | 84           | 84           |

The reconstructed table applies the same format strings, row labels, and
column structure defined in the spec. This means the spec acts as a
reusable formatting template: the same ARD data can be reformatted with
different specs if needed, or the same spec can be applied to ARD data
from different studies.

### Verifying the Round-Trip

We can confirm that the formatted values match between the original
build and the ARD reconstruction:

``` r
original_sorted <- result[order(result$rowlabel1), ]
rebuilt_sorted  <- rebuilt[order(rebuilt$rowlabel1), ]

all(trimws(original_sorted$res1) == trimws(rebuilt_sorted$res1))
#> [1] TRUE
```

The formatted cell values are identical, confirming that no information
is lost in the conversion.

## Use Cases

### Standards-Compliant Data Exchange

When submitting results to a regulatory agency or sharing across
organizations, the ARD format provides a self-describing, tool-agnostic
representation of the computed values. The long format is
straightforward to validate, compare across submissions, or load into
any analysis environment.

### Separating Computation from Presentation

In a production workflow, you might compute results once, archive the
ARD, and then apply formatting later – or apply different formatting for
different audiences. The spec defines the presentation; the ARD holds
the numbers.

``` r
# Same ARD, different formatting
compact_spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(
      "SEX",
      settings = layer_settings(
        format_strings = list(n_counts = f_str("xx", "n"))
      )
    ),
    group_desc(
      "AGE",
      settings = layer_settings(
        format_strings = list(
          "n"    = f_str("xx", "n"),
          "Mean" = f_str("xx.x", "mean")
        )
      )
    )
  )
)

compact_result <- tplyr_from_ard(ard, compact_spec)
kable(compact_result[, !grepl("^ord", names(compact_result))])
```

| rowlabel1 | res1 | res2 | res3 |
|:----------|:-----|:-----|:-----|
| F         | 53   | 40   | 50   |
| M         | 33   | 44   | 34   |
| Mean      | 75.2 | 74.4 | 75.7 |
| n         | 86   | 84   | 84   |

The same underlying data now appears in a more compact layout, with
counts displayed without percentages and only two descriptive statistics
rows.

### Archiving and Reproducibility

Because the ARD captures every computed statistic as a plain numeric
value, it serves as a durable archive of the analysis results. Paired
with a saved spec (see `vignette("serialize")`), the full table can be
reproduced at any time without re-running the analysis against the
source data.

## Summary

The ARD workflow in tplyr2 is straightforward:

1.  Build your table with
    [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
    as usual.
2.  Convert to long format with
    [`tplyr_to_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_to_ard.md)
    to get one row per statistic.
3.  Reconstruct a formatted table with
    [`tplyr_from_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_from_ard.md)
    using any compatible spec.

This separation of computed results from formatted output supports
standards-compliant data exchange, flexible re-formatting, and
reproducible archiving of analysis results.
