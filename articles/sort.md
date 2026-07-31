# Sorting a tplyr2 Table

## Introduction

When you build a table with
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md),
the output includes a set of ordering columns alongside the formatted
results. These columns – `ord_layer_index`, `ord_layer_1`,
`ord_layer_2`, and so on – carry the sorting information that tplyr2
computed during the build. They exist so that you can re-sort,
interleave, or rearrange your output after the fact without losing track
of the intended presentation order.

This vignette explains what the ordering columns mean, how tplyr2
decides their values, and how you can control them to get the row order
you need.

## Ordering Columns Explained

Every output from
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)
includes at least two ordering columns:

- **`ord_layer_index`**: An integer identifying which layer produced
  each row. The first layer in your spec gets index 1, the second gets
  index 2, and so on.
- **`ord_layer_1`**: A numeric sort key for ordering rows within a
  layer. For count layers this reflects the target variable’s sort
  position; for desc layers it reflects the order of statistics in the
  `format_strings` list.

Nested count layers add a second within-layer ordering column,
`ord_layer_2`, which tracks the nesting depth. Additional `by` variables
may produce further ordering columns.

Let’s see this in a simple example.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "ord_layer_index", "ord_layer_1")])
```

| rowlabel1                   | res1       | ord_layer_index | ord_layer_1 |
|:----------------------------|:-----------|----------------:|------------:|
| ADVERSE EVENT               | 8 ( 9.3%)  |               1 |           1 |
| COMPLETED                   | 58 (67.4%) |               1 |           2 |
| DEATH                       | 2 ( 2.3%)  |               1 |           3 |
| LACK OF EFFICACY            | 3 ( 3.5%)  |               1 |           4 |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)  |               1 |           5 |
| PHYSICIAN DECISION          | 1 ( 1.2%)  |               1 |           6 |
| PROTOCOL VIOLATION          | 2 ( 2.3%)  |               1 |           7 |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)  |               1 |           8 |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)  |               1 |           9 |

The rows are sorted alphabetically by the values of `DCDECOD`, and
`ord_layer_1` captures that alphabetical position. The `ord_layer_index`
is 1 for every row because there is only one layer.

## Sorting Across Layers

When a spec contains multiple layers, `ord_layer_index` is what keeps
them in the right order. Consider a table that combines a count layer
with a descriptive statistics layer.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE"),
    group_desc("AGE",
      by = "Age (years)",
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"     = f_str("xx.x", "median")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "rowlabel2", "res1", "ord_layer_index", "ord_layer_1")])
```

| rowlabel1 | rowlabel2 | res1 | ord_layer_index | ord_layer_1 |
|:---|:---|:---|---:|---:|
| WHITE |  | 78 (90.7%) | 1 | 1 |
| BLACK OR AFRICAN AMERICAN |  | 8 ( 9.3%) | 1 | 2 |
| AMERICAN INDIAN OR ALASKA NATIVE |  | 0 ( 0.0%) | 1 | 3 |
| Age (years) | n | 86 | 2 | 1 |
| Age (years) | Mean (SD) | 75.2 ( 8.59) | 2 | 2 |
| Age (years) | Median | 76.0 | 2 | 3 |

The count layer rows have `ord_layer_index = 1` and the desc layer rows
have `ord_layer_index = 2`. If you wanted to flip the layers so that the
descriptive statistics appear first, you can sort by `ord_layer_index`
in a custom order:

``` r

# Swap layer order: desc first, then counts
result$sort_key <- ifelse(result$ord_layer_index == 2, 1, 2)
reordered <- result[order(result$sort_key, result$ord_layer_1), ]
kable(reordered[, c("rowlabel1", "rowlabel2", "res1", "ord_layer_index", "ord_layer_1")])
```

|  | rowlabel1 | rowlabel2 | res1 | ord_layer_index | ord_layer_1 |
|:---|:---|:---|:---|---:|---:|
| 4 | Age (years) | n | 86 | 2 | 1 |
| 5 | Age (years) | Mean (SD) | 75.2 ( 8.59) | 2 | 2 |
| 6 | Age (years) | Median | 76.0 | 2 | 3 |
| 1 | WHITE |  | 78 (90.7%) | 1 | 1 |
| 2 | BLACK OR AFRICAN AMERICAN |  | 8 ( 9.3%) | 1 | 2 |
| 3 | AMERICAN INDIAN OR ALASKA NATIVE |  | 0 ( 0.0%) | 1 | 3 |

## Sorting by Variable Values

Within a layer, tplyr2 determines row order using a priority system
implemented in the internal
[`compute_var_order()`](https://github.com/mstackhouse/tplyr2/reference/compute_var_order.md)
function. The priority is:

1.  **Factor levels** – If the target variable is a factor, rows follow
    the factor’s level order.
2.  **VARN companion columns** – If the data contains a numeric
    companion column (e.g., `RACEN` for `RACE`), its values are used as
    sort keys.
3.  **Alphabetical** – If neither of the above applies, rows are sorted
    alphabetically.

### Factor Level Ordering

When the target variable is a factor, tplyr2 uses its level order. This
gives you direct control over the row sequence.

``` r

adsl <- tplyr_adsl
adsl$DCDECOD <- factor(adsl$DCDECOD, levels = c(
  "COMPLETED",
  "ADVERSE EVENT",
  "WITHDRAWAL BY SUBJECT",
  "PHYSICIAN DECISION",
  "STUDY TERMINATED BY SPONSOR",
  "LACK OF EFFICACY",
  "PROTOCOL VIOLATION",
  "LOST TO FOLLOW-UP",
  "DEATH"
))

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        order_count_method = "byfactor"
      )
    )
  )
)

result <- tplyr_build(spec, adsl)
sorted <- result[order(result$ord_layer_1), ]
kable(sorted[, c("rowlabel1", "res1", "res2", "res3", "ord_layer_1")])
```

| rowlabel1                   | res1       | res2       | res3       | ord_layer_1 |
|:----------------------------|:-----------|:-----------|:-----------|------------:|
| COMPLETED                   | 58 (67.4%) | 27 (32.1%) | 25 (29.8%) |           1 |
| ADVERSE EVENT               | 8 ( 9.3%)  | 40 (47.6%) | 44 (52.4%) |           2 |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)  | 8 ( 9.5%)  | 10 (11.9%) |           3 |
| PHYSICIAN DECISION          | 1 ( 1.2%)  | 2 ( 2.4%)  | 0 ( 0.0%)  |           4 |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)  | 3 ( 3.6%)  | 2 ( 2.4%)  |           5 |
| LACK OF EFFICACY            | 3 ( 3.5%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |           6 |
| PROTOCOL VIOLATION          | 2 ( 2.3%)  | 3 ( 3.6%)  | 1 ( 1.2%)  |           7 |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |           8 |
| DEATH                       | 2 ( 2.3%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |           9 |

With `order_count_method = "byfactor"`, `ord_layer_1` maps to the factor
level position: `COMPLETED` is level 1, `ADVERSE EVENT` is level 2, and
so on. To get the rows in factor-level order, sort by `ord_layer_1`.

### VARN Companion Column Ordering

CDISC datasets often include numeric companion columns that encode a
preferred sort order. For example, `RACEN` provides a numeric key for
`RACE`. When tplyr2 detects a column named `<VAR>N` in the data, it uses
those values automatically.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE",
      settings = layer_settings(
        order_count_method = "byvarn"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
sorted <- result[order(result$ord_layer_1), ]
kable(sorted[, c("rowlabel1", "res1", "res2", "res3", "ord_layer_1")])
```

| rowlabel1 | res1 | res2 | res3 | ord_layer_1 |
|:---|:---|:---|:---|---:|
| WHITE | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) | 1 |
| BLACK OR AFRICAN AMERICAN | 8 ( 9.3%) | 9 (10.7%) | 6 ( 7.1%) | 2 |
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) | 3 |

Here, `RACEN` values of 1 (WHITE), 2 (BLACK OR AFRICAN AMERICAN), and 6
(AMERICAN INDIAN OR ALASKA NATIVE) drive the sort keys in `ord_layer_1`.

## Sorting Descriptive Statistics

For
[`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
layers, `ord_layer_1` reflects the position of each statistic within the
`format_strings` list. The first entry gets 1, the second gets 2, and so
forth. There is no additional setting needed – the order you write your
format strings in is the order your rows appear.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
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
kable(result[, c("rowlabel1", "res1", "res2", "res3", "ord_layer_1")])
```

| rowlabel1 | res1         | res2         | res3         | ord_layer_1 |
|:----------|:-------------|:-------------|:-------------|------------:|
| n         | 86           | 84           | 84           |           1 |
| Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |           2 |
| Median    | 76.0         | 76.0         | 77.5         |           3 |
| Q1, Q3    | 69.2, 81.8   | 70.8, 80.0   | 71.0, 82.0   |           4 |
| Min, Max  | 52, 89       | 56, 88       | 51, 88       |           5 |

If you want “Median” to appear before “Mean (SD)”, simply move it
earlier in the `format_strings` list.

## Sorting Count Layers

Count layers offer the most flexibility through the `order_count_method`
parameter in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
The available methods are:

| Method           | Description                                        |
|------------------|----------------------------------------------------|
| `"byfactor"`     | Sort by factor level position                      |
| `"byvarn"`       | Sort by a numeric companion column (e.g., `RACEN`) |
| `"bycount"`      | Sort by count values (descending)                  |
| `"alphabetical"` | Sort by the target value alphabetically            |

When `order_count_method` is left as `NULL` (the default), tplyr2
auto-detects: it checks for factor levels first, then VARN companions,
then falls back to alphabetical.

### Sorting by Count

A frequent request – especially for adverse event tables – is to order
rows by descending frequency so the most common events appear first. Set
`order_count_method = "bycount"`:

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(order_count_method = "bycount"))
  )
)
result <- tplyr_build(spec, tplyr_adsl)
result <- result[order(result$ord_layer_1), ]
kable(result[, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1                   | res1       | res2       | res3       |
|:----------------------------|:-----------|:-----------|:-----------|
| COMPLETED                   | 58 (67.4%) | 27 (32.1%) | 25 (29.8%) |
| ADVERSE EVENT               | 8 ( 9.3%)  | 40 (47.6%) | 44 (52.4%) |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)  | 8 ( 9.5%)  | 10 (11.9%) |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)  | 3 ( 3.6%)  | 2 ( 2.4%)  |
| PROTOCOL VIOLATION          | 2 ( 2.3%)  | 3 ( 3.6%)  | 1 ( 1.2%)  |
| LACK OF EFFICACY            | 3 ( 3.5%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| DEATH                       | 2 ( 2.3%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| PHYSICIAN DECISION          | 1 ( 1.2%)  | 2 ( 2.4%)  | 0 ( 0.0%)  |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |

The rows are ordered by total count across all treatment columns,
descending, so the most frequent disposition reasons come first. Two
companion settings refine this:

- `result_order_var` chooses which statistic drives the sort (defaults
  to `"n"`; set it to `"distinct_n"` to sort by distinct subjects when
  `distinct_by` is in play).
- `ordering_cols` restricts the tally to a specific column level instead
  of the total across all columns – e.g. `ordering_cols = "Placebo"`
  sorts by the Placebo count.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        order_count_method = "bycount",
        ordering_cols = "Placebo"))
  )
)
result <- tplyr_build(spec, tplyr_adsl)
result <- result[order(result$ord_layer_1), ]
kable(result[, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1                   | res1       | res2       | res3       |
|:----------------------------|:-----------|:-----------|:-----------|
| COMPLETED                   | 58 (67.4%) | 27 (32.1%) | 25 (29.8%) |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)  | 8 ( 9.5%)  | 10 (11.9%) |
| ADVERSE EVENT               | 8 ( 9.3%)  | 40 (47.6%) | 44 (52.4%) |
| LACK OF EFFICACY            | 3 ( 3.5%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| DEATH                       | 2 ( 2.3%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| PROTOCOL VIOLATION          | 2 ( 2.3%)  | 3 ( 3.6%)  | 1 ( 1.2%)  |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)  | 3 ( 3.6%)  | 2 ( 2.4%)  |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| PHYSICIAN DECISION          | 1 ( 1.2%)  | 2 ( 2.4%)  | 0 ( 0.0%)  |

`"bycount"` keeps `by`-groups blocked and sorts the target within each
group; total and missing rows always sort last regardless of their
counts.

If you need an ordering that no setting expresses, you can always sort
the built frame yourself in post-processing – pull values out of the
result with
[`str_extract_num()`](https://github.com/mstackhouse/tplyr2/reference/str_extract_num.md)
(or
[`tplyr_numeric_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_numeric_data.md))
and reorder.

## Nested Count Sorting

Nested count layers – created by passing two variables to
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
– produce an interleaved output where outer-level rows (e.g., body
system) alternate with their inner-level rows (e.g., preferred term).
The ordering system handles both levels.

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
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

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "ord_layer_index",
                       "ord_layer_1", "ord_layer_2")], 12))
```

| rowlabel1 | rowlabel2 | res1 | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:---|:---|:---|---:|---:|---:|
| CARDIAC DISORDERS |  | 4 ( 4.7%) | 1 | 1 | 1 |
| CARDIAC DISORDERS | ATRIAL FIBRILLATION | 0 ( 0.0%) | 1 | 2 | 2 |
| CARDIAC DISORDERS | ATRIAL FLUTTER | 0 ( 0.0%) | 1 | 3 | 2 |
| CARDIAC DISORDERS | ATRIAL HYPERTROPHY | 1 ( 1.2%) | 1 | 4 | 2 |
| CARDIAC DISORDERS | BUNDLE BRANCH BLOCK RIGHT | 1 ( 1.2%) | 1 | 5 | 2 |
| CARDIAC DISORDERS | CARDIAC FAILURE CONGESTIVE | 1 ( 1.2%) | 1 | 6 | 2 |
| CARDIAC DISORDERS | MYOCARDIAL INFARCTION | 0 ( 0.0%) | 1 | 7 | 2 |
| CARDIAC DISORDERS | SINUS BRADYCARDIA | 0 ( 0.0%) | 1 | 8 | 2 |
| CARDIAC DISORDERS | SUPRAVENTRICULAR EXTRASYSTOLES | 1 ( 1.2%) | 1 | 9 | 2 |
| CARDIAC DISORDERS | SUPRAVENTRICULAR TACHYCARDIA | 0 ( 0.0%) | 1 | 10 | 2 |
| CARDIAC DISORDERS | TACHYCARDIA | 1 ( 1.2%) | 1 | 11 | 2 |
| CARDIAC DISORDERS | VENTRICULAR EXTRASYSTOLES | 0 ( 0.0%) | 1 | 12 | 2 |

In nested output:

- `ord_layer_1` captures the row sequence within the interleaved
  structure.
- `ord_layer_2` indicates the nesting depth: 1 for outer-level (body
  system) rows, 2 for inner-level (preferred term) rows. Total rows, if
  present, get depth 0.

### Sorting Preferred Terms by Frequency

The canonical adverse event display sorts preferred terms by descending
frequency *within* each body system. `order_count_method = "bycount"`
reaches the inner level of a nested layer and does exactly that, with
`ordering_cols` choosing which treatment column drives the sort:

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by         = "USUBJID",
        order_count_method  = "bycount",
        ordering_cols       = "Xanomeline High Dose",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", "res2", "ord_layer_1",
                      "ord_layer_2")], 12))
```

| rowlabel1 | rowlabel2 | res2 | ord_layer_1 | ord_layer_2 |
|:---|:---|:---|---:|---:|
| CARDIAC DISORDERS |  | 6 ( 7.1%) | 1 | 1 |
| CARDIAC DISORDERS | SINUS BRADYCARDIA | 3 ( 3.6%) | 2 | 2 |
| CARDIAC DISORDERS | ATRIAL FLUTTER | 1 ( 1.2%) | 3 | 2 |
| CARDIAC DISORDERS | MYOCARDIAL INFARCTION | 1 ( 1.2%) | 4 | 2 |
| CARDIAC DISORDERS | VENTRICULAR EXTRASYSTOLES | 1 ( 1.2%) | 5 | 2 |
| CARDIAC DISORDERS | ATRIAL FIBRILLATION | 0 ( 0.0%) | 6 | 2 |
| CARDIAC DISORDERS | ATRIAL HYPERTROPHY | 0 ( 0.0%) | 7 | 2 |
| CARDIAC DISORDERS | BUNDLE BRANCH BLOCK RIGHT | 0 ( 0.0%) | 8 | 2 |
| CARDIAC DISORDERS | CARDIAC FAILURE CONGESTIVE | 0 ( 0.0%) | 9 | 2 |
| CARDIAC DISORDERS | SUPRAVENTRICULAR EXTRASYSTOLES | 0 ( 0.0%) | 10 | 2 |
| CARDIAC DISORDERS | SUPRAVENTRICULAR TACHYCARDIA | 0 ( 0.0%) | 11 | 2 |
| CARDIAC DISORDERS | TACHYCARDIA | 0 ( 0.0%) | 12 | 2 |

Within Cardiac Disorders the terms now run 3, 1, 1, 1, 0, 0, … on the
`Xanomeline High Dose` column, with ties broken alphabetically. Each
body system is sorted independently, and its own subtotal row stays at
the top of its block.

**The outer level is not sorted by frequency.** `bycount` applies to the
inner level only; absent it, nested layers order both levels by factor
level (then VARN, then alphabetical), and with it the outer level keeps
that default order.

`outer_sort_position` accepts `"asc"` or `"desc"`, but `"desc"`
*reverses* the outer level’s default order rather than ranking the
system organ classes by count – so Vascular Disorders, last
alphabetically and with no High Dose subjects at all, leads. If your
shell needs the body systems in descending frequency too, reorder the
outer blocks yourself after the build: pull each system’s subtotal from
`ord_layer_2 == 1`, rank those, and use the ranking as the primary sort
key ahead of `ord_layer_1`.

To reverse the outer level – for example to list system organ classes in
descending rather than ascending order – set
`outer_sort_position = "desc"`; the inner (preferred-term) order and the
subtotal-before-detail nesting are preserved.

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        outer_sort_position = "desc"))
  )
)
result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
result <- result[order(result$ord_layer_1), ]
kable(head(result[, c("rowlabel1", "rowlabel2", "res1")], 12))
```

| rowlabel1                              | rowlabel2            | res1      |
|:---------------------------------------|:---------------------|:----------|
| VASCULAR DISORDERS                     |                      | 0 ( 0.0%) |
| VASCULAR DISORDERS                     | HYPOTENSION          | 0 ( 0.0%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS |                      | 7 ( 8.1%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | BLISTER              | 0 ( 0.0%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | DERMATITIS CONTACT   | 0 ( 0.0%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | ERYTHEMA             | 4 ( 4.7%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | HYPERHIDROSIS        | 0 ( 0.0%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | PRURITUS             | 3 ( 3.5%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | PRURITUS GENERALISED | 0 ( 0.0%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | RASH                 | 0 ( 0.0%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | RASH MACULO-PAPULAR  | 0 ( 0.0%) |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | RASH PRURITIC        | 0 ( 0.0%) |

## Practical Example: Preparing a Final Table

In practice, the ordering columns are tools for you to use during
post-processing. When you are ready to produce a final display table,
you will typically sort your data using the `ord_layer_*` columns and
then drop them before rendering.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE",
      by = "Race",
      settings = layer_settings(
        order_count_method = "byvarn"
      )
    ),
    group_desc("AGE",
      by = "Age (years)",
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"     = f_str("xx.x", "median"),
          "Min, Max"   = f_str("xx, xx", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)

# Sort by layer index, then within-layer order
result <- result[order(result$ord_layer_index, result$ord_layer_1), ]

# Drop ordering columns for display
display_cols <- !grepl("^ord_", names(result))
kable(result[, display_cols])
```

| rowlabel1 | rowlabel2 | res1 | res2 | res3 |
|:---|:---|:---|:---|:---|
| Race | WHITE | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |
| Race | BLACK OR AFRICAN AMERICAN | 8 ( 9.3%) | 9 (10.7%) | 6 ( 7.1%) |
| Race | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| Age (years) | n | 86 | 84 | 84 |
| Age (years) | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Age (years) | Median | 76.0 | 76.0 | 77.5 |
| Age (years) | Min, Max | 52, 89 | 56, 88 | 51, 88 |

The ordering columns gave us full control over the row sequence. After
sorting, we strip them away so the final table shows only the content
columns. This pattern – build, sort, drop – is the standard workflow for
preparing tplyr2 output for display or export.
