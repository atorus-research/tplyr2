# Table Properties

## Introduction

In tplyr2, a table is defined by its **specification**. The
[`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md)
function captures the full configuration – column variables, filters,
treatment groups, population data, and layers – as a pure description of
what you want. No data processing happens until you call
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md).
This vignette covers the spec-level parameters that control the overall
structure of your table.

Every tplyr2 workflow follows two steps: **define** a spec with
[`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md),
then **build** the table with `tplyr_build(spec, data)`. Let’s look at
an example using the included `tplyr_adsl` dataset.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(target_var = "SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])
```

| rowlabel1 | res1       | res2       | res3       |
|:----------|:-----------|:-----------|:-----------|
| F         | 53 (61.6%) | 40 (47.6%) | 50 (59.5%) |
| M         | 33 (38.4%) | 44 (52.4%) | 34 (40.5%) |

Note how the `cols` parameter defines the column structure of the
output. Each unique value of `TRT01P` becomes a result column, and the
column labels automatically include the group count as `(N=xx)`.

## Column Variables

The `cols` parameter accepts a character vector of one or more variable
names that define the columns of your output table. The most common case
is a single treatment variable:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(target_var = "AGEGR1")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])
```

| rowlabel1 | res1       | res2       | res3       |
|:----------|:-----------|:-----------|:-----------|
| 65-80     | 42 (48.8%) | 55 (65.5%) | 47 (56.0%) |
| \<65      | 14 (16.3%) | 11 (13.1%) | 8 ( 9.5%)  |
| \>80      | 30 (34.9%) | 18 (21.4%) | 29 (34.5%) |

### Multiple Column Variables

When you provide multiple variables, tplyr2 creates a cross of all
combinations. This is useful when you need columns split by treatment
and another variable.

``` r
spec <- tplyr_spec(
  cols = c("TRT01P", "SEX"),
  layers = tplyr_layers(
    group_count(target_var = "AGEGR1")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])
```

| rowlabel1 | res1       | res2       | res3       | res4       | res5       | res6       |
|:----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|
| 65-80     | 22 (41.5%) | 20 (60.6%) | 28 (70.0%) | 27 (61.4%) | 28 (56.0%) | 19 (55.9%) |
| \<65      | 9 (17.0%)  | 5 (15.2%)  | 5 (12.5%)  | 6 (13.6%)  | 5 (10.0%)  | 3 ( 8.8%)  |
| \>80      | 22 (41.5%) | 8 (24.2%)  | 7 (17.5%)  | 11 (25.0%) | 17 (34.0%) | 12 (35.3%) |

Notice that the column labels use a `" | "` separator to show the cross
of treatment and sex, and each combination gets its own N.

## Table-Level Filtering with `where`

The `where` parameter applies a filter to all data before any layer
processing begins. This is useful when records should be excluded from
the entire table.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count(target_var = "AGEGR1", by = "Age Group"),
    group_desc(
      target_var = "AGE",
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
kable(result[, c("rowlabel1", "rowlabel2", grep("^res", names(result), value = TRUE))])
```

| rowlabel1   | rowlabel2 | res1         | res2         | res3         |
|:------------|:----------|:-------------|:-------------|:-------------|
| Age Group   | 65-80     | 42 (48.8%)   | 55 (65.5%)   | 47 (56.0%)   |
| Age Group   | \<65      | 14 (16.3%)   | 11 (13.1%)   | 8 ( 9.5%)    |
| Age Group   | \>80      | 30 (34.9%)   | 18 (21.4%)   | 29 (34.5%)   |
| Age (Years) | n         | 86           | 84           | 84           |
| Age (Years) | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Age (Years) | Median    | 76.0         | 76.0         | 77.5         |
| Age (Years) | Min, Max  | 52, 89       | 56, 88       | 51, 88       |

Both the count and descriptive statistics layers are computed on the
safety population. Individual layers can also have their own `where`
filters, which are applied in addition to the table-level filter.

## Treatment Groups

Clinical tables often need columns beyond the individual treatment arms.
tplyr2 provides **total groups** and **custom groups** for this purpose.

### Total Groups

A total group creates a synthetic column that includes all subjects by
duplicating every row with the column variable set to the total group
label.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  total_groups = list(
    total_group("TRT01P", label = "Total")
  ),
  layers = tplyr_layers(
    group_count(target_var = "SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])
```

| rowlabel1 | res1       | res2        | res3       | res4       |
|:----------|:-----------|:------------|:-----------|:-----------|
| F         | 53 (61.6%) | 143 (56.3%) | 40 (47.6%) | 50 (59.5%) |
| M         | 33 (38.4%) | 111 (43.7%) | 44 (52.4%) | 34 (40.5%) |

The “Total” column now appears alongside the individual treatment arms,
with its N reflecting the sum of all subjects.

### Custom Groups

Custom groups combine specific treatment levels into a new group. For
example, you might pool the two active dose groups together.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  custom_groups = list(
    custom_group(
      "TRT01P",
      "Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose")
    )
  ),
  layers = tplyr_layers(
    group_count(target_var = "SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])
```

| rowlabel1 | res1       | res2       | res3       | res4       |
|:----------|:-----------|:-----------|:-----------|:-----------|
| F         | 53 (61.6%) | 90 (53.6%) | 40 (47.6%) | 50 (59.5%) |
| M         | 33 (38.4%) | 78 (46.4%) | 44 (52.4%) | 34 (40.5%) |

The “Xanomeline” column includes all subjects from both dose groups,
while the original dose-level columns are preserved.

### Combining Total and Custom Groups

You can use both together. Custom groups are applied first, and then
total groups duplicate all rows (including the custom group rows). This
means the “Total” column will include subjects from the custom group as
well.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  custom_groups = list(
    custom_group(
      "TRT01P",
      "Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose")
    )
  ),
  total_groups = list(
    total_group("TRT01P", label = "Total")
  ),
  layers = tplyr_layers(
    group_count(target_var = "SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])
```

| rowlabel1 | res1       | res2        | res3       | res4       | res5       |
|:----------|:-----------|:------------|:-----------|:-----------|:-----------|
| F         | 53 (61.6%) | 233 (55.2%) | 90 (53.6%) | 40 (47.6%) | 50 (59.5%) |
| M         | 33 (38.4%) | 189 (44.8%) | 78 (46.4%) | 44 (52.4%) | 34 (40.5%) |

## Population Data

In many clinical analyses, denominators and header Ns should come from a
different dataset than the analysis data. The classic example is an
adverse event table: `ADAE` only contains subjects who experienced
events, but percentages should reflect the full safety population from
`ADSL`.

The
[`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md)
configuration specifies how the population dataset maps to the spec. The
actual data is provided at build time.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count(
      target_var = "AEBODSYS",
      settings = layer_settings(
        distinct_by = "USUBJID"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))], 8))
```

| rowlabel1                                            | res1       | res2       | res3       |
|:-----------------------------------------------------|:-----------|:-----------|:-----------|
| CARDIAC DISORDERS                                    | 5 ( 5.8%)  | 6 ( 7.1%)  | 6 ( 7.1%)  |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS           | 0 ( 0.0%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| GASTROINTESTINAL DISORDERS                           | 6 ( 7.0%)  | 6 ( 7.1%)  | 3 ( 3.6%)  |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | 11 (12.8%) | 21 (25.0%) | 21 (25.0%) |
| IMMUNE SYSTEM DISORDERS                              | 0 ( 0.0%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| INFECTIONS AND INFESTATIONS                          | 5 ( 5.8%)  | 4 ( 4.8%)  | 3 ( 3.6%)  |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS       | 2 ( 2.3%)  | 2 ( 2.4%)  | 2 ( 2.4%)  |
| INVESTIGATIONS                                       | 3 ( 3.5%)  | 1 ( 1.2%)  | 1 ( 1.2%)  |

A few things to note:

- `cols = "TRTA"` matches the treatment variable in `ADAE`.
- `pop_data(cols = c("TRTA" = "TRT01A"))` maps `TRT01A` in the
  population data to `TRTA` in the analysis data (format:
  `c("analysis_name" = "pop_name")`).
- `distinct_by = "USUBJID"` counts each subject once per body system.
- Denominators and column Ns come from the full `tplyr_adsl` population.

### Extracting Header N

After building a table with population data, you can extract the header
N values using
[`tplyr_header_n()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_header_n.md):

``` r
header_n <- tplyr_header_n(result)
kable(header_n)
```

| TRTA                 |  .n |
|:---------------------|----:|
| Placebo              |  86 |
| Xanomeline High Dose |  84 |
| Xanomeline Low Dose  |  84 |

This is useful when you need to programmatically construct column
headers or integrate with other reporting tools.

### Population Data with Filters

The population data is **not** subject to the spec-level `where` filter.
It uses its own `where` clause, specified in the
[`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md)
call:

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(
    cols = c("TRTA" = "TRT01A"),
    where = SAFFL == "Y"
  ),
  layers = tplyr_layers(
    group_count(
      target_var = "AEBODSYS",
      settings = layer_settings(
        distinct_by = "USUBJID"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(tplyr_header_n(result))
```

| TRTA                 |  .n |
|:---------------------|----:|
| Placebo              |  86 |
| Xanomeline High Dose |  84 |
| Xanomeline Low Dose  |  84 |

This separation is intentional. The table-level `where` controls which
records are summarized, while `pop_data` `where` controls which subjects
contribute to denominators. In practice these often differ – you might
filter AE records to treatment-emergent events while basing denominators
on the full safety population.

## Data Completion

When building count layers, tplyr2 automatically completes all
combinations of factor levels and cross-variables. If a treatment group
has zero subjects with a given characteristic, a `0 (0.0%)` row still
appears rather than being dropped.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(target_var = "RACE")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])
```

| rowlabel1                        | res1       | res2       | res3       |
|:---------------------------------|:-----------|:-----------|:-----------|
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)  | 9 (10.7%)  | 6 ( 7.1%)  |
| WHITE                            | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |

Every race category appears for every treatment group, even when the
count is zero.

### Limiting Completion with `limit_data_by`

Sometimes completing all combinations is too aggressive. The
`limit_data_by` parameter in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
restricts the completion grid to combinations that actually exist in the
data. This is essential for AE tables where preferred terms should only
appear under their actual body system:

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count(
      target_var = "AEDECOD",
      by = "AEBODSYS",
      settings = layer_settings(
        distinct_by = "USUBJID",
        limit_data_by = c("AEBODSYS", "AEDECOD")
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", grep("^res", names(result), value = TRUE))], 10))
```

| rowlabel1         | rowlabel2                      | res1      | res2      | res3      |
|:------------------|:-------------------------------|:----------|:----------|:----------|
| CARDIAC DISORDERS | ATRIAL FIBRILLATION            | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| CARDIAC DISORDERS | ATRIAL FLUTTER                 | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | ATRIAL HYPERTROPHY             | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | BUNDLE BRANCH BLOCK RIGHT      | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | CARDIAC FAILURE CONGESTIVE     | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC DISORDERS | MYOCARDIAL INFARCTION          | 0 ( 0.0%) | 1 ( 1.2%) | 2 ( 2.4%) |
| CARDIAC DISORDERS | SINUS BRADYCARDIA              | 0 ( 0.0%) | 3 ( 3.6%) | 1 ( 1.2%) |
| CARDIAC DISORDERS | SUPRAVENTRICULAR EXTRASYSTOLES | 1 ( 1.2%) | 0 ( 0.0%) | 1 ( 1.2%) |
| CARDIAC DISORDERS | SUPRAVENTRICULAR TACHYCARDIA   | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| CARDIAC DISORDERS | TACHYCARDIA                    | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |

With `limit_data_by = c("AEBODSYS", "AEDECOD")`, tplyr2 only creates
rows for body system/preferred term combinations that exist in the data,
while still filling in zeros for treatment groups with no events for a
given combination.

## Where to Go From Here

This vignette covered the table-level properties that control the
overall structure of your tplyr2 output. For details on specific layer
types and additional features, see:

- **Count layers**:
  [`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
  for frequency tables, including nested counts, distinct subject
  counts, and missing value handling
- **Descriptive statistics layers**:
  [`group_desc()`](https://github.com/mstackhouse/tplyr2/reference/group_desc.md)
  for summary statistics with format strings, auto-precision, and custom
  summary functions
- **Shift layers**:
  [`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md)
  for baseline-by-post-baseline cross-tabulations
- **Ordering**: How tplyr2 sorts rows and controls output order
- **Options**: Package-level options via
  [`tplyr2_options()`](https://github.com/mstackhouse/tplyr2/reference/tplyr2_options.md)
