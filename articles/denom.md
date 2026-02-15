# Denominators

## Introduction

When a clinical table displays a percentage, the natural question is: a
percentage of what? The denominator – the number in the bottom of that
fraction – drives the interpretation of every count cell in your output.
A disposition table typically uses the total number of subjects in each
treatment arm. An adverse event table stratified by gender might need
the number of subjects within each treatment-by-gender combination. A
subgroup analysis might exclude certain categories from the denominator
entirely.

tplyr2 provides several mechanisms for controlling denominators in count
layers, all configured through
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
This vignette walks through each one, starting with the default behavior
and building up to more specialized scenarios.

## Default Behavior

By default, tplyr2 uses the column variable total as the denominator. If
your spec uses `cols = "TRT01P"`, then the denominator for each cell is
the total number of rows in that treatment group.

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1                        | res1       | res2       | res3       |
|:---------------------------------|:-----------|:-----------|:-----------|
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)  | 9 (10.7%)  | 6 ( 7.1%)  |
| WHITE                            | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |

Each percentage is computed against the total count within its column.
For the Placebo arm, there are 86 subjects, so the WHITE row shows
`78 (90.7%)` because 78 / 86 = 90.7%.

This default is appropriate for most simple tables, but many real-world
scenarios call for something different.

## Controlling Denominator Grouping with `denoms_by`

The `denoms_by` parameter in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
lets you specify exactly which variables define the denominator groups.
When you add a `by` variable to a count layer, the default denominator
is still the column total. But sometimes you want the percentage to
reflect the subgroup population instead.

Consider a disposition table broken out by sex. With the default
denominator, each cell percentage is based on the total treatment arm
count:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      by = "SEX"
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 10))
```

| rowlabel1 | rowlabel2                   | res1       | res2       | res3       |
|:----------|:----------------------------|:-----------|:-----------|:-----------|
| F         | ADVERSE EVENT               | 6 ( 7.0%)  | 20 (23.8%) | 26 (31.0%) |
| F         | COMPLETED                   | 34 (39.5%) | 13 (15.5%) | 17 (20.2%) |
| F         | DEATH                       | 1 ( 1.2%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| F         | LACK OF EFFICACY            | 2 ( 2.3%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| F         | LOST TO FOLLOW-UP           | 1 ( 1.2%)  | 0 ( 0.0%)  | 1 ( 1.2%)  |
| F         | PHYSICIAN DECISION          | 1 ( 1.2%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| F         | PROTOCOL VIOLATION          | 1 ( 1.2%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| F         | STUDY TERMINATED BY SPONSOR | 1 ( 1.2%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| F         | WITHDRAWAL BY SUBJECT       | 6 ( 7.0%)  | 4 ( 4.8%)  | 5 ( 6.0%)  |
| M         | ADVERSE EVENT               | 2 ( 2.3%)  | 20 (23.8%) | 18 (21.4%) |

Here the percentages use the full treatment arm as the denominator. But
if you want each sex subgroup to sum to 100% independently, set
`denoms_by` to include both the column variable and the `by` variable:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      by = "SEX",
      settings = layer_settings(
        denoms_by = c("TRT01P", "SEX")
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 10))
```

| rowlabel1 | rowlabel2                   | res1       | res2       | res3       |
|:----------|:----------------------------|:-----------|:-----------|:-----------|
| F         | ADVERSE EVENT               | 6 (11.3%)  | 20 (50.0%) | 26 (52.0%) |
| F         | COMPLETED                   | 34 (64.2%) | 13 (32.5%) | 17 (34.0%) |
| F         | DEATH                       | 1 ( 1.9%)  | 0 ( 0.0%)  | 1 ( 2.0%)  |
| F         | LACK OF EFFICACY            | 2 ( 3.8%)  | 1 ( 2.5%)  | 0 ( 0.0%)  |
| F         | LOST TO FOLLOW-UP           | 1 ( 1.9%)  | 0 ( 0.0%)  | 1 ( 2.0%)  |
| F         | PHYSICIAN DECISION          | 1 ( 1.9%)  | 1 ( 2.5%)  | 0 ( 0.0%)  |
| F         | PROTOCOL VIOLATION          | 1 ( 1.9%)  | 1 ( 2.5%)  | 0 ( 0.0%)  |
| F         | STUDY TERMINATED BY SPONSOR | 1 ( 1.9%)  | 0 ( 0.0%)  | 0 ( 0.0%)  |
| F         | WITHDRAWAL BY SUBJECT       | 6 (11.3%)  | 4 (10.0%)  | 5 (10.0%)  |
| M         | ADVERSE EVENT               | 2 ( 6.1%)  | 20 (45.5%) | 18 (52.9%) |

Now the denominator for each cell is the count of subjects in that
specific treatment-by-sex combination. The female Placebo subjects have
their own denominator, the male Placebo subjects have theirs, and so on.
Within each sex-by-treatment group, the percentages will sum to 100%.

The `denoms_by` parameter accepts any character vector of variable names
present in the data. You are not restricted to column or `by` variables
– you could use any grouping that makes sense for your analysis.

## Separate Denominator Populations with `denom_where`

Sometimes the denominator should come from a different subset of the
data than the numerator. The `denom_where` parameter accepts a quoted
expression that filters the data used for denominator computation, while
leaving the numerator counts unaffected.

For example, suppose you want to show all disposition categories but
compute percentages relative to subjects who did not complete the study:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        denom_where = quote(DCDECOD != "COMPLETED")
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1                   | res1        | res2       | res3       |
|:----------------------------|:------------|:-----------|:-----------|
| ADVERSE EVENT               | 8 (28.6%)   | 40 (70.2%) | 44 (74.6%) |
| COMPLETED                   | 58 (207.1%) | 27 (47.4%) | 25 (42.4%) |
| DEATH                       | 2 ( 7.1%)   | 0 ( 0.0%)  | 1 ( 1.7%)  |
| LACK OF EFFICACY            | 3 (10.7%)   | 1 ( 1.8%)  | 0 ( 0.0%)  |
| LOST TO FOLLOW-UP           | 1 ( 3.6%)   | 0 ( 0.0%)  | 1 ( 1.7%)  |
| PHYSICIAN DECISION          | 1 ( 3.6%)   | 2 ( 3.5%)  | 0 ( 0.0%)  |
| PROTOCOL VIOLATION          | 2 ( 7.1%)   | 3 ( 5.3%)  | 1 ( 1.7%)  |
| STUDY TERMINATED BY SPONSOR | 2 ( 7.1%)   | 3 ( 5.3%)  | 2 ( 3.4%)  |
| WITHDRAWAL BY SUBJECT       | 9 (32.1%)   | 8 (14.0%)  | 10 (16.9%) |

Notice that the COMPLETED row now shows percentages exceeding 100%. That
is because the numerator (subjects who completed) is larger than the
denominator (subjects who did not complete). This is expected when using
`denom_where` – it shifts the frame of reference for the percentage
calculation without changing which rows appear in the count.

This feature is particularly useful in adverse event tables where you
might want to restrict the denominator to the safety population or to
subjects who received at least one dose of study medication.

## Excluding Values from Denominators with `denom_ignore`

The `denom_ignore` parameter removes specific values of the target
variable from the denominator calculation. Unlike `denom_where`, which
filters rows based on an arbitrary condition, `denom_ignore` targets
specific levels of the variable being counted.

This is useful when a categorical variable includes a level like “NOT
APPLICABLE” or “NOT ASSESSED” that should not contribute to the total.
Here we exclude “AMERICAN INDIAN OR ALASKA NATIVE” from the denominator
when counting race categories:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE",
      settings = layer_settings(
        denom_ignore = c("AMERICAN INDIAN OR ALASKA NATIVE")
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])
```

| rowlabel1                        | res1       | res2       | res3       |
|:---------------------------------|:-----------|:-----------|:-----------|
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)  | 1 ( 1.2%)  | 0 ( 0.0%)  |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)  | 9 (10.8%)  | 6 ( 7.1%)  |
| WHITE                            | 78 (90.7%) | 74 (89.2%) | 78 (92.9%) |

The “AMERICAN INDIAN OR ALASKA NATIVE” row still appears in the output
with its count, but subjects in that category are excluded from the
denominator used to calculate all percentages. This means the remaining
categories will have percentages that sum to slightly more than 100%
(because the “AMERICAN INDIAN OR ALASKA NATIVE” count was removed from
the bottom of the fraction but included in the numerator for its own
row).

You can pass multiple values to `denom_ignore` as a character vector.

## Denominators with Distinct Counts

When `distinct_by` is set in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md),
tplyr2 computes both event-level and subject-level counts. The
denominator system applies separately to each:

- `n` and `pct` use the event-level (row-level) denominator
- `distinct_n` and `distinct_pct` use the distinct-subject denominator

Both denominators respect `denoms_by`, `denom_where`, and
`denom_ignore`.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xx (xx.x%) [xxx]", "distinct_n", "distinct_pct", "n")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3")], 8))
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

The first number is the count of distinct subjects, the percentage is
the proportion of distinct subjects, and the bracketed number is the
total event count. The denominators for `distinct_n` and `distinct_pct`
are the total number of distinct subjects per treatment arm (rather than
the total number of event rows).

## Denominators with Population Data

In many clinical studies, the analysis data contains only a subset of
the randomized population. Adverse event data, for example, only
includes subjects who experienced at least one event. If you compute
denominators from the event data alone, you will undercount the
population.

The
[`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md)
configuration at the spec level tells tplyr2 to draw denominators from a
separate population dataset. When `pop_data` is specified, the
denominator for each treatment arm comes from the population dataset
rather than the analysis data.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3")], 8))
```

| rowlabel1                   | res1      | res2      | res3      |
|:----------------------------|:----------|:----------|:----------|
| ABDOMINAL PAIN              | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| AGITATION                   | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| ANXIETY                     | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| APPLICATION SITE DERMATITIS | 1 ( 1.2%) | 3 ( 3.6%) | 2 ( 2.4%) |
| APPLICATION SITE ERYTHEMA   | 0 ( 0.0%) | 3 ( 3.6%) | 4 ( 4.8%) |
| APPLICATION SITE IRRITATION | 1 ( 1.2%) | 3 ( 3.6%) | 2 ( 2.4%) |
| APPLICATION SITE PAIN       | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| APPLICATION SITE PRURITUS   | 4 ( 4.7%) | 6 ( 7.1%) | 5 ( 6.0%) |

Without `pop_data`, the denominators come from the AE dataset itself.
With it, they come from the full ADSL population. This typically
produces smaller percentages because the denominator is larger – it
includes all randomized subjects, not just those with events.

The named vector in `pop_data(cols = c("TRTA" = "TRT01P"))` maps the
analysis data column name (`TRTA`) to the corresponding population data
column name (`TRT01P`). This mapping is necessary when the column
variables have different names across datasets.

You can also retrieve the population-level N values used in column
headers:

``` r
kable(tplyr_header_n(result))
```

| TRTA                 |  .n |
|:---------------------|----:|
| Placebo              |  86 |
| Xanomeline High Dose |  84 |
| Xanomeline Low Dose  |  84 |

## Missing Subjects

When using population data, some subjects in the population may not
appear in the analysis data at all. The `missing_subjects` parameter
adds a row that counts these absent subjects.

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        missing_subjects = TRUE,
        missing_subjects_label = "Not reported",
        format_strings = list(
          n_counts = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)

# Show the "Not reported" row
nr_idx <- which(result$rowlabel1 == "Not reported")
kable(result[nr_idx, c("rowlabel1", "res1", "res2", "res3")])
```

|     | rowlabel1    | res1       | res2       | res3       |
|:----|:-------------|:-----------|:-----------|:-----------|
| 63  | Not reported | 54 (62.8%) | 41 (48.8%) | 34 (40.5%) |

The “Not reported” row counts subjects who are present in the population
(ADSL) but have no records in the analysis data (ADAE). This is
particularly important for adverse event tables, where a large
proportion of the safety population may have no reported events.

The `missing_subjects_label` parameter controls the text displayed in
the row label. It defaults to `"Missing"`, but `"Not reported"` or
`"No events reported"` are common alternatives for adverse event tables.

Note that `missing_subjects` requires `pop_data` to be set at the spec
level. Without a separate population dataset, there is no reference
population to identify missing subjects against.

## Putting It All Together

In practice, you often combine several of these features in a single
layer. Here is a more complete adverse event summary that uses
population-based denominators, distinct subject counts, and a missing
subjects row:

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
  total_groups = list(total_group("TRTA", label = "Total")),
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        missing_subjects = TRUE,
        missing_subjects_label = "No events reported",
        format_strings = list(
          n_counts = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2", indent = "   ")

# Show the first section plus the missing subjects row
nr_idx <- which(collapsed$row_label == "No events reported")
show_rows <- c(1:12, nr_idx)
kable(collapsed[show_rows, c("row_label", "res1", "res2", "res3", "res4")])
```

| row_label                      | res1      | res2       | res3      | res4      |
|:-------------------------------|:----------|:-----------|:----------|:----------|
| CARDIAC DISORDERS              |           |            |           |           |
|                                | 4 ( 4.7%) | 15 ( 0.0%) | 6 ( 7.1%) | 5 ( 6.0%) |
| ATRIAL FIBRILLATION            | 0 ( 0.0%) | 1 ( 0.0%)  | 0 ( 0.0%) | 1 ( 1.2%) |
| ATRIAL FLUTTER                 | 0 ( 0.0%) | 1 ( 0.0%)  | 1 ( 1.2%) | 0 ( 0.0%) |
| ATRIAL HYPERTROPHY             | 1 ( 1.2%) | 1 ( 0.0%)  | 0 ( 0.0%) | 0 ( 0.0%) |
| BUNDLE BRANCH BLOCK RIGHT      | 1 ( 1.2%) | 1 ( 0.0%)  | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC FAILURE CONGESTIVE     | 1 ( 1.2%) | 1 ( 0.0%)  | 0 ( 0.0%) | 0 ( 0.0%) |
| MYOCARDIAL INFARCTION          | 0 ( 0.0%) | 3 ( 0.0%)  | 1 ( 1.2%) | 2 ( 2.4%) |
| SINUS BRADYCARDIA              | 0 ( 0.0%) | 4 ( 0.0%)  | 3 ( 3.6%) | 1 ( 1.2%) |
| SUPRAVENTRICULAR EXTRASYSTOLES | 1 ( 1.2%) | 2 ( 0.0%)  | 0 ( 0.0%) | 1 ( 1.2%) |
| SUPRAVENTRICULAR TACHYCARDIA   | 0 ( 0.0%) | 1 ( 0.0%)  | 0 ( 0.0%) | 1 ( 1.2%) |
| TACHYCARDIA                    | 1 ( 1.2%) | 1 ( 0.0%)  | 0 ( 0.0%) | 0 ( 0.0%) |

This spec includes population-based denominators from ADSL, a “Total”
column, nested body system / preferred term counts, and a row for
subjects with no reported events. The denominator for every cell comes
from the full population dataset, so the percentages reflect the
proportion of all randomized subjects in each arm.

## Summary

The table below summarizes the denominator controls available in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md):

| Parameter                                                                   | Purpose                                             | Default                  |
|-----------------------------------------------------------------------------|-----------------------------------------------------|--------------------------|
| `denoms_by`                                                                 | Variables defining denominator groups               | Column variable(s)       |
| `denom_where`                                                               | Expression to filter denominator data               | No filter                |
| `denom_ignore`                                                              | Target variable values to exclude from denominators | None                     |
| `distinct_by`                                                               | Variable for distinct subject counting              | NULL (event-level only)  |
| `missing_subjects`                                                          | Add row for subjects absent from analysis data      | FALSE                    |
| [`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md) | Separate population dataset for denominators        | None (use analysis data) |

Each of these can be used independently or combined as needed. The key
principle is that the numerator (what you are counting) and the
denominator (what you are dividing by) can be controlled separately,
giving you precise control over how percentages are computed in your
clinical tables.
