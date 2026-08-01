# Count Layers

## Introduction

Counting things seems simple enough. You look at the data, tally up how
many observations fall into each category, and move on. But in clinical
reporting, counting is deceptively nuanced. A demographics table needs
simple frequencies. An adverse events table needs both the number of
subjects who experienced an event and the total number of events. A
disposition table might need all of those plus custom column groupings
and sorting by frequency.

tplyr2 handles all of these scenarios through
[`group_count()`](https://atorus-research.github.io/tplyr2/reference/group_count.md).
In this vignette, we will walk through the most common counting patterns
you will encounter when building clinical tables, starting with the
basics and building up to nested hierarchical summaries.

## A Simple Count

The simplest use of
[`group_count()`](https://atorus-research.github.io/tplyr2/reference/group_count.md)
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
[`total_group()`](https://atorus-research.github.io/tplyr2/reference/total_group.md).

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
[`total_group()`](https://atorus-research.github.io/tplyr2/reference/total_group.md)
function works by duplicating every row in the data with the treatment
variable set to the label you provide (here, “Total”). This means the
total column is computed through the same pipeline as every other
column, keeping the logic consistent.

### Custom Column Groups

Sometimes you need to combine existing treatment arms into a new group.
For example, you might want a “Treated” column that pools all active
dose groups. The
[`custom_group()`](https://atorus-research.github.io/tplyr2/reference/custom_group.md)
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
[`total_group()`](https://atorus-research.github.io/tplyr2/reference/total_group.md),
custom groups duplicate the matching rows with the treatment variable
set to the new label. Custom groups and total groups can be combined in
a single spec when you need both.

### Custom Format Strings

The default `n (xx.x%)` format works for many tables, but you have full
control over how counts are displayed through the
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
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
[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md)
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
| PHYSICIAN DECISION          | 1 ( 1.2%)   | 2 ( 2.4%)   | 0 ( 0.0%)   |
| PROTOCOL VIOLATION          | 2 ( 2.3%)   | 3 ( 3.6%)   | 1 ( 1.2%)   |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)   | 3 ( 3.6%)   | 2 ( 2.4%)   |
| WITHDRAWAL BY SUBJECT       | 9 (10.5%)   | 8 ( 9.5%)   | 10 (11.9%)  |
| Overall Total               | 86 (100.0%) | 84 (100.0%) | 84 (100.0%) |

## Population Data: Getting the Denominator Right

Everything so far has summarized `tplyr_adsl`, which has one row per
subject, so the column total *is* the number of subjects in each arm.
Adverse event data is different, and this is where denominators most
often go wrong.

`tplyr_adae` only contains subjects who experienced at least one event,
with many rows per subject. If you count it directly, the denominator
becomes “subjects who had *any* adverse event” rather than the number of
subjects at risk – and every incidence percentage comes out too large.
For an adverse event table (and for most tables built from a findings or
events dataset), the denominator must come from the **population
dataset** – typically `ADSL`, the set of subjects in the analysis
population.

You supply population data in two places: a
[`pop_data()`](https://atorus-research.github.io/tplyr2/reference/pop_data.md)
mapping in the spec, and the population data frame itself at build time.
Here is the same simple adverse-event count built both ways:

``` r

# WITHOUT population data: denominator = subjects present in ADAE
spec_no_pop <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD", settings = layer_settings(distinct_by = "USUBJID"))
  )
)
res_no_pop <- tplyr_build(spec_no_pop, tplyr_adae)

# WITH population data: denominator = full safety population from ADSL
spec_pop <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count("AEDECOD", settings = layer_settings(distinct_by = "USUBJID"))
  )
)
res_pop <- tplyr_build(spec_pop, tplyr_adae, pop_data = tplyr_adsl)

# Column Ns: subjects-with-events vs. the true population
sapply(c("res1", "res2", "res3"), function(c) attr(res_no_pop[[c]], "label"))
#>                          res1                          res2 
#>              "Placebo (N=47)" "Xanomeline High Dose (N=77)" 
#>                          res3 
#>  "Xanomeline Low Dose (N=76)"
sapply(c("res1", "res2", "res3"), function(c) attr(res_pop[[c]], "label"))
#>                          res1                          res2 
#>              "Placebo (N=86)" "Xanomeline High Dose (N=84)" 
#>                          res3 
#>  "Xanomeline Low Dose (N=84)"
```

The header N tells the story: without population data the Placebo column
is `N=47` (only the subjects who had an event), and with it the column
is the true `N=86`. Because the numerator is unchanged, every percentage
roughly halves once the denominator is correct – a cell that read
`6 (14.0%)` becomes `6 (7.1%)`. Getting this wrong silently inflates
every incidence in the table, so **for adverse event tables population
data is effectively mandatory.**

The named vector in `pop_data(cols = c("TRTA" = "TRT01A"))` maps the
treatment column in the analysis data (`TRTA`, actual treatment in
`ADAE`) to the matching column in the population data (`TRT01A` in
`ADSL`). The mapping is needed because the two datasets frequently name
the treatment variable differently.

Population data also unlocks related features – a “no events reported”
row for subjects who are in the population but absent from the analysis
data (`missing_subjects`), a separate denominator filter
(`denom_where`), and per-subgroup denominators (`denoms_by`). Those are
covered in depth in
[`vignette("denom")`](https://atorus-research.github.io/tplyr2/articles/denom.md).
Every adverse event example in the rest of this vignette uses population
data.

## Distinct Versus Event Counts

When summarizing adverse events, the distinction between events and
subjects matters enormously. A single subject might experience the same
event multiple times. If you simply count rows, you are counting events.
If you want the number of subjects who experienced each event at least
once, you need distinct counts.

The `distinct_by` parameter in
[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md)
tells tplyr2 which variable identifies unique subjects. Once set, the
`distinct_n` and `distinct_pct` statistics become available for use in
format strings.

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
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

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3")], 10))
```

| rowlabel1 | res1 | res2 | res3 |
|:---|:---|:---|:---|
| ABDOMINAL PAIN | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 1.2%) \[ 1\] |
| AGITATION | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 1.2%) \[ 1\] |
| ANXIETY | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 1.2%) \[ 1\] |
| APPLICATION SITE DERMATITIS | 1 ( 1.2%) \[ 1\] | 3 ( 3.6%) \[ 3\] | 2 ( 2.4%) \[ 2\] |
| APPLICATION SITE ERYTHEMA | 0 ( 0.0%) \[ 0\] | 3 ( 3.6%) \[ 3\] | 4 ( 4.8%) \[ 4\] |
| APPLICATION SITE IRRITATION | 1 ( 1.2%) \[ 1\] | 3 ( 3.6%) \[ 4\] | 2 ( 2.4%) \[ 2\] |
| APPLICATION SITE PAIN | 0 ( 0.0%) \[ 0\] | 1 ( 1.2%) \[ 1\] | 0 ( 0.0%) \[ 0\] |
| APPLICATION SITE PRURITUS | 4 ( 4.7%) \[ 4\] | 6 ( 7.1%) \[ 7\] | 5 ( 6.0%) \[ 5\] |
| APPLICATION SITE REACTION | 1 ( 1.2%) \[ 1\] | 1 ( 1.2%) \[ 1\] | 0 ( 0.0%) \[ 0\] |
| APPLICATION SITE URTICARIA | 0 ( 0.0%) \[ 0\] | 0 ( 0.0%) \[ 0\] | 1 ( 1.2%) \[ 1\] |

In this output, the first number is the count of distinct subjects, the
percentage is based on distinct subjects, and the number in brackets is
the total event count. This pattern – `xxx (xx.x%) [xxx]` for distinct
subjects, percent, and events – is one of the most common formats for
adverse event tables.

### Displaying Statistics in Separate Columns

Some sponsors require the subject and event counts in separate columns
rather than packed into one cell – for example, an “n (%)” column and an
“E” column under each treatment arm. The `stat_columns` setting produces
this layout directly. Pass a named list of
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
objects: each entry becomes its own result column per treatment group,
and the entry name becomes the column sub-label.

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        stat_columns = list(
          "n (%)" = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct"),
          "E"     = f_str("xxx", "n")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "res4")], 10))
```

| rowlabel1                   | res1      | res2 | res3      | res4 |
|:----------------------------|:----------|:-----|:----------|:-----|
| ABDOMINAL PAIN              | 0 ( 0.0%) | 0    | 0 ( 0.0%) | 0    |
| AGITATION                   | 0 ( 0.0%) | 0    | 0 ( 0.0%) | 0    |
| ANXIETY                     | 0 ( 0.0%) | 0    | 0 ( 0.0%) | 0    |
| APPLICATION SITE DERMATITIS | 1 ( 1.2%) | 1    | 3 ( 3.6%) | 3    |
| APPLICATION SITE ERYTHEMA   | 0 ( 0.0%) | 0    | 3 ( 3.6%) | 3    |
| APPLICATION SITE IRRITATION | 1 ( 1.2%) | 1    | 3 ( 3.6%) | 4    |
| APPLICATION SITE PAIN       | 0 ( 0.0%) | 0    | 1 ( 1.2%) | 1    |
| APPLICATION SITE PRURITUS   | 4 ( 4.7%) | 4    | 6 ( 7.1%) | 7    |
| APPLICATION SITE REACTION   | 1 ( 1.2%) | 1    | 1 ( 1.2%) | 1    |
| APPLICATION SITE URTICARIA  | 0 ( 0.0%) | 0    | 0 ( 0.0%) | 0    |

The result columns interleave arm-major, so each treatment group’s stat
columns sit adjacent: `res1`/`res2` are the first arm’s “n (%)” and “E”
columns, `res3`/`res4` the second arm’s, and so on. Column identity is
carried on the label attributes, which follow the pattern
`"<treatment> (N=n) | <stat name>"`:

``` r

attr(result$res1, "label")
#> [1] "Placebo (N=86) | n (%)"
attr(result$res2, "label")
#> [1] "Placebo (N=86) | E"
```

Renderers can split these labels on `" | "` to span the treatment group
(with its header N) over its stat sub-columns as a two-level header.
`stat_columns` works with nested count layers, by variables, total and
missing rows, risk difference, metadata, and serialization. A few things
to keep in mind:

- Stat names may not contain `" | "` or `"(N="`, which are reserved by
  the label grammar.
- When a spec contains multiple layers, all layers must be count layers
  using `stat_columns` with the same statistic names so result columns
  align; otherwise build separate specs.
- The `empty` argument of
  [`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
  applies per format, so a row can display empty text in one stat column
  and a value in another when their variables differ in missingness.

### A Note on Parenthesis Hugging

You may notice that tplyr2 carefully aligns numbers within their format
fields, padding with leading spaces so that decimal points and
parentheses line up across rows. For even tighter alignment, tplyr2
supports “parenthesis hugging”: writing a format group with uppercase
`X` moves its leading spaces *past* the number, so the opening
parenthesis sits flush against the first digit while the cell keeps its
total width. See
[`vignette("precision_alignment")`](https://atorus-research.github.io/tplyr2/articles/precision_alignment.md)
for the mechanics and the caveats.

## Nested Count Summaries

Adverse event tables in clinical reports almost always use a
hierarchical structure: body system (SOC) as the outer grouping and
preferred term (PT) as the inner grouping. tplyr2 supports this directly
by passing a vector of two variable names to
[`group_count()`](https://atorus-research.github.io/tplyr2/reference/group_count.md).

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
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 15))
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

The first variable in the vector (`AEBODSYS`) becomes the outer level,
and the second (`AEDECOD`) becomes the inner level. In the output,
`rowlabel1` holds the body system name and `rowlabel2` holds the
preferred term. Outer-level summary rows have an empty `rowlabel2`,
while inner-level rows show the preferred term in `rowlabel2`.

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
[`collapse_row_labels()`](https://atorus-research.github.io/tplyr2/reference/collapse_row_labels.md)
function provides two modes for this.

#### Default Mode (Stub Rows)

The default mode inserts stub rows for outer-level labels. These stub
rows have no numeric results – they serve purely as group headers:

``` r

collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2", indent = "   ")
kable(head(collapsed[, c("row_label", "res1", "res2", "res3")], 15))
```

| row_label                                  | res1      | res2      | res3      |
|:-------------------------------------------|:----------|:----------|:----------|
| CARDIAC DISORDERS                          |           |           |           |
|                                            | 4 ( 4.7%) | 6 ( 7.1%) | 5 ( 6.0%) |
| ATRIAL FIBRILLATION                        | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| ATRIAL FLUTTER                             | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| ATRIAL HYPERTROPHY                         | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| BUNDLE BRANCH BLOCK RIGHT                  | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC FAILURE CONGESTIVE                 | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| MYOCARDIAL INFARCTION                      | 0 ( 0.0%) | 1 ( 1.2%) | 2 ( 2.4%) |
| SINUS BRADYCARDIA                          | 0 ( 0.0%) | 3 ( 3.6%) | 1 ( 1.2%) |
| SUPRAVENTRICULAR EXTRASYSTOLES             | 1 ( 1.2%) | 0 ( 0.0%) | 1 ( 1.2%) |
| SUPRAVENTRICULAR TACHYCARDIA               | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| TACHYCARDIA                                | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| VENTRICULAR EXTRASYSTOLES                  | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS |           |           |           |
|                                            | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |

#### Nest Mode

With `nest = TRUE`, the row labels are collapsed in place without adding
new rows. Outer-level rows keep their results, and inner rows are
indented. This matches the behavior of `set_nest_count(TRUE)` in Tplyr
v1:

``` r

nested <- collapse_row_labels(result, nest = TRUE, indent = "   ")
kable(head(nested[, c("row_label", "res1", "res2", "res3")], 15))
```

| row_label                                  | res1      | res2      | res3      |
|:-------------------------------------------|:----------|:----------|:----------|
| CARDIAC DISORDERS                          | 4 ( 4.7%) | 6 ( 7.1%) | 5 ( 6.0%) |
| ATRIAL FIBRILLATION                        | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| ATRIAL FLUTTER                             | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| ATRIAL HYPERTROPHY                         | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| BUNDLE BRANCH BLOCK RIGHT                  | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC FAILURE CONGESTIVE                 | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| MYOCARDIAL INFARCTION                      | 0 ( 0.0%) | 1 ( 1.2%) | 2 ( 2.4%) |
| SINUS BRADYCARDIA                          | 0 ( 0.0%) | 3 ( 3.6%) | 1 ( 1.2%) |
| SUPRAVENTRICULAR EXTRASYSTOLES             | 1 ( 1.2%) | 0 ( 0.0%) | 1 ( 1.2%) |
| SUPRAVENTRICULAR TACHYCARDIA               | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| TACHYCARDIA                                | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| VENTRICULAR EXTRASYSTOLES                  | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| VENTRICULAR SEPTAL DEFECT                  | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| GASTROINTESTINAL DISORDERS                 | 6 ( 7.0%) | 4 ( 4.8%) | 3 ( 3.6%) |

The `indent` parameter controls the string used for each level of
nesting. Here we use three spaces, but you can use any string that suits
your output format.

### Nested Counts with Total Rows

Total rows work with nested counts as well. When `total_row = TRUE` is
set on a nested layer, the total row reflects the overall count across
all body systems.

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
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

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2", indent = "   ")
kable(head(collapsed[, c("row_label", "res1", "res2", "res3")], 15))
```

| row_label                                  | res1      | res2      | res3      |
|:-------------------------------------------|:----------|:----------|:----------|
| CARDIAC DISORDERS                          |           |           |           |
|                                            | 4 ( 4.7%) | 6 ( 7.1%) | 5 ( 6.0%) |
| ATRIAL FIBRILLATION                        | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| ATRIAL FLUTTER                             | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| ATRIAL HYPERTROPHY                         | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| BUNDLE BRANCH BLOCK RIGHT                  | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| CARDIAC FAILURE CONGESTIVE                 | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| MYOCARDIAL INFARCTION                      | 0 ( 0.0%) | 1 ( 1.2%) | 2 ( 2.4%) |
| SINUS BRADYCARDIA                          | 0 ( 0.0%) | 3 ( 3.6%) | 1 ( 1.2%) |
| SUPRAVENTRICULAR EXTRASYSTOLES             | 1 ( 1.2%) | 0 ( 0.0%) | 1 ( 1.2%) |
| SUPRAVENTRICULAR TACHYCARDIA               | 0 ( 0.0%) | 0 ( 0.0%) | 1 ( 1.2%) |
| TACHYCARDIA                                | 1 ( 1.2%) | 0 ( 0.0%) | 0 ( 0.0%) |
| VENTRICULAR EXTRASYSTOLES                  | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS |           |           |           |
|                                            | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |

## Where to Go from Here

Count layers in tplyr2 cover a wide range of clinical table patterns,
but there is much more to explore. Here are some related topics covered
in other vignettes:

- [`vignette("denom")`](https://atorus-research.github.io/tplyr2/articles/denom.md)
  – Denominator control in depth: population data, `denoms_by`,
  `denom_where`, `denom_ignore`, single-proportion confidence intervals,
  and the “no events reported” row.
- [`vignette("adverse-events")`](https://atorus-research.github.io/tplyr2/articles/adverse-events.md)
  – An end-to-end adverse event table combining nested counts,
  population-based incidence, sorting, and comparative statistics into
  one worked example.
- [`vignette("riskdiff")`](https://atorus-research.github.io/tplyr2/articles/riskdiff.md)
  and
  [`vignette("binding-statistics")`](https://atorus-research.github.io/tplyr2/articles/binding-statistics.md)
  – Risk differences and association-test (Fisher / chi-square) p-value
  columns alongside your counts.
- [`vignette("sort")`](https://atorus-research.github.io/tplyr2/articles/sort.md)
  – Ordering rows by frequency, factor levels, or a VARN companion
  variable.
- [`vignette("shift")`](https://atorus-research.github.io/tplyr2/articles/shift.md)
  –
  [`group_shift()`](https://atorus-research.github.io/tplyr2/reference/group_shift.md)
  for cross-tabulation of baseline versus post-baseline categories.
- [`vignette("desc")`](https://atorus-research.github.io/tplyr2/articles/desc.md)
  –
  [`group_desc()`](https://atorus-research.github.io/tplyr2/reference/group_desc.md)
  for continuous variable summaries like mean, median, and standard
  deviation.
- [`vignette("post_processing")`](https://atorus-research.github.io/tplyr2/articles/post_processing.md)
  – Row masks, conditional formatting, and row label collapsing to
  polish your final output.
- [`vignette("format_strings")`](https://atorus-research.github.io/tplyr2/articles/format_strings.md)
  – The complete count-layer statistic keyword reference and the
  `n_counts` rule.
- [`vignette("display_conventions")`](https://atorus-research.github.io/tplyr2/articles/display_conventions.md)
  – `<1%` and `>99%` percents (`pct_lt`, `pct_gt`), zero-count
  suppression (`zero_count_display`), showing only selected levels
  (`keep_levels`), and missing rows (`missing_count`).

One note on the total row: it inherits the layer’s own format string,
and there is no `total_row_format` setting. If the total needs a
different width from the category rows, reformat that row after the
build with
[`apply_formats()`](https://atorus-research.github.io/tplyr2/reference/apply_formats.md).
