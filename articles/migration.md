# Migrating from Tplyr v1

## Introduction

Both Tplyr v1 and tplyr2 build formatted clinical summary tables from
ADaM-style data, but they take fundamentally different approaches.

**Tplyr v1** uses an imperative, piped workflow: create a table object
holding data and configuration, pipe in layers, set options with
modifier functions, then call `build()`.

**tplyr2** uses a declarative, spec-based approach: build a
[`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md)
that is pure configuration (no data, no side effects), then supply data
at build time via `tplyr_build(spec, data)`. This separation makes specs
portable, serializable, and reusable across datasets and studies.

This vignette covers the key differences with side-by-side examples.

## Quick Reference: Function Mapping

The table below maps v1 functions to their tplyr2 equivalents.

| Tplyr v1                                                              | tplyr2                                                                                                                                                                     | Notes                                 |
|-----------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------|
| `tplyr_table(data, treat_var)`                                        | `tplyr_spec(cols = "treat_var")`                                                                                                                                           | Data at build time, not in the spec   |
| `add_layer()`                                                         | [`tplyr_layers()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_layers.md) inside [`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md) | Declarative layer collection          |
| `group_count(target_var)`                                             | `group_count("target_var")`                                                                                                                                                | Variable names are quoted strings     |
| `group_desc(target_var)`                                              | `group_desc("target_var")`                                                                                                                                                 | Variable names are quoted strings     |
| `group_shift(vars)`                                                   | `group_shift(c(row = "v1", column = "v2"))`                                                                                                                                | Named character vector                |
| `set_format_strings()`                                                | `format_strings` in [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)                                                                | Nested in settings object             |
| `set_distinct_by()`                                                   | `distinct_by` in [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)                                                                   | Character string                      |
| `set_denoms_by()`                                                     | `denoms_by` in [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)                                                                     | Character vector                      |
| `set_where()`                                                         | `where` parameter in layer or spec                                                                                                                                         | Bare expression (unquoted)            |
| `add_total_group()`                                                   | [`total_group()`](https://github.com/mstackhouse/tplyr2/reference/total_group.md) in spec’s `total_groups`                                                                 | Spec-level config                     |
| `set_pop_data()`                                                      | [`pop_data()`](https://github.com/mstackhouse/tplyr2/reference/pop_data.md) in spec + [`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md)    | Config in spec, data at build         |
| `build()`                                                             | `tplyr_build(spec, data)`                                                                                                                                                  | Data supplied at build time           |
| [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md) | [`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)                                                                                                      | Variable names are now quoted strings |

## Key Differences in Detail

### Data Is Separated from Configuration

In v1, data lives inside the table object from the moment you create it:

``` r
# Tplyr v1: data bound at table creation
t <- tplyr_table(adsl, TRT01P)
```

In tplyr2, the spec knows nothing about data. You supply data only when
you are ready to build:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX")
  )
)

# Data provided at build time
result <- tplyr_build(spec, tplyr_adsl)
```

This means the same spec can be applied to different datasets without
modifying the spec itself.

### Variable Names Are Quoted Strings

In v1, variable names are bare symbols. In tplyr2, they are character
strings. This is required for JSON/YAML serialization and simpler
programmatic construction. The one exception is `where`, which accepts
bare expressions:

``` r
# Tplyr v1: bare symbols              # tplyr2: quoted strings
group_count(SEX)                       group_count("SEX")
group_desc(AGE)                        group_desc("AGE")
```

``` r
group_count("SEX", where = SAFFL == "Y")
```

### Settings Are Collected in an Object

In v1, you configure layers by piping modifier functions:

``` r
# Tplyr v1: piped modifiers
group_count(RACE) %>%
  set_format_strings(f_str("xx (xx.x%)", n, pct)) %>%
  set_distinct_by(USUBJID) %>%
  set_denoms_by(TRT01P)
```

In tplyr2, all configuration lives in a single
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
object:

``` r
# tplyr2: declarative settings
group_count("RACE",
  settings = layer_settings(
    format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct")),
    distinct_by = "USUBJID",
    denoms_by = "TRT01P"
  )
)
```

### Format Strings Use Quoted Variable Names

The
[`f_str()`](https://github.com/mstackhouse/tplyr2/reference/f_str.md)
function works the same way, but variable names are now strings. For
desc layers, format strings are a named list (each name becomes a row
label). For count layers, the list key is `n_counts`:

``` r
# v1: bare symbols                     # tplyr2: quoted strings
f_str("xx (xx.x%)", n, pct)            f_str("xx (xx.x%)", "n", "pct")
```

``` r
# Desc layer: named list of format strings
format_strings = list(
  "n"         = f_str("xxx", "n"),
  "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
)
# Count layer: key is n_counts
format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
```

## Side-by-Side Examples

### Demographics Table

#### Tplyr v1

``` r
# Tplyr v1 approach (not evaluated)
tplyr_table(adsl, TRT01P, where = SAFFL == "Y") %>%
  add_layer(
    group_count(SEX, by = "Sex n (%)")
  ) %>%
  add_layer(
    group_desc(AGE, by = "Age (Years)") %>%
      set_format_strings(
        "n"         = f_str("xxx", n),
        "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd),
        "Median"    = f_str("xx.x", median),
        "Min, Max"  = f_str("xx, xx", min, max)
      )
  ) %>%
  build()
```

#### tplyr2

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count("SEX", by = "Sex n (%)"),
    group_desc("AGE",
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

### Adverse Events Table (Nested Counts)

#### Tplyr v1

``` r
# Tplyr v1 approach (not evaluated)
tplyr_table(adae, TRTA) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>%
      set_distinct_by(USUBJID) %>%
      set_format_strings(f_str("xxx (xx.x%)", distinct_n, distinct_pct)) %>%
      set_order_count_method("bycount") %>%
      set_ordering_cols("Xanomeline High Dose")
  ) %>%
  build()
```

#### tplyr2

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
        order_count_method = "bycount",
        ordering_cols = "Xanomeline High Dose"
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

Note how `vars(AEBODSYS, AEDECOD)` becomes `c("AEBODSYS", "AEDECOD")`,
piped modifiers become arguments in
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md),
and data is supplied at build.

## New Features in tplyr2

Beyond the API redesign, tplyr2 introduces several entirely new
capabilities.

### Spec Serialization

Specs can be saved to disk as JSON or YAML and loaded later, enabling
centralized spec authoring with distributed execution:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX"),
    group_desc("AGE")
  )
)

tmp <- tempfile(fileext = ".json")
tplyr_write_spec(spec, tmp)
spec_loaded <- tplyr_read_spec(tmp)
spec_loaded
#> tplyr2 table specification
#> Column variables: TRT01PLayers: 2[1] count: SEX (Layer 1)[2] desc: AGE (Layer 2)
```

### Custom Analysis Layers

[`group_analyze()`](https://github.com/mstackhouse/tplyr2/reference/group_analyze.md)
accepts a user-defined function for arbitrary computations. The function
receives each group’s data subset and returns a data.frame of numeric
results:

``` r
custom_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  data.frame(
    geo_mean = exp(mean(log(vals[vals > 0]), na.rm = TRUE)),
    geo_sd   = exp(sd(log(vals[vals > 0]), na.rm = TRUE))
  )
}

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_analyze("AGE", analyze_fn = custom_fn, settings = layer_settings(
      format_strings = list(
        "Geometric Mean (SD)" = f_str("xx.xx (xx.xx)", "geo_mean", "geo_sd")
      )
    ))
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])
```

| rowlabel1           | res1          | res2          | res3          |
|:--------------------|:--------------|:--------------|:--------------|
| Geometric Mean (SD) | 74.70 ( 1.13) | 73.94 ( 1.12) | 75.18 ( 1.12) |

### Cell-Level Metadata

When `metadata = TRUE` is passed to
[`tplyr_build()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_build.md),
every cell carries metadata tracing back to source data rows for
auditability:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("SEX"))
)
result <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
row_ids <- generate_row_ids(result)

# Inspect the metadata for one cell, then retrieve its source rows
tplyr_meta_result(result, row_ids[1], "res1")
#> tplyr_meta [layer 1]
#>   Names: TRT01P, SEX
#>   Filters:
#>     TRT01P == "Placebo"
#>     SEX == "F"
source_rows <- tplyr_meta_subset(result, row_ids[1], "res1", tplyr_adsl)
kable(head(source_rows[, c("USUBJID", "SEX", "TRT01P")]))
```

| USUBJID     | SEX | TRT01P  |
|:------------|:----|:--------|
| 01-701-1015 | F   | Placebo |
| 01-701-1047 | F   | Placebo |
| 01-701-1153 | F   | Placebo |
| 01-701-1203 | F   | Placebo |
| 01-701-1345 | F   | Placebo |
| 01-701-1363 | F   | Placebo |

### ARD Conversion and Numeric Data

[`tplyr_to_ard()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_to_ard.md)
converts results into long-format Analysis Results Data.
[`tplyr_numeric_data()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_numeric_data.md)
provides raw unformatted numbers for validation:

``` r
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("SEX"))
)
result <- tplyr_build(spec, tplyr_adsl)

kable(head(tplyr_to_ard(result), 10))
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

``` r
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

## Summary

The migration from Tplyr v1 to tplyr2 involves three main shifts:

1.  **Declare, don’t pipe.** Replace
    `tplyr_table() %>% add_layer() %>% build()` with
    [`tplyr_spec()`](https://github.com/mstackhouse/tplyr2/reference/tplyr_spec.md) +
    `tplyr_build(spec, data)`.

2.  **Quote your variable names.** Bare symbols like `AGE` become
    `"AGE"`. The `where` parameter is the only place bare expressions
    are still used.

3.  **Collect settings in one place.** Instead of piping `set_*()`
    calls, pass a
    [`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
    object to the `settings` parameter of each layer.

The output structure – `rowlabel` columns, `res` columns with label
attributes, and `ord` columns for sorting – remains the same. Existing
downstream code that consumes tplyr output should work with minimal
changes.
