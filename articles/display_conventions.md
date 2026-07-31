# Clinical Display Conventions

## Introduction

The two companion vignettes cover how a number becomes a string:
[`vignette("format_strings")`](https://github.com/mstackhouse/tplyr2/articles/format_strings.md)
for the grammar and
[`vignette("precision_alignment")`](https://github.com/mstackhouse/tplyr2/articles/precision_alignment.md)
for widths and delimiters. This one covers the rules that come from
somewhere else entirely – a statistical analysis plan, a sponsor’s mock
shell, a regulatory reviewing convention. They are not properties of the
data. They are decisions about what the table is allowed to say.

Most of them are
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md)
arguments, and most are one line each:

| Convention | Setting |
|:---|:---|
| Report small percentages as `<1%` | `pct_lt` |
| Report large percentages as `>99%` | `pct_gt` |
| Suppress the `(0.0%)` on zero counts | `zero_count_display` |
| Show only selected levels of a variable | `keep_levels` |
| Add a Missing row, or a Total that excludes it | `missing_count`, `total_row_count_missings` |
| Put each statistic in its own column | `stat_columns`, `stats_as_columns` |
| Change what a shift-table percent is out of | `shift_denom`, `denoms_by` |
| Show the shift denominator as an `n` row | `denom_row`, `denom_row_format` |
| Indent and wrap nested terms | [`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md), [`str_indent_wrap()`](https://github.com/mstackhouse/tplyr2/reference/str_indent_wrap.md) |
| Anything else, applied after the build | [`apply_conditional_format()`](https://github.com/mstackhouse/tplyr2/reference/apply_conditional_format.md) |

## Small and Large Percentages

One subject out of an arm of 300 is 0.33%, and `xxx.x%` renders it as
`0.3%`. Many reviewing conventions object: the reader is meant to see
that the event is rare, not to read a spuriously precise decimal. The
convention is `<1%`, with a matching `>99%` at the top of the scale.

`pct_lt` and `pct_gt` implement both. Here is a two-arm safety
population of 300 subjects per arm, carrying one rare event and one
nearly universal one:

``` r

set.seed(7)
n_arm <- 300
subj <- data.frame(
  USUBJID = sprintf("S%04d", seq_len(2 * n_arm)),
  TRTA    = rep(c("Placebo", "Drug"), each = n_arm)
)

incidence <- list(
  "HEADACHE"                = c(Placebo = 0.31,   Drug = 0.44),
  "NAUSEA"                  = c(Placebo = 0.12,   Drug = 0.19),
  "INJECTION SITE REACTION" = c(Placebo = 0.995,  Drug = 1.00),
  "ANAPHYLACTIC REACTION"   = c(Placebo = 1/300,  Drug = 2/300)
)

ae <- do.call(rbind, lapply(names(incidence), function(term) {
  do.call(rbind, lapply(c("Placebo", "Drug"), function(arm) {
    pool <- subj$USUBJID[subj$TRTA == arm]
    k <- round(incidence[[term]][[arm]] * length(pool))
    if (k == 0) return(NULL)
    data.frame(USUBJID = pool[seq_len(k)], TRTA = arm, AEDECOD = term)
  }))
}))

ae_spec <- function(...) {
  tplyr_spec(
    cols = "TRTA",
    pop_data = pop_data(cols = c("TRTA" = "TRTA")),
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(
          distinct_by = "USUBJID", ...,
          format_strings = list(
            n_counts = f_str("xxx (xxx.x%)", "distinct_n", "distinct_pct")
          )
        )
      )
    )
  )
}
```

Without the conventions:

``` r

show_table(tplyr_build(ae_spec(), ae, pop_data = subj))
```

| rowlabel1               | res1         | res2         |
|:------------------------|:-------------|:-------------|
| ANAPHYLACTIC REACTION   |   2 (  0.7%) |   1 (  0.3%) |
| HEADACHE                | 132 ( 44.0%) |  93 ( 31.0%) |
| INJECTION SITE REACTION | 300 (100.0%) | 298 ( 99.3%) |
| NAUSEA                  |  57 ( 19.0%) |  36 ( 12.0%) |

With them:

``` r

show_table(tplyr_build(ae_spec(pct_lt = 1, pct_gt = 99), ae, pop_data = subj))
```

| rowlabel1               | res1          | res2          |
|:------------------------|:--------------|:--------------|
| ANAPHYLACTIC REACTION   |   2 ( \<1.0%) |   1 ( \<1.0%) |
| HEADACHE                | 132 ( 44.0%)  |  93 ( 31.0%)  |
| INJECTION SITE REACTION | 300 (100.0%)  | 298 (\>99.0%) |
| NAUSEA                  |  57 ( 19.0%)  |  36 ( 12.0%)  |

The anaphylaxis row becomes `<1.0%` in both arms, and the 99.3%
injection-site reaction becomes `>99.0%`. Three behaviors are worth
knowing.

**The token is rendered at the format group’s own decimal width.**
`xxx.x%` produces `<1.0`, not `<1`. If the mock calls for `<1`, use an
integer percent field:

``` r

int_spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRTA")),
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        pct_lt = 1, pct_gt = 99,
        format_strings = list(
          n_counts = f_str("xxx (xxx%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

show_table(tplyr_build(int_spec, ae, pop_data = subj))
```

| rowlabel1               | res1       | res2        |
|:------------------------|:-----------|:------------|
| ANAPHYLACTIC REACTION   |   2 (  1%) |   1 ( \<1%) |
| HEADACHE                | 132 ( 44%) |  93 ( 31%)  |
| INJECTION SITE REACTION | 300 (100%) | 298 ( 99%)  |
| NAUSEA                  |  57 ( 19%) |  36 ( 12%)  |

**The threshold is compared against the rounded display value, not the
raw percentage.** In the integer table above, the Drug arm reported 2 of
300 subjects and shows `1%` rather than `<1%` – 0.67% rounds to `1`, and
`1` is not below the threshold. The rule is deliberate: a cell never
claims to be below a number it visibly displays. The Placebo arm’s
single subject is 0.33%, which rounds to `0`, so it does get the token.
And 99.3% rounds to `99`, which is not above 99, so the integer table
leaves it as `99`.

**Exactly 0% and exactly 100% are never rewritten.** A zero count is
zero, not “fewer than one percent”, and a universal event is 100%, not
“more than ninety-nine”. The injection-site reaction stays a true 100%
in the Drug arm in every table above – `100.0%` where the field carries
a decimal, `100%` in the integer table.

Both settings target the **first** `pct` or `distinct_pct` group in the
format string, so a cell that displays two percentages only has the
leading one rewritten. Both also apply to shift layers, where the same
reasoning holds.

## Zero Counts

A count layer fills in every level for every column, so a category no
subject fell into gets a real row rather than a gap. What that row
should *say* is a convention. `zero_count_display` offers three answers:

``` r

zero_spec <- function(z) {
  tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count("RACE",
        settings = layer_settings(
          zero_count_display = z,
          format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
        )
      )
    )
  )
}

show_table(tplyr_build(zero_spec("full"), tplyr_adsl),
           caption = 'zero_count_display = "full" (the default)')
```

| rowlabel1                        | res1       | res2       | res3       |
|:---------------------------------|:-----------|:-----------|:-----------|
| WHITE                            | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |
| BLACK OR AFRICAN AMERICAN        |  8 ( 9.3%) |  9 (10.7%) |  6 ( 7.1%) |
| AMERICAN INDIAN OR ALASKA NATIVE |  0 ( 0.0%) |  1 ( 1.2%) |  0 ( 0.0%) |

zero_count_display = “full” (the default) {.table}

``` r

show_table(tplyr_build(zero_spec("count_only"), tplyr_adsl),
           caption = 'zero_count_display = "count_only"')
```

| rowlabel1                        | res1       | res2       | res3       |
|:---------------------------------|:-----------|:-----------|:-----------|
| WHITE                            | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |
| BLACK OR AFRICAN AMERICAN        |  8 ( 9.3%) |  9 (10.7%) |  6 ( 7.1%) |
| AMERICAN INDIAN OR ALASKA NATIVE |  0         |  1 ( 1.2%) |  0         |

zero_count_display = “count_only” {.table}

``` r

show_table(tplyr_build(zero_spec("blank"), tplyr_adsl),
           caption = 'zero_count_display = "blank"')
```

| rowlabel1                        | res1       | res2       | res3       |
|:---------------------------------|:-----------|:-----------|:-----------|
| WHITE                            | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |
| BLACK OR AFRICAN AMERICAN        |  8 ( 9.3%) |  9 (10.7%) |  6 ( 7.1%) |
| AMERICAN INDIAN OR ALASKA NATIVE |            |  1 ( 1.2%) |            |

zero_count_display = “blank” {.table}

`"full"` leaves the cell alone – a full-width `` ` 0 ( 0.0%)` ``,
leading pad space and all. `"count_only"` keeps the count field and
drops everything after it, so the digit still sits under the counts
above it. `"blank"` empties the cell.

The two suppressing modes are not interchangeable. `"count_only"`
asserts *we looked and found none*; `"blank"` asserts nothing at all,
which is why it is usually reserved for cells that are structurally
inapplicable rather than merely empty.

Both shorten the string rather than padding it out, which matters if
anything downstream assumes a fixed cell width:

``` r

nchar(tplyr_build(zero_spec("full"), tplyr_adsl)$res1)
#> [1] 10 10 10
nchar(tplyr_build(zero_spec("count_only"), tplyr_adsl)$res1)
#> [1] 10 10  2
nchar(tplyr_build(zero_spec("blank"), tplyr_adsl)$res1)
#> [1] 10 10  0
```

`"blank"` produces a genuinely zero-length string, not spaces – so
[`nzchar()`](https://rdrr.io/r/base/nchar.html) and
[`trimws()`](https://rdrr.io/r/base/trimws.html) checks behave as you
would want, but a monospace renderer that relies on the string’s own
width needs `"count_only"` instead.

The setting keys off the first count statistic in the format string –
`n` or `distinct_n` – so on a subject-level AE table it triggers when no
*subject* reported the term, which is the intent. A format string that
references neither leaves the setting inert. It applies to shift layers
too.

## Showing Only Selected Levels

A flag variable summarized as a count layer produces a row for every
level, including the `"N"` one nobody wants to print. `keep_levels`
restricts the output to the levels you name:

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE",
      settings = layer_settings(
        keep_levels = c("WHITE", "BLACK OR AFRICAN AMERICAN"),
        format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
      )
    )
  )
)

show_table(tplyr_build(spec, tplyr_adsl))
```

| rowlabel1                 | res1       | res2       | res3       |
|:--------------------------|:-----------|:-----------|:-----------|
| WHITE                     | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |
| BLACK OR AFRICAN AMERICAN |  8 ( 9.3%) |  9 (10.7%) |  6 ( 7.1%) |

**`keep_levels` filters rows after the denominators are computed, so the
percentages do not re-base.** The dropped level’s subjects are still in
every denominator: in the Xanomeline High Dose arm the two surviving
rows sum to 98.8%, not 100%, because the one American Indian or Alaska
Native subject is still counted below the line.

That is usually the correct behavior – for a flag variable, `n (%)` of
“Yes” should be out of the whole arm. When you do want the kept levels
to sum to 100%, remove the dropped subjects from the denominator too
with `denom_where` or `denom_ignore`; see
[`vignette("denom")`](https://github.com/mstackhouse/tplyr2/articles/denom.md).

## Missing and Total Rows

A `Missing` row and a `Total` row are display elements as much as
statistics – their presence, label, and position are shell decisions.

`total_row = TRUE` appends a total, with `total_row_label` naming it.
`total_row_count_missings` (default `TRUE`) decides whether missing
values are included in that total. `missing_count` adds the missing row,
keyed by `label`:

``` r

dat <- data.frame(
  TRT  = rep(c("Placebo", "Drug"), each = 20),
  RESP = c(rep("CR", 6), rep("PR", 5), rep("SD", 7), NA, NA,
           rep("CR", 9), rep("PR", 4), rep("SD", 6), NA)
)

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_count("RESP",
      settings = layer_settings(
        missing_count   = list(label = "Not evaluable"),
        total_row       = TRUE,
        total_row_label = "Total evaluated",
        total_row_count_missings = FALSE,
        format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
      )
    )
  )
)

show_table(tplyr_build(spec, dat))
```

| rowlabel1       | res1       | res2       |
|:----------------|:-----------|:-----------|
| CR              |  9 (45.0%) |  6 (30.0%) |
| PR              |  4 (20.0%) |  5 (25.0%) |
| SD              |  6 (30.0%) |  7 (35.0%) |
| Not evaluable   |  1 ( 5.0%) |  2 (10.0%) |
| Total evaluated | 19 (95.0%) | 18 (90.0%) |

The missing row is always emitted once `missing_count` is set, and is
zero-filled across every column and `by` group, so its presence does not
depend on whether a particular arm happens to have gaps. `sort_value`
controls where it lands relative to the category rows; it defaults to
`Inf`, which puts it last.

`missing_values` handles the other kind of missing: sentinel codes such
as `"UNK"` or `"NOT DONE"` that are present in the data but mean *no
result*. Naming them folds their records into the missing row **and**
removes their own category row, so nothing is counted twice:

``` r

dat$RESP[c(3, 25)] <- "UNK"

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_count("RESP",
      settings = layer_settings(
        missing_count = list(label = "Not evaluable", missing_values = "UNK"),
        format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
      )
    )
  )
)

show_table(tplyr_build(spec, dat))
```

| rowlabel1     | res1       | res2       |
|:--------------|:-----------|:-----------|
| CR            |  8 (40.0%) |  5 (25.0%) |
| PR            |  4 (20.0%) |  5 (25.0%) |
| SD            |  6 (30.0%) |  7 (35.0%) |
| Not evaluable |  2 (10.0%) |  3 (15.0%) |

There is no `UNK` row, and “Not evaluable” carries both the `NA` records
and the `UNK` ones. On a nested layer, naming an outer-level value
removes its inner rows along with it.

## One Statistic Per Column

Everything so far packs several numbers into one cell. Some shells want
them separated: an `n (%)` column and an `E` column under each treatment
arm, or a demographics block laid out with the statistics running across
the page.

### Count layers: `stat_columns`

Replace `format_strings` with `stat_columns`, a named list of format
strings. Each entry becomes its own result column within every treatment
group, and the entry name becomes the column’s sub-label:

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count("AEBODSYS",
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
show_table(head(as_display(result), 6))
```

| rowlabel1 | res1 | res2 | res3 | res4 | res5 | res6 |
|:---|:---|:---|:---|:---|:---|:---|
| CARDIAC DISORDERS |   4 ( 4.7%) |   5 |   6 ( 7.1%) |   6 |   5 ( 6.0%) |   6 |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS |   0 ( 0.0%) |   0 |   1 ( 1.2%) |   1 |   0 ( 0.0%) |   0 |
| GASTROINTESTINAL DISORDERS |   6 ( 7.0%) |   6 |   4 ( 4.8%) |   6 |   3 ( 3.6%) |   3 |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS |   9 (10.5%) |  11 |  15 (17.9%) |  21 |  18 (21.4%) |  21 |
| IMMUNE SYSTEM DISORDERS |   0 ( 0.0%) |   0 |   0 ( 0.0%) |   0 |   1 ( 1.2%) |   1 |
| INFECTIONS AND INFESTATIONS |   5 ( 5.8%) |   5 |   4 ( 4.8%) |   4 |   3 ( 3.6%) |   3 |

Columns interleave arm-major, so each arm’s statistics sit adjacent.
Which column is which lives on the label attributes, in the form
`"<arm> (N=n) | <stat>"`:

``` r

vapply(grep("^res", names(result), value = TRUE),
       function(nm) attr(result[[nm]], "label"), character(1))
#>                                  res1                                  res2 
#>              "Placebo (N=86) | n (%)"                  "Placebo (N=86) | E" 
#>                                  res3                                  res4 
#> "Xanomeline High Dose (N=84) | n (%)"     "Xanomeline High Dose (N=84) | E" 
#>                                  res5                                  res6 
#>  "Xanomeline Low Dose (N=84) | n (%)"      "Xanomeline Low Dose (N=84) | E"
```

A renderer splits those on `" | "` to build a two-level header spanning
each arm over its sub-columns. Stat names may not themselves contain
`" | "` or `"(N="`, which the label grammar reserves. See
[`vignette("count")`](https://github.com/mstackhouse/tplyr2/articles/count.md)
for more.

### Descriptive layers: `stats_as_columns`

The desc-layer equivalent is a logical switch,
`stats_as_columns = TRUE`. The statistics named in `format_strings`
become columns instead of rows, so the `by` groups run down the page:

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      by = "AGEGR1",
      settings = layer_settings(
        stats_as_columns = TRUE,
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Min, Max"  = f_str("xx, xx", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
show_table(result)
```

| rowlabel1 | res1 | res2 | res3 | res4 | res5 | res6 | res7 | res8 | res9 |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| \<65 | 14 | 61.1 ( 3.51) | 52, 64 | 11 | 59.1 ( 2.91) | 56, 63 |  8 | 57.1 ( 3.72) | 51, 62 |
| 65-80 | 42 | 73.6 ( 4.17) | 65, 80 | 55 | 74.5 ( 4.19) | 65, 80 | 47 | 74.0 ( 4.34) | 65, 80 |
| \>80 | 30 | 84.0 ( 2.51) | 81, 89 | 18 | 83.4 ( 2.12) | 81, 88 | 29 | 83.4 ( 2.08) | 81, 88 |

One row per age group, and one column per arm-by-statistic, labelled
with the same `"<arm> (N=n) | <stat>"` grammar as `stat_columns`:

``` r

vapply(grep("^res", names(result), value = TRUE),
       function(nm) attr(result[[nm]], "label"), character(1))
#>                                      res1 
#>                      "Placebo (N=86) | n" 
#>                                      res2 
#>              "Placebo (N=86) | Mean (SD)" 
#>                                      res3 
#>               "Placebo (N=86) | Min, Max" 
#>                                      res4 
#>         "Xanomeline High Dose (N=84) | n" 
#>                                      res5 
#> "Xanomeline High Dose (N=84) | Mean (SD)" 
#>                                      res6 
#>  "Xanomeline High Dose (N=84) | Min, Max" 
#>                                      res7 
#>          "Xanomeline Low Dose (N=84) | n" 
#>                                      res8 
#>  "Xanomeline Low Dose (N=84) | Mean (SD)" 
#>                                      res9 
#>   "Xanomeline Low Dose (N=84) | Min, Max"
```

This is the layout for a summary that reads across – a lab panel by
visit, an exposure table by duration category – where the row dimension
is the grouping and the statistics belong in the header.

Note that `stats_as_columns` produces a *different* shape when there is
no `by` variable: the treatment groups become the rows and the result
columns are named for the statistics directly rather than `res1`,
`res2`, …, in the order the format strings declared them.
[`as_display()`](https://github.com/mstackhouse/tplyr2/reference/as_display.md)
handles that shape – it drops the internal ordering columns and keeps
everything else – but the `"<arm> | <stat>"` label grammar does not
apply, since the column names *are* the statistics. Reach for this form
when you want a compact arm-by-statistic grid; for a two-level header,
give the layer a `by`.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        stats_as_columns = TRUE,
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Min, Max"  = f_str("xx, xx", "min", "max")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, tplyr_adsl))
```

| rowlabel1                   | n   | Mean (SD)    | Min, Max |
|:----------------------------|:----|:-------------|:---------|
| Placebo (N=86)              | 86  | 75.2 ( 8.59) | 52, 89   |
| Xanomeline High Dose (N=84) | 84  | 74.4 ( 7.89) | 56, 88   |
| Xanomeline Low Dose (N=84)  | 84  | 75.7 ( 8.29) | 51, 88   |

## Shift Table Denominators

A shift table cross-tabulates baseline against post-baseline, so the
percent in each cell can mean several different things depending on what
sits underneath it. Which one the table wants is a convention.

`shift_denom = "total"`, the default, divides by the whole treatment arm
– every cell is a share of all subjects, and the matrix sums to 100% per
arm:

``` r

set.seed(99)
n <- 120
shift_data <- data.frame(
  USUBJID = sprintf("S%03d", seq_len(n)),
  TRTA    = rep(c("Placebo", "Drug"), each = n / 2),
  BNRIND  = factor(sample(c("LOW", "NORMAL", "HIGH"), n, TRUE, c(.15, .7, .15)),
                   levels = c("LOW", "NORMAL", "HIGH")),
  ANRIND  = factor(sample(c("LOW", "NORMAL", "HIGH"), n, TRUE, c(.2, .6, .2)),
                   levels = c("LOW", "NORMAL", "HIGH"))
)

shift_spec <- function(...) {
  tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        settings = layer_settings(
          distinct_by = "USUBJID", ...,
          format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
        )
      )
    )
  )
}

show_table(tplyr_build(shift_spec(), shift_data),
           caption = 'shift_denom = "total" (the default)')
```

| rowlabel1 | res1       | res2       | res3       | res4       | res5       | res6       |
|:----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|
| LOW       |  0 ( 0.0%) |  2 ( 3.3%) |  3 ( 5.0%) |  1 ( 1.7%) |  6 (10.0%) |  2 ( 3.3%) |
| NORMAL    |  5 ( 8.3%) | 32 (53.3%) | 10 (16.7%) | 11 (18.3%) | 24 (40.0%) |  8 (13.3%) |
| HIGH      |  2 ( 3.3%) |  3 ( 5.0%) |  3 ( 5.0%) |  0 ( 0.0%) |  5 ( 8.3%) |  3 ( 5.0%) |

shift_denom = “total” (the default) {.table}

`shift_denom = "column"` divides by the *result column group* instead –
the treatment arm crossed with the post-baseline category. Each result
column then sums to 100%, and a cell reads “of the subjects who ended up
here, this share started there”:

``` r

result <- tplyr_build(shift_spec(shift_denom = "column"), shift_data)
show_table(result, caption = 'shift_denom = "column"')
```

| rowlabel1 | res1       | res2       | res3       | res4       | res5       | res6       |
|:----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|
| LOW       |  0 ( 0.0%) |  2 ( 5.4%) |  3 (18.8%) |  1 ( 8.3%) |  6 (17.1%) |  2 (15.4%) |
| NORMAL    |  5 (71.4%) | 32 (86.5%) | 10 (62.5%) | 11 (91.7%) | 24 (68.6%) |  8 (61.5%) |
| HIGH      |  2 (28.6%) |  3 ( 8.1%) |  3 (18.8%) |  0 ( 0.0%) |  5 (14.3%) |  3 (23.1%) |

shift_denom = “column” {.table}

The `(N=)` values on the column labels change with it – each is now the
count of subjects in that post-baseline category rather than the arm
total:

``` r

vapply(grep("^res", names(result), value = TRUE),
       function(nm) attr(result[[nm]], "label"), character(1))
#>                      res1                      res2                      res3 
#>        "Drug | LOW (N=7)"    "Drug | NORMAL (N=37)"      "Drug | HIGH (N=16)" 
#>                      res4                      res5                      res6 
#>    "Placebo | LOW (N=12)" "Placebo | NORMAL (N=35)"   "Placebo | HIGH (N=13)"
```

The mirror-image convention – each *baseline* group summing to 100%, so
a cell reads “of the subjects who started here, this share ended up
there” – is not a `shift_denom` value. Name the denominator groups
directly with `denoms_by`, listing the column variable(s) and the
baseline row variable:

``` r

show_table(
  tplyr_build(shift_spec(denoms_by = c("TRTA", "BNRIND")), shift_data),
  caption = 'denoms_by = c("TRTA", "BNRIND") -- row-wise percentages'
)
```

| rowlabel1 | res1       | res2       | res3       | res4       | res5       | res6       |
|:----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|
| LOW       |  0 ( 0.0%) |  2 (40.0%) |  3 (60.0%) |  1 (11.1%) |  6 (66.7%) |  2 (22.2%) |
| NORMAL    |  5 (10.6%) | 32 (68.1%) | 10 (21.3%) | 11 (25.6%) | 24 (55.8%) |  8 (18.6%) |
| HIGH      |  2 (25.0%) |  3 (37.5%) |  3 (37.5%) |  0 ( 0.0%) |  5 (62.5%) |  3 (37.5%) |

denoms_by = c(“TRTA”, “BNRIND”) – row-wise percentages {.table}

An explicit `denoms_by` overrides `shift_denom`, and must list every
variable that scopes the denominator (including any `by` variables) or
the groups pool together. See
[`vignette("denom")`](https://github.com/mstackhouse/tplyr2/articles/denom.md).

### The denominator row

Once the percentages are relative to a column group, the table has to
state what each column’s denominator was. `denom_row = TRUE` emits those
counts as an integer row above the baseline rows rather than making you
recompute them. It reports the `shift_denom = "column"` denominator, so
it pairs with that setting rather than with a row-wise `denoms_by`:

``` r

show_table(tplyr_build(
  shift_spec(shift_denom = "column", denom_row = TRUE),
  shift_data
))
```

| rowlabel1 | res1       | res2       | res3       | res4       | res5       | res6       |
|:----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|
| n         |          7 |         37 |         16 |         12 |         35 |         13 |
| LOW       |  0 ( 0.0%) |  2 ( 5.4%) |  3 (18.8%) |  1 ( 8.3%) |  6 (17.1%) |  2 (15.4%) |
| NORMAL    |  5 (71.4%) | 32 (86.5%) | 10 (62.5%) | 11 (91.7%) | 24 (68.6%) |  8 (61.5%) |
| HIGH      |  2 (28.6%) |  3 ( 8.1%) |  3 (18.8%) |  0 ( 0.0%) |  5 (14.3%) |  3 (23.1%) |

`denom_row_label` renames it (the default is `"n"`), and
`denom_row_format` gives it its own `f_str` so the count field can be
sized independently of the shift cells:

``` r

show_table(tplyr_build(
  shift_spec(
    shift_denom     = "column",
    denom_row       = TRUE,
    denom_row_label = "n",
    denom_row_format = f_str("xxx", "n")
  ),
  shift_data
))
```

| rowlabel1 | res1       | res2       | res3       | res4       | res5       | res6       |
|:----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|
| n         |   7        |  37        |  16        |  12        |  35        |  13        |
| LOW       |  0 ( 0.0%) |  2 ( 5.4%) |  3 (18.8%) |  1 ( 8.3%) |  6 (17.1%) |  2 (15.4%) |
| NORMAL    |  5 (71.4%) | 32 (86.5%) | 10 (62.5%) | 11 (91.7%) | 24 (68.6%) |  8 (61.5%) |
| HIGH      |  2 (28.6%) |  3 ( 8.1%) |  3 (18.8%) |  0 ( 0.0%) |  5 (14.3%) |  3 (23.1%) |

Without `denom_row_format` the integer is right-aligned to the **full
width of a formatted shift cell** – ten characters for `xx (xx.x%)` –
which leaves a wide gap in front of a two-digit count.
`denom_row_format` takes a format string with a single numeric field and
sizes the row independently, usually narrower.

## Indenting and Wrapping Nested Terms

An adverse event table is hierarchical – system organ class over
preferred term – and the display convention is a single row-label column
with the preferred terms indented under their class.
[`collapse_row_labels()`](https://github.com/mstackhouse/tplyr2/reference/collapse_row_labels.md)
does the merge:

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        zero_count_display = "count_only",
        format_strings = list(
          n_counts = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2",
                                 nest = TRUE, indent = "   ")
show_table(head(collapsed[, c("row_label", "res1", "res2", "res3")], 10))
```

| row_label                         | res1       | res2       | res3       |
|:----------------------------------|:-----------|:-----------|:-----------|
| CARDIAC DISORDERS                 |  4 ( 4.7%) |  6 ( 7.1%) |  5 ( 6.0%) |
|    ATRIAL FIBRILLATION            |  0         |  0         |  1 ( 1.2%) |
|    ATRIAL FLUTTER                 |  0         |  1 ( 1.2%) |  0         |
|    ATRIAL HYPERTROPHY             |  1 ( 1.2%) |  0         |  0         |
|    BUNDLE BRANCH BLOCK RIGHT      |  1 ( 1.2%) |  0         |  0         |
|    CARDIAC FAILURE CONGESTIVE     |  1 ( 1.2%) |  0         |  0         |
|    MYOCARDIAL INFARCTION          |  0         |  1 ( 1.2%) |  2 ( 2.4%) |
|    SINUS BRADYCARDIA              |  0         |  3 ( 3.6%) |  1 ( 1.2%) |
|    SUPRAVENTRICULAR EXTRASYSTOLES |  1 ( 1.2%) |  0         |  1 ( 1.2%) |
|    SUPRAVENTRICULAR TACHYCARDIA   |  0         |  0         |  1 ( 1.2%) |

Real preferred terms then overflow the column width the shell allows.
[`str_indent_wrap()`](https://github.com/mstackhouse/tplyr2/reference/str_indent_wrap.md)
wraps them to a width while *preserving* the indentation on continuation
lines, and hyphenates any single word at least as long as the width:

``` r

collapsed$row_label <- str_indent_wrap(collapsed$row_label, width = 28)
cat(head(collapsed$row_label, 9), sep = "\n")
#> CARDIAC DISORDERS
#>    ATRIAL FIBRILLATION
#>    ATRIAL FLUTTER
#>    ATRIAL HYPERTROPHY
#>    BUNDLE BRANCH BLOCK
#>    RIGHT
#>    CARDIAC FAILURE
#>    CONGESTIVE
#>    MYOCARDIAL INFARCTION
#>    SINUS BRADYCARDIA
#>    SUPRAVENTRICULAR
#>    EXTRASYSTOLES
```

`BUNDLE BRANCH BLOCK RIGHT` breaks after `BLOCK` and its continuation
line keeps the three-space preferred-term indent, so the hierarchy
survives the wrap. That is why
[`str_indent_wrap()`](https://github.com/mstackhouse/tplyr2/reference/str_indent_wrap.md)
exists rather than plain
[`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html):
it detects the existing indentation and re-applies it to every
continuation line.

One quirk to know when picking `width`: the indent is charged against it
twice, so an indented label wraps at `width` minus its own indent – a
three-space indent at `width = 28` fills only 25 characters. Set `width`
a few characters above the stub width if you want the indented rows to
reach it.

## Conventions a Format String Cannot Express

Everything above is declared in the spec and applied at build time,
which is where you want a display rule: the numeric data behind the cell
stays intact, and the convention travels with the spec through
serialization, metadata, and ARD conversion. For the rules that have no
setting,
[`apply_conditional_format()`](https://github.com/mstackhouse/tplyr2/reference/apply_conditional_format.md)
rewrites a built column based on a number found inside it.

You pick a **format group** – the *n*th number in the string, counting
from the left – give a condition on it using the variable name `x`, and
supply a replacement:

``` r

res <- tplyr_build(zero_spec("full"), tplyr_adsl)

# flag any row where the percentage clears 90%
apply_conditional_format(res$res1, format_group = 2, x > 90,
                         replacement = "(>90%)")
#> [1] "78  (>90%)" " 8 ( 9.3%)" " 0 ( 0.0%)"
```

With `full_string = FALSE` (the default) the replacement refills just
that format group and is padded to preserve the original alignment. With
`full_string = TRUE` it replaces the whole cell:

``` r

apply_conditional_format(res$res1, 2, x == 0, "    -     ", full_string = TRUE)
#> [1] "78 (90.7%)" " 8 ( 9.3%)" "    -     "
```

Because the condition reads the *displayed* number, the same function
works on externally bound statistics and on risk-difference columns.
[`str_extract_num()`](https://github.com/mstackhouse/tplyr2/reference/str_extract_num.md)
pulls a format group out as a number when you want to compute with it
rather than replace it. Both are covered in
[`vignette("post_processing")`](https://github.com/mstackhouse/tplyr2/articles/post_processing.md).

## Getting the Table Out

Two last conventions belong to the hand-off rather than the build.

[`as_display()`](https://github.com/mstackhouse/tplyr2/reference/as_display.md)
drops the internal ordering columns and returns just what belongs on the
page. `labels = TRUE` renames the result columns to their column-group
headers:

``` r

result <- tplyr_build(zero_spec("full"), tplyr_adsl)
kable(as_display(result, labels = TRUE))
```

| rowlabel1 | Placebo (N=86) | Xanomeline High Dose (N=84) | Xanomeline Low Dose (N=84) |
|:---|:---|:---|:---|
| WHITE | 78 (90.7%) | 74 (88.1%) | 78 (92.9%) |
| BLACK OR AFRICAN AMERICAN | 8 ( 9.3%) | 9 (10.7%) | 6 ( 7.1%) |
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%) | 1 ( 1.2%) | 0 ( 0.0%) |

And the padding that every format string in these vignettes produces is
real whitespace, which HTML collapses.
[`replace_leading_whitespace()`](https://github.com/mstackhouse/tplyr2/reference/replace_leading_whitespace.md)
converts leading spaces to non-breaking spaces so indentation and
alignment survive a web or RTF renderer:

``` r

indented <- c("CARDIAC DISORDERS", "   ATRIAL FIBRILLATION")
nchar(indented)
#> [1] 17 22
nchar(replace_leading_whitespace(indented))
#> [1] 17 22
```

The character count is unchanged – each leading space became one
non-breaking space. This vignette’s own tables use the same trick on
their numeric columns, which is why the alignment above is visible at
all.

## Where to Go From Here

- [`vignette("format_strings")`](https://github.com/mstackhouse/tplyr2/articles/format_strings.md)
  – the format string grammar, statistic keywords, rounding, and
  missing-value handling.
- [`vignette("precision_alignment")`](https://github.com/mstackhouse/tplyr2/articles/precision_alignment.md)
  – data-driven widths and parenthesis hugging.
- [`vignette("count")`](https://github.com/mstackhouse/tplyr2/articles/count.md)
  – count layers, including total rows, missing rows, `keep_levels`, and
  nested counts.
- [`vignette("shift")`](https://github.com/mstackhouse/tplyr2/articles/shift.md)
  – shift layers and factor-driven completion.
- [`vignette("denom")`](https://github.com/mstackhouse/tplyr2/articles/denom.md)
  – what the denominator behind every percentage actually is.
- [`vignette("post_processing")`](https://github.com/mstackhouse/tplyr2/articles/post_processing.md)
  – row masks, label collapsing, and the rest of the display helpers.
