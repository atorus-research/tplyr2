# Format Strings

## Introduction

Clinical tables live and die by their alignment. When a reviewer scans a
column of numbers, the decimal points must line up, the parentheses must
sit in the same position from row to row, and the whitespace must be
consistent. One misaligned digit and the table looks unprofessional – or
worse, it raises questions about the numbers themselves.

Every number tplyr2 puts on the page passes through a **format string**:
a compact template that declares how wide each numeric field is, how
many decimal places it carries, and what literal text surrounds it. You
write format strings with
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md),
and the same object drives count cells, descriptive statistics, shift
counts, risk differences, p-values, and standalone formatting of numbers
that never went through a layer at all.

This vignette is the reference for that system: the grammar, where
format strings attach for each layer type, which statistic names are
available, how rounding works, and what happens when a value is missing.
Two companion vignettes build on it:

- [`vignette("precision_alignment")`](https://atorus-research.github.io/tplyr2/articles/precision_alignment.md)
  – letting the *data* set the decimal width (`a`/`A`), and closing the
  gap between a number and its delimiter (`X`/`A`).
- [`vignette("display_conventions")`](https://atorus-research.github.io/tplyr2/articles/display_conventions.md)
  – the regulatory display rules layered on top: `<1%`, zero-count
  suppression, statistics in their own columns.

### Seeing the alignment in this vignette

Format strings pad numbers with spaces, and HTML collapses runs of
spaces inside a table cell. Left alone, every table below would render
with its padding silently squeezed out – hiding the exact thing this
vignette is about. So the examples go through a small helper that
converts those spaces to non-breaking spaces, and the vignette sets its
tables in a monospace font:

``` r

show_table <- function(x, ...) {
  d <- if (any(grepl("^ord_layer", names(x)))) as_display(x) else x
  is_label <- grepl("^rowlabel[0-9]+$|^row_label$", names(d))
  for (j in seq_along(d)) {
    if (!is.character(d[[j]])) next
    d[[j]] <- if (is_label[j]) {
      replace_leading_whitespace(d[[j]])          # keep indentation, allow wrapping
    } else {
      gsub(" ", "\u00a0", d[[j]], fixed = TRUE)   # keep every space in a numeric cell
    }
  }
  kable(d, ...)
}
```

This is not a trick reserved for vignettes – it is the same problem you
hit shipping a tplyr2 table to any HTML or RTF report, which is why
tplyr2 exports
[`replace_leading_whitespace()`](https://atorus-research.github.io/tplyr2/reference/replace_leading_whitespace.md).
Row labels get that treatment, so their indentation survives but long
terms can still wrap; numeric cells get every space converted, so their
alignment is exact. See
[`vignette("post_processing")`](https://atorus-research.github.io/tplyr2/articles/post_processing.md).

[`as_display()`](https://atorus-research.github.io/tplyr2/reference/as_display.md)
does the other half of the work: it strips the internal `ord_layer_*`
bookkeeping columns and hands back just the `rowlabel*`, `res*`,
`rdiff*`, and `pval*` columns that belong on the page.

## Anatomy of a Format String

A format string is a character template made of **format groups**
separated by **literal text**. Each format group corresponds to exactly
one statistic, and the statistics are named as the arguments that follow
the template.

``` r

f_str("xx.x (xx.xx)", "mean", "sd")
#> tplyr format string: "xx.x (xx.xx)"
#>   Variables: mean, sd
```

Breaking that template apart:

| Piece   | Kind         | Meaning                                  |
|:--------|:-------------|:-----------------------------------------|
| `xx.x`  | format group | `mean`: 2 integer positions, 1 decimal   |
| `(`     | literal      | printed verbatim between the two numbers |
| `xx.xx` | format group | `sd`: 2 integer positions, 2 decimals    |
| `)`     | literal      | printed verbatim after the second number |

Literals are anything that is not an `x`, `X`, `a`, or `A`: spaces,
parentheses, brackets, commas, slashes, percent signs, even a leading
`N=`. They are copied into every cell exactly as written.

The number of format groups must match the number of statistic names,
and
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
tells you immediately when it does not:

``` r

f_str("xx.x (xx.xx)", "mean")
#> Error in `f_str()`:
#> ! Format string has 2 format group(s) but 1 variable(s) were provided
```

### Literals cannot contain x, X, a, or A

The parser finds format groups by scanning for runs of `x`, `X`, `a`,
and `A`. It has no way to know that some of those characters were meant
as prose, so **any of those four letters inside intended literal text
silently becomes a format group.** Units and prefixes are where this
bites:

``` r

f_str("xx days", "n")
#> Error in `f_str()`:
#> ! Format string has 2 format group(s) but 1 variable(s) were provided
```

The error names two format groups because the `a` in `days` became one.
Supplying a second variable does not help – it produces a mangled cell
instead:

``` r

apply_formats(f_str("xx.x years", "mean", "sd"), 3.2, 1)
#> [1] " 3.2 ye1rs"
```

`years` was split into the literal `" ye"`, a one-character format group
taking `sd`, and the literal `"rs"`. Nothing warns, and the cell looks
plausible enough to survive a review.

Safe literal characters are anything outside `x`, `X`, `a`, `A` – `(`,
`)`, `[`, `]`, `%`, `,`, `/`, `:`, `=`, `-`, `<`, `>`, digits, spaces,
and every other letter. So `N=xx`, `xx/xx`, `xx (xx.x%)`, and
`xx.x [xx.x]` are all fine, while units like `days`, `max`, `years`, and
`mmax` are not. Put units in the row label or the column header, which
is where a clinical shell wants them anyway:

``` r

apply_formats(f_str("xx.x (xx.xx)", "mean", "sd"), 3.2, 1)   # units live in the label
#> [1] " 3.2 ( 1.00)"
```

One further exception: a `+` immediately after a format group and
followed by digits is the auto-precision offset (see
[`vignette("precision_alignment")`](https://atorus-research.github.io/tplyr2/articles/precision_alignment.md)),
not literal text, so it disappears from the output. A space in front of
it restores the literal:

``` r

apply_formats(f_str("xx+5", "n"), 12)    # "+5" absorbed as an offset
#> [1] "12"
apply_formats(f_str("xx +5", "n"), 12)   # "+5" printed
#> [1] "12 +5"
```

## Field Width and Padding

Each `x` reserves one character position. A number narrower than its
field is **left-padded with spaces** so that digits in the same column
line up across every row of the table.

``` r

apply_formats(f_str("xxx (xxx.x%)", "n", "pct"),
              c(4, 78, 126), c(4.7, 90.7, 100.0))
#> [1] "  4 (  4.7%)" " 78 ( 90.7%)" "126 (100.0%)"
```

All three cells are the same width, and the decimal point and closing
parenthesis land in the same position in each. That is the whole point
of declaring a width rather than letting each number size itself.

Declared widths are **minimums, not maximums**. A number wider than its
field is never truncated – tplyr2 prints it in full and lets that cell
run long, because silently dropping a digit from a clinical table is far
worse than a ragged column. Declare the percent above two digits wide
instead of three and the `100.0` breaks the alignment the other two rows
had:

``` r

apply_formats(f_str("xxx (xx.x%)", "n", "pct"),
              c(4, 78, 126), c(4.7, 90.7, 100.0))
#> [1] "  4 ( 4.7%)"  " 78 (90.7%)"  "126 (100.0%)"
apply_formats(f_str("x", "n"), c(5, 1234))
#> [1] "5"    "1234"
apply_formats(f_str("x.x", "v"), c(5.55, 1234.567))
#> [1] "5.6"    "1234.6"
```

If a column goes ragged, the field was declared too narrow. Widen it, or
let the data choose the width for you with auto-precision (see
[`vignette("precision_alignment")`](https://atorus-research.github.io/tplyr2/articles/precision_alignment.md)).

Negative numbers consume one position for the sign, and a value that
rounds to negative zero is normalized so it never prints as `-0.0`:

``` r

apply_formats(f_str("xx.x", "v"), c(-2.34, 5.6, -0.04))
#> [1] "-2.3" " 5.6" " 0.0"
```

## Where Format Strings Go

Format strings live in
[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md),
but the shape of the `format_strings` argument depends on the layer
type.

### Count and shift layers: the `n_counts` key

A count or shift layer produces one cell per level, so it takes a
**single** format string under the reserved name `n_counts`.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE",
      settings = layer_settings(
        format_strings = list(n_counts = f_str("xxx (xxx.x%)", "n", "pct"))
      )
    )
  )
)

show_table(tplyr_build(spec, tplyr_adsl))
```

| rowlabel1                        | res1         | res2         | res3         |
|:---------------------------------|:-------------|:-------------|:-------------|
| WHITE                            |  78 ( 90.7%) |  74 ( 88.1%) |  78 ( 92.9%) |
| BLACK OR AFRICAN AMERICAN        |   8 (  9.3%) |   9 ( 10.7%) |   6 (  7.1%) |
| AMERICAN INDIAN OR ALASKA NATIVE |   0 (  0.0%) |   1 (  1.2%) |   0 (  0.0%) |

Omit `format_strings` and a count layer defaults to
`f_str("xx (xx.x%)", "n", "pct")`.

A count layer uses exactly **one** format string. It looks for the
`n_counts` key and falls back to the first entry in the list if that key
is absent, so a differently named single entry still works – but any
additional entries are silently ignored. If you want two formats side by
side, you want `stat_columns` (below), not a second `format_strings`
entry.

### Descriptive statistics layers: a named list, one entry per row

A
[`group_desc()`](https://atorus-research.github.io/tplyr2/reference/group_desc.md)
layer produces a *block* of rows, so `format_strings` is a named list.
Each **name** becomes the row label and each **value** is the format
string for that row. List order is row order.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      by = "Age (years)",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"    = f_str("xx.x", "median"),
          "Q1, Q3"    = f_str("xx.x, xx.x", "q1", "q3"),
          "Min, Max"  = f_str("xx, xx", "min", "max"),
          "Missing"   = f_str("xx", "missing")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, tplyr_adsl))
```

| rowlabel1   | rowlabel2 | res1         | res2         | res3         |
|:------------|:----------|:-------------|:-------------|:-------------|
| Age (years) | n         | 86           | 84           | 84           |
| Age (years) | Mean (SD) | 75.2 ( 8.59) | 74.4 ( 7.89) | 75.7 ( 8.29) |
| Age (years) | Median    | 76.0         | 76.0         | 77.5         |
| Age (years) | Q1, Q3    | 69.2, 81.8   | 70.8, 80.0   | 71.0, 82.0   |
| Age (years) | Min, Max  | 52, 89       | 56, 88       | 51, 88       |
| Age (years) | Missing   |  0           |  0           |  0           |

A single row can pull in more than one statistic – `"Mean (SD)"`,
`"Q1, Q3"`, and `"Min, Max"` each use two format groups. Nothing stops
you from packing three or four numbers into one cell when the display
calls for it.

Omit `format_strings` and a desc layer produces those same six rows with
sensible default widths.

### Analyze layers: whatever your function returns

[`group_analyze()`](https://atorus-research.github.io/tplyr2/reference/group_analyze.md)
calls a function you supply, so the statistic names are the column names
of the data frame it returns. See
[`vignette("analyze")`](https://atorus-research.github.io/tplyr2/articles/analyze.md).

### One format string per column: `stat_columns`

When the display wants the subject count and the event count in
*separate columns* rather than packed into one cell, that is
`stat_columns` rather than `format_strings`. See
[`vignette("display_conventions")`](https://atorus-research.github.io/tplyr2/articles/display_conventions.md).

## Statistic Keywords by Layer Type

The names passed to
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
after the template must be statistics the layer actually computes. Here
is the complete set.

### Count and shift layers

| Keyword             | Meaning                                              |
|:--------------------|:-----------------------------------------------------|
| `n`                 | Number of records                                    |
| `pct`               | Percent of records, `n / total * 100`                |
| `total`             | Denominator behind `pct`                             |
| `distinct_n`        | Number of distinct subjects (requires `distinct_by`) |
| `distinct_pct`      | Percent of distinct subjects                         |
| `distinct_total`    | Distinct-subject denominator                         |
| `ci_lower`          | Lower confidence bound on `pct`                      |
| `ci_upper`          | Upper confidence bound on `pct`                      |
| `distinct_ci_lower` | Lower confidence bound on `distinct_pct`             |
| `distinct_ci_upper` | Upper confidence bound on `distinct_pct`             |

`total` and `distinct_total` are ordinary keywords, so an `n/N` display
is just a format string with a slash in it:

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX",
      settings = layer_settings(
        format_strings = list(
          n_counts = f_str("xxx/xxx (xx.x%)", "n", "total", "pct")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, tplyr_adsl))
```

| rowlabel1 | res1            | res2            | res3            |
|:----------|:----------------|:----------------|:----------------|
| F         |  53/ 86 (61.6%) |  40/ 84 (47.6%) |  50/ 84 (59.5%) |
| M         |  33/ 86 (38.4%) |  44/ 84 (52.4%) |  34/ 84 (40.5%) |

What the denominator behind `total` and `pct` actually is – the column
total, a subgroup, a separate population dataset – is the subject of
[`vignette("denom")`](https://atorus-research.github.io/tplyr2/articles/denom.md).

The four confidence-interval keywords are computed only when a format
string references one, and are configured with `ci_method` and
`ci_level`; see
[`vignette("denom")`](https://atorus-research.github.io/tplyr2/articles/denom.md)
and
[`vignette("binding-statistics")`](https://atorus-research.github.io/tplyr2/articles/binding-statistics.md).

### Descriptive statistics layers

| Keyword | Meaning |
|:---|:---|
| `n` | Non-missing count |
| `n_records` | Records assessed – non-missing plus missing |
| `missing` | Count of `NA` values |
| `mean` | Arithmetic mean |
| `sd` | Standard deviation |
| `var` | Variance |
| `median` | Median |
| `q1`, `q3` | First and third quartiles |
| `iqr` | Interquartile range |
| `min`, `max` | Minimum and maximum of the finite values |
| `total` | Denominator for `pct` – the *record* count of the denominator source |
| `pct` | `n / total * 100` |

`n_records`, `total`, and `pct` are easy to overlook, and between them
they answer a question that comes up in nearly every efficacy table:
*how many subjects contributed data, and what share of the arm is that?*
– with one caveat, below.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("BMIBL",
      by = "Baseline BMI (kg/m2)",
      settings = layer_settings(
        format_strings = list(
          "Subjects with data, n (%)" = f_str("xx (xx.x%)", "n", "pct"),
          "Records assessed"          = f_str("xx", "n_records"),
          "Mean (SD)"                 = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Missing"                   = f_str("xx", "missing")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, tplyr_adsl))
```

| rowlabel1 | rowlabel2 | res1 | res2 | res3 |
|:---|:---|:---|:---|:---|
| Baseline BMI (kg/m2) | Subjects with data, n (%) | 86 (100.0%) | 84 (100.0%) | 83 (98.8%) |
| Baseline BMI (kg/m2) | Records assessed | 86 | 84 | 84 |
| Baseline BMI (kg/m2) | Mean (SD) | 23.6 ( 3.67) | 25.3 ( 4.16) | 25.1 ( 4.27) |
| Baseline BMI (kg/m2) | Missing |  0 |  0 |  1 |

The Xanomeline Low Dose arm has one subject with no baseline BMI, and
the four rows pull that fact apart: `n` counts the 83 *non-missing*
analysis values, `n_records` counts all 84 records assessed, `missing`
counts the 1 gap, and `pct` reports the 83 as 98.8% of the arm. A table
that reports “subjects assessed” wants `n_records`; one that reports the
analysis population wants `n`.

**The caveat: `total` is a row count, not a subject count.** It is the
number of *records* in the denominator source – `tplyr_adsl` above,
where one row is one subject, so `pct` really is the share of the arm.
Point the same format string at a BDS dataset with many rows per subject
and `total` counts records, so `pct` becomes the share of *records*
rather than of subjects. On multi-record data, supply a
one-row-per-subject
[`pop_data()`](https://atorus-research.github.io/tplyr2/reference/pop_data.md)
if you want a subject-level percent. `pct` divides `n` by the same
denominator the layer would use for a count, so `denoms_by` and
`denom_where` apply to desc layers as well as count layers – see
[`vignette("denom")`](https://atorus-research.github.io/tplyr2/articles/denom.md).

Any statistic registered through `custom_summaries` is also available by
name – see
[`vignette("desc")`](https://atorus-research.github.io/tplyr2/articles/desc.md).

Referencing a name the layer does not compute is a **warning**, not an
error, because custom summaries can introduce arbitrary names. Read
those warnings: a typo’d keyword yields a column of blanks rather than a
failure.

``` r

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list("Mean" = f_str("xx.x", "average"))
      )
    )
  )
)

show_table(tplyr_build(spec, tplyr_adsl))
#> Warning: Layer 1: format string 'Mean' references variable 'average' which is
#> not a recognized statistic for desc layers
```

| rowlabel1 | res1 | res2 | res3 |
|:----------|:-----|:-----|:-----|
| Mean      |      |      |      |

## Rounding

Format strings round to the declared number of decimals, and *how* they
round is a regulatory question rather than a stylistic one. R’s default
is banker’s rounding (round half to even), so `2.5` becomes `2` and
`4.5` becomes `4`. SAS – and therefore most legacy study output – rounds
half away from zero, so `2.5` becomes `3` and `4.5` becomes `5`.

`tplyr2_options(IBMRounding = TRUE)` switches every format string in the
session to the SAS convention:

``` r

demo <- data.frame(
  TRT = rep(c("Drug", "Placebo"), each = 4),
  VAL = c(1, 2, 3, 4,   3, 4, 5, 6)   # means of exactly 2.5 and 4.5
)

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_desc("VAL",
      settings = layer_settings(
        format_strings = list("Mean" = f_str("xx", "mean"))
      )
    )
  )
)
```

``` r

show_table(tplyr_build(spec, demo), caption = "Banker's rounding (the default)")
```

| rowlabel1 | res1 | res2 |
|:----------|:-----|:-----|
| Mean      |  2   |  4   |

Banker’s rounding (the default) {.table}

``` r

tplyr2_options(IBMRounding = TRUE)
show_table(tplyr_build(spec, demo), caption = "IBM rounding (half away from zero)")
```

| rowlabel1 | res1 | res2 |
|:----------|:-----|:-----|
| Mean      |  3   |  5   |

IBM rounding (half away from zero) {.table}

``` r

tplyr2_options(IBMRounding = FALSE)
```

Both means land exactly on a rounding boundary, so both cells move:
`2.5` renders as `2` then `3`, and `4.5` as `4` then `5`. Negative
values round away from zero too, so `-2.5` becomes `-3`.

The option applies to every rounded value in a build – percentages,
means, quartiles, risk differences – so a table stays internally
consistent either way. Set it once at the top of your script or in
`.Rprofile` rather than per table; see
[`vignette("options")`](https://atorus-research.github.io/tplyr2/articles/options.md).

## Missing Values and the `empty` Argument

When a statistic is `NA`, the default is to fill its field with **spaces
of the same width**. The cell looks blank but keeps its shape, so the
column below it stays aligned. Note that the surrounding literals are
still printed:

``` r

apply_formats(f_str("xx.x", "v"), c(2.3, NA, 12.7))
#> [1] " 2.3" "    " "12.7"
apply_formats(f_str("xx.x (xx.xx)", "mean", "sd"), NA, NA)
#> [1] "     (     )"
```

Many tables want something visible instead – a dash, an `NE`, an `N/A`.
The `empty` argument of
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
supplies it, keyed by the reserved name `.overall`:

``` r

d <- data.frame(
  TRT = c(rep("A", 4), rep("B", 3)),
  VAL = c(1.5, 2.5, 3.5, 4.5, NA, NA, NA)
)

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_desc("VAL",
      settings = layer_settings(
        format_strings = list(
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd", empty = c(.overall = "---")),
          "Median"    = f_str("xx.x", "median", empty = c(.overall = "NE")),
          "SD"        = f_str("xx.xx", "sd", empty = c(.overall = "N/A"))
        )
      )
    )
  )
)

show_table(tplyr_build(spec, d))
```

| rowlabel1 | res1         | res2 |
|:----------|:-------------|:-----|
| Mean (SD) |  3.0 ( 1.29) | —    |
| Median    |  3.0         | NE   |
| SD        |  1.29        | N/A  |

Treatment B is entirely `NA`, so each row shows its own replacement. Two
details matter, and both bite in practice.

**`.overall` fires only when *every* format group in the string is
`NA`.** A row that packs two statistics together and loses only one of
them keeps the surviving number and blank-fills the other. Watch a group
with a single observation, where `mean` is computable but `sd` is not:

``` r

d1 <- data.frame(TRT = c("A", "A", "B"), VAL = c(1.5, 2.5, 7.5))
show_table(tplyr_build(spec, d1))
```

| rowlabel1 | res1         | res2         |
|:----------|:-------------|:-------------|
| Mean (SD) |  2.0 ( 0.71) |  7.5 (     ) |
| Median    |  2.0         |  7.5         |
| SD        |  0.71        | N/A          |

`"Mean (SD)"` renders as `` ` 7.5 ( )` `` – leading pad space included,
since the mean is real and the string is therefore not “overall” empty –
while the standalone `"SD"` row does hit `.overall` and shows `N/A`. If
you need all-or-nothing behavior for one statistic, give it its own row.

**An unnamed `empty` is a different mode, not a mistake.** Drop the
`.overall` name and the value instead fills *each* NA format group in
place, right-justified to the width that group would have occupied. The
cell keeps its width, and a partially missing cell keeps its alignment:

``` r

fmt_overall <- f_str("xx (xxx)", "n", "pct", empty = c(.overall = "NA"))
fmt_fill    <- f_str("xx (xxx)", "n", "pct", empty = "NA")

apply_formats(fmt_overall, NA, NA)   # whole cell replaced
#> [1] "NA"
apply_formats(fmt_fill,    NA, NA)   # each field filled
#> [1] "NA ( NA)"
apply_formats(fmt_fill,    NA, 12)   # only the missing field filled
#> [1] "NA ( 12)"
```

So the two modes answer different questions. `.overall` says *this row
has nothing to report*, and is right for a `Median` or `SD` row that
collapses to `NE`. The unnamed form says *this number is missing*, and
is right when the cell has to stay the same shape as the cells above and
below it. Both match Tplyr v1.

A replacement longer than the field is never truncated – the cell runs
long, the same rule format strings follow for numbers.

## Formatting Numbers Outside a Layer

[`apply_formats()`](https://atorus-research.github.io/tplyr2/reference/apply_formats.md)
is the engine behind every cell, and it is exported so you can point it
at numbers that never went through a layer – a model-based p-value, an
LS-mean from `emmeans`, a hazard ratio. Formatting these through the
same
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
machinery keeps their rounding, width, and alignment identical to the
layer-computed cells beside them, which
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) and
[`format()`](https://rdrr.io/r/base/format.html) cannot promise.

``` r

apply_formats(f_str("xxx.x (xxx.xx)", "mean", "sd"),
              c(75.3, 68.1, 80.5),
              c(8.21, 7.55, 9.03))
#> [1] " 75.3 (  8.21)" " 68.1 (  7.55)" " 80.5 (  9.03)"
```

Arguments map to format groups positionally, in template order. Seven
further arguments handle the cases that come up when the numbers are
external.

`na` replaces cells whose inputs are all `NA` with a string of your
choosing – including a genuinely empty one, which the default
blank-width fill is not. This keeps
[`nzchar()`](https://rdrr.io/r/base/nchar.html) and trimming checks
downstream honest:

``` r

apply_formats(f_str("xx.x", "v"), c(2.3, NA, 12.7))              # width-preserving blank
#> [1] " 2.3" "    " "12.7"
apply_formats(f_str("xx.x", "v"), c(2.3, NA, 12.7), na = "")     # nchar 0
#> [1] " 2.3" ""     "12.7"
apply_formats(f_str("xx.x", "v"), c(2.3, NA, 12.7), na = "NE")   # a sentinel
#> [1] " 2.3" "NE"   "12.7"
```

`width` and `pad` pad the whole token to a fixed total width, for
monospace output where the renderer does not own alignment. When `na`
applies to a cell, `na` wins and that cell is not padded:

``` r

apply_formats(f_str("xx.x", "v"), c(2.3, NA, 12.7), width = 8)
#> [1] " 2.3    " "        " "12.7    "
apply_formats(f_str("xx.x", "v"), c(2.3, NA, 12.7), width = 8, pad = "left")
#> [1] "     2.3" "        " "    12.7"
```

`lt`, `gt`, and `lt_gt_group` implement the `<1%` / `>99%` convention on
a chosen format group – group 2 here being the percent. The threshold
token is rendered at that group’s declared decimal width, so an `xx.x`
percent field produces `<1.0%`, not `<1%`; declare the field as `xx` if
you want the bare form:

``` r

apply_formats(f_str("xx (xx.x%)", "n", "pct"),
              c(1, 40, 253), c(0.4, 47.1, 99.6),
              lt = 1, gt = 99, lt_gt_group = 2)
#> [1] " 1 (<1.0%)"   "40 (47.1%)"   "253 (>99.0%)"
```

Inside a count layer you would reach for the `pct_lt` and `pct_gt`
settings instead, which drive exactly this machinery; see
[`vignette("display_conventions")`](https://atorus-research.github.io/tplyr2/articles/display_conventions.md).

The `precision` argument accepts resolved auto-precision widths; see
[`vignette("precision_alignment")`](https://atorus-research.github.io/tplyr2/articles/precision_alignment.md).

A full worked example of binding externally computed statistics onto a
built table lives in
[`vignette("binding-statistics")`](https://atorus-research.github.io/tplyr2/articles/binding-statistics.md).

## Where to Go From Here

- [`vignette("precision_alignment")`](https://atorus-research.github.io/tplyr2/articles/precision_alignment.md)
  – data-driven decimal widths with `a`/`A`, and parenthesis hugging
  with `X`/`A`.
- [`vignette("display_conventions")`](https://atorus-research.github.io/tplyr2/articles/display_conventions.md)
  – `<1%` and `>99%` percents, zero-count suppression, statistics as
  separate columns, denominator rows.
- [`vignette("post_processing")`](https://atorus-research.github.io/tplyr2/articles/post_processing.md)
  – reformatting, indenting, and wrapping a built table on its way to a
  renderer.
- [`vignette("options")`](https://atorus-research.github.io/tplyr2/articles/options.md)
  – session options including `IBMRounding` and `quantile_type`.
