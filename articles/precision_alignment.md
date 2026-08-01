# Precision and Alignment

## Introduction

[`vignette("format_strings")`](https://atorus-research.github.io/tplyr2/articles/format_strings.md)
covers format strings whose widths you declare yourself: `xx.x` is
always two integer positions and one decimal. That works when you know
the scale of the data in advance. Two situations break it.

The first is a table that covers **variables of different scales**. A
lab listing might report sodium in whole numbers, ALT to one decimal,
and creatinine to two. No single hardcoded format string serves all
three: `xxx.x` prints sodium’s minimum as `132.0`, inventing a decimal
the assay never measured, while rendering creatinine as `0.9` and
dropping a digit the SAP requires. Writing a separate layer per
parameter with hand-tuned widths is tedious and goes stale the moment
the data changes.

The second is **ragged delimiters**. Padding a number to a fixed width
means the opening parenthesis can end up a space or two away from the
digit it encloses, which many sponsor mocks disallow:

     14 ( 16.3%)
      7 (  8.1%)

tplyr2 answers the first with **auto-precision** (`a` and `A`), which
lets the data set the width at build time, and the second with
**parenthesis hugging** (`X` and `A`), which relocates padding without
changing the total width. This vignette covers both, and the case where
you want them together.

## Auto-Precision

Lowercase `a` behaves exactly like `x`, except the width is not read
from the template – it is measured from the data during the build.

At build time tplyr2 scans a variable and records two numbers per
precision group:

- `max_int` – the largest number of integer digits present
- `max_dec` – the largest number of *meaningful* decimal places present,
  up to base R’s seven-significant-digit display limit. Integer
  magnitude eats into it: a value of `140.123456` measures as four
  decimals, not six, while `0.123456` measures as six.

An `a` on the integer side resolves to `max_int`, and an `a` on the
decimal side resolves to `max_dec`. Four
[`layer_settings()`](https://atorus-research.github.io/tplyr2/reference/layer_settings.md)
arguments control how those widths are resolved – the first three shape
the scan, and the fourth replaces it:

| Setting | Purpose |
|:---|:---|
| `precision_on` | The variable to measure. Defaults to the target variable. |
| `precision_by` | Grouping variables; each group gets its own widths. |
| `precision_cap` | Upper bounds on the measured widths. |
| `precision_data` | Supply widths from outside instead of measuring. |

### The `+N` offset

Clinical convention rarely displays a summary statistic at the raw
precision of the data. A mean usually carries one more decimal than the
source values and a standard deviation two more. The `+N` suffix
expresses that directly: `a+1` means “the measured width, plus one”.

Here is a three-parameter lab panel recorded at three different
precisions – sodium in whole numbers, ALT to one decimal, creatinine to
two:

``` r

set.seed(42)
n_subj <- 30
params <- data.frame(
  PARAM  = c("Sodium (mmol/L)", "Alanine Aminotransferase (U/L)", "Creatinine (mg/dL)"),
  mu     = c(140,  28,   0.9),
  sigma  = c(  3,   9,   0.15),
  digits = c(  0,   1,   2)      # the precision each assay reports
)

labs <- do.call(rbind, lapply(seq_len(nrow(params)), function(i) {
  base <- round(rnorm(n_subj, params$mu[i], params$sigma[i]), params$digits[i])
  post <- round(base + rnorm(n_subj, 0, params$sigma[i] / 2), params$digits[i])
  data.frame(
    USUBJID = sprintf("S%03d", seq_len(n_subj)),
    TRTA    = rep(c("Placebo", "Drug"), each = n_subj / 2),
    PARAM   = params$PARAM[i],
    AVAL    = post,
    BASE    = base,
    CHG     = post - base
  )
}))
```

(The built-in `tplyr_adlb` carries a single parameter, so this vignette
uses a synthetic panel to show precision varying across parameters.)

One layer, one set of format strings, three different resolved
precisions:

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAM",
      settings = layer_settings(
        precision_by = "PARAM",
        precision_on = "AVAL",
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"),
          "Median"    = f_str("a.a+1", "median"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, labs))
```

| rowlabel1                      | rowlabel2 | res1           | res2           |
|:-------------------------------|:----------|:---------------|:---------------|
| Alanine Aminotransferase (U/L) | n         | 15             | 15             |
| Creatinine (mg/dL)             | n         | 15             | 15             |
| Sodium (mmol/L)                | n         | 15             | 15             |
| Alanine Aminotransferase (U/L) | Mean (SD) | 28.76 ( 8.244) | 30.49 ( 7.571) |
| Creatinine (mg/dL)             | Mean (SD) | 0.890 (0.1272) | 0.833 (0.1852) |
| Sodium (mmol/L)                | Mean (SD) | 139.1 (  4.33) | 141.0 (  2.98) |
| Alanine Aminotransferase (U/L) | Median    | 30.80          | 30.70          |
| Creatinine (mg/dL)             | Median    | 0.870          | 0.810          |
| Sodium (mmol/L)                | Median    | 139.0          | 141.0          |
| Alanine Aminotransferase (U/L) | Min, Max  |  9.8, 41.5     | 16.5, 46.9     |
| Creatinine (mg/dL)             | Min, Max  | 0.70, 1.11     | 0.54, 1.16     |
| Sodium (mmol/L)                | Min, Max  | 132, 146       | 137, 146       |

Read down the `Mean (SD)` rows and the precision tracks the source data.
Taking the Drug column (`res1`):

| Parameter  | `max_dec` | `a.a+1` (mean) | `a.a+2` (SD) | `a.a` (min, max) |
|:-----------|:---------:|:---------------|:-------------|:-----------------|
| Sodium     |     0     | `139.1`        | `4.33`       | `132, 146`       |
| ALT        |     1     | `28.76`        | `8.244`      | `9.8, 41.5`      |
| Creatinine |     2     | `0.890`        | `0.1272`     | `0.70, 1.11`     |

`Min, Max` uses bare `a.a`, so it reports the raw precision of the data
with no extra digits – exactly right for a minimum and maximum, which
are observed values rather than derived ones.

That is the payoff: a single layer specification handles a panel of any
size, and each parameter renders at the precision it was actually
measured at. Add a parameter to the dataset and the table adapts without
a spec change.

Note that `n` uses `xx` rather than `a`. A count has no meaningful
decimal precision, so declaring its width outright is clearer.

### `precision_on`: measuring a different variable

`precision_on` defaults to the target variable, and the two come apart
whenever you summarize a *derived* value. Change from baseline is the
standard case. A lab table stacks a value block on top of a change
block, and the two need to line up – but `CHG` has a smaller range than
`AVAL`, so measuring it produces a narrower field.

Here are both blocks, with the change layer left to measure itself:

``` r

value_block <- group_desc("AVAL",
  by = c(label("Value at Week 8"), "PARAM"),
  settings = layer_settings(
    precision_by = "PARAM",
    precision_on = "AVAL",
    format_strings = list("Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"))
  )
)

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    value_block,
    group_desc("CHG",
      by = c(label("Change from Baseline"), "PARAM"),
      settings = layer_settings(
        precision_by = "PARAM",        # no precision_on: measures CHG
        format_strings = list("Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"))
      )
    )
  )
)

show_table(tplyr_build(spec, labs))
```

| rowlabel1 | rowlabel2 | rowlabel3 | res1 | res2 |
|:---|:---|:---|:---|:---|
| Value at Week 8 | Alanine Aminotransferase (U/L) | Mean (SD) | 28.76 ( 8.244) | 30.49 ( 7.571) |
| Value at Week 8 | Creatinine (mg/dL) | Mean (SD) | 0.890 (0.1272) | 0.833 (0.1852) |
| Value at Week 8 | Sodium (mmol/L) | Mean (SD) | 139.1 (  4.33) | 141.0 (  2.98) |
| Change from Baseline | Alanine Aminotransferase (U/L) | Mean (SD) | -0.64 ( 4.311) |  0.47 ( 5.047) |
| Change from Baseline | Creatinine (mg/dL) | Mean (SD) | 0.015 (0.0743) | -0.017 (0.0614) |
| Change from Baseline | Sodium (mmol/L) | Mean (SD) | 0.1 (1.49) | -0.5 (1.64) |

The sodium change cell reads `0.1 (1.49)`, which does not sit under the
`139.1 ( 4.33)` above it – `CHG` needs one integer digit where `AVAL`
needs three. Pointing `precision_on` at `AVAL` gives the change block
the value block’s widths:

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    value_block,
    group_desc("CHG",
      by = c(label("Change from Baseline"), "PARAM"),
      settings = layer_settings(
        precision_by = "PARAM",
        precision_on = "AVAL",         # measure AVAL, summarize CHG
        format_strings = list("Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"))
      )
    )
  )
)

show_table(tplyr_build(spec, labs))
```

| rowlabel1 | rowlabel2 | rowlabel3 | res1 | res2 |
|:---|:---|:---|:---|:---|
| Value at Week 8 | Alanine Aminotransferase (U/L) | Mean (SD) | 28.76 ( 8.244) | 30.49 ( 7.571) |
| Value at Week 8 | Creatinine (mg/dL) | Mean (SD) | 0.890 (0.1272) | 0.833 (0.1852) |
| Value at Week 8 | Sodium (mmol/L) | Mean (SD) | 139.1 (  4.33) | 141.0 (  2.98) |
| Change from Baseline | Alanine Aminotransferase (U/L) | Mean (SD) | -0.64 ( 4.311) |  0.47 ( 5.047) |
| Change from Baseline | Creatinine (mg/dL) | Mean (SD) | 0.015 (0.0743) | -0.017 (0.0614) |
| Change from Baseline | Sodium (mmol/L) | Mean (SD) |   0.1 (  1.49) |  -0.5 (  1.64) |

Sodium’s change cell is now `0.1 ( 1.49)` and the decimal points align
down the column. The same argument handles the mirror-image case:
summarizing a rounded derived variable whose arithmetic has introduced
floating-point noise, where measuring the source keeps the display at
the precision the lab actually reported.

### `precision_by`: one width per group

`precision_by` defines the groups the scan runs within. In the example
above it is `PARAM`, so each parameter gets its own widths. Drop it and
tplyr2 measures the whole column at once, which means the widest
parameter dictates the format for every parameter:

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAM",
      settings = layer_settings(
        precision_on = "AVAL",         # no precision_by
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, labs))
```

| rowlabel1 | rowlabel2 | res1 | res2 |
|:---|:---|:---|:---|
| Alanine Aminotransferase (U/L) | Mean (SD) |  28.760 (  8.2437) |  30.493 (  7.5710) |
| Creatinine (mg/dL) | Mean (SD) |   0.890 (  0.1272) |   0.833 (  0.1852) |
| Sodium (mmol/L) | Mean (SD) | 139.067 (  4.3337) | 141.000 (  2.9761) |

Sodium now shows three decimals it never measured, because creatinine’s
two-decimal precision set the width for the whole layer. Whenever a
layer spans variables of different scales, `precision_by` should name
the variable that distinguishes them.

## Capping Precision

Auto-precision follows the data, and data is not always well behaved. A
single value carried to more decimals than the rest – a unit conversion
artifact, an imputed value – drags the whole column several digits wider
than the assay warrants. `precision_cap` sets an upper bound:

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAM",
      settings = layer_settings(
        precision_by  = "PARAM",
        precision_on  = "AVAL",
        precision_cap = c(int = 3, dec = 1),
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, labs))
```

| rowlabel1                      | rowlabel2 | res1           | res2           |
|:-------------------------------|:----------|:---------------|:---------------|
| Alanine Aminotransferase (U/L) | Mean (SD) | 28.76 ( 8.244) | 30.49 ( 7.571) |
| Creatinine (mg/dL)             | Mean (SD) | 0.89 (0.127)   | 0.83 (0.185)   |
| Sodium (mmol/L)                | Mean (SD) | 139.1 (  4.33) | 141.0 (  2.98) |
| Alanine Aminotransferase (U/L) | Min, Max  |  9.8, 41.5     | 16.5, 46.9     |
| Creatinine (mg/dL)             | Min, Max  | 0.7, 1.1       | 0.5, 1.2       |
| Sodium (mmol/L)                | Min, Max  | 132, 146       | 137, 146       |

**The cap applies to the measured width, before `+N` is added.** With
`dec = 1`, creatinine’s measured 2 decimals is capped to 1, so `a.a+1`
renders two decimals (`0.89`) rather than the three it did above
(`0.890`), and `a.a` renders one (`0.7, 1.1`). ALT was already at 1
decimal, so the cap does not touch it and `a.a+1` still gives `28.76`.
Getting this ordering backwards is the most common surprise with
capping: a cap of `dec = 1` does not guarantee one decimal in the
output, it guarantees one decimal *before your offsets*.

Either component may be given alone – `c(dec = 2)` caps decimals and
leaves integer width uncapped.

`tplyr2_options(precision_cap = ...)` sets a session-wide default, and a
layer-level `precision_cap` overrides it. That combination is the useful
one: a conservative session default protects every table in the study,
and the occasional layer that genuinely needs more digits asks for them
explicitly. See
[`vignette("options")`](https://atorus-research.github.io/tplyr2/articles/options.md).

## Supplying Precision From Outside

When the statistical analysis plan fixes the display precision per
parameter, you do not want it inferred from whatever happens to be in
the extract. `precision_data` takes a data frame with a `max_int`
column, a `max_dec` column, and the `precision_by` variables:

``` r

sap_precision <- data.frame(
  PARAM = c("Sodium (mmol/L)",
            "Alanine Aminotransferase (U/L)",
            "Creatinine (mg/dL)"),
  max_int = c(3L, 3L, 1L),
  max_dec = c(0L, 1L, 2L)
)

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAM",
      settings = layer_settings(
        precision_by   = "PARAM",
        precision_on   = "AVAL",
        precision_data = sap_precision,
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, labs))
```

| rowlabel1                      | rowlabel2 | res1             | res2             |
|:-------------------------------|:----------|:-----------------|:-----------------|
| Alanine Aminotransferase (U/L) | Mean (SD) |  28.76 (  8.244) |  30.49 (  7.571) |
| Creatinine (mg/dL)             | Mean (SD) | 0.890 (0.1272)   | 0.833 (0.1852)   |
| Sodium (mmol/L)                | Mean (SD) | 139.1 (  4.33)   | 141.0 (  2.98)   |

The `+N` offsets still apply on top of the supplied widths, so the SAP
specifies the data precision once and the display convention stays in
the format string.

Two things to know. **Coverage is checked, but only with a warning.** A
group with no row in `precision_data` has no widths to resolve and
renders as a blank cell; the build warns and names the groups it could
not cover. A `precision_data` that omits the `precision_by` columns
entirely is also accepted, with a warning, and its widths are applied to
every group. Both are warnings rather than errors because a deliberately
partial table is legitimate – but read them, because a blank cell is the
alternative.

**Nothing tells you when the data moves.** That is the point of pinning
precision to the SAP, but it means the table keeps rendering at the
declared widths even if the extract arrives at a different precision –
so a genuine upstream change in the assay looks like nothing at all.

### Auto-precision is a descriptive-statistics feature

One scope limit is worth stating plainly, because it is easy to assume
otherwise: **the precision scan runs only for
[`group_desc()`](https://atorus-research.github.io/tplyr2/reference/group_desc.md)
layers.** Count, shift, and analyze layers never resolve `a` against the
data. In those layers an `a` degrades silently to a fixed width equal to
the number of characters you wrote, exactly as if you had typed `x`:

``` r

counts <- data.frame(TRT = "A", GROUP = c(rep("Common", 250), rep("Rare", 3)))

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_count("GROUP",
      settings = layer_settings(
        format_strings = list(n_counts = f_str("a (xx.x%)", "n", "pct"))
      )
    )
  )
)

c(as_display(tplyr_build(spec, counts))$res1)
#> [1] "250 (98.8%)" "3 ( 1.2%)"
```

A single `a` gave a field of width 1, so `250` overflows and `3` is
unpadded – not the data-driven three-digit field the syntax suggests.
Declare count widths with `x` and size them to the largest plausible
count.

## Parenthesis Hugging

Padding a number to a fixed width leaves space between the number and
whatever literal precedes it. Uppercase `X` and `A` relocate that space.

**A hugged format group moves its leading spaces to the far side of the
number, just inside the last character of the trailing literal.** The
number ends up flush against its opening delimiter, the closing
delimiter stays at the end of the cell, and the displaced spaces sit
between them. Nothing is added or removed, so the total width – and
therefore the column alignment – is unchanged.

``` r

# standard: padding sits between "(" and the number
apply_formats(f_str("xxx (xxx.x%)", "n", "pct"), c(1, 78), c(1.2, 90.7))
#> [1] "  1 (  1.2%)" " 78 ( 90.7%)"

# hugged: the same padding moves to just before ")"
apply_formats(f_str("xxx (XXX.x%)", "n", "pct"), c(1, 78), c(1.2, 90.7))
#> [1] "  1 (1.2%  )" " 78 (90.7% )"
```

Every cell above is 12 characters wide, hugged or not. In the hugged
version the `(` sits against the first digit, the `%` travels along with
the number it belongs to, and the closing `)` stays pinned at the end of
the cell with the displaced spaces in between.

That is the general rule: hugging splits the trailing literal at its
**last** character, keeps everything before it attached to the number,
and leaves that last character where it was. So any closing delimiter
works, not just a parenthesis:

``` r

apply_formats(f_str("xxx [XXX.x]", "n", "pct"), c(1, 78), c(1.2, 90.7))
#> [1] "  1 [1.2  ]" " 78 [90.7 ]"
```

### Hugging is applied per group, not per side

A capital anywhere in a format group hugs the whole group. It makes no
difference which side of the decimal you write it on, or how many you
write – all three of these are the same instruction:

``` r

apply_formats(f_str("xx (XX.xx%)", "n", "pct"), 12, 4.2)
#> [1] "12 (4.20% )"
apply_formats(f_str("xx (xx.XX%)", "n", "pct"), 12, 4.2)
#> [1] "12 (4.20% )"
apply_formats(f_str("xx (XX.XX%)", "n", "pct"), 12, 4.2)
#> [1] "12 (4.20% )"
```

Write the capitals on the integer side by convention – that is where the
leading spaces being relocated actually are, and Tplyr v1 *required* it,
erroring on a decimal-side capital with “`X` or `A` can only be used on
the left side of a decimal within a format string.” tplyr2 accepts
either, so a spec written for tplyr2 may not load in v1.

### There must be something to hug

A hugged group with no literal in front of it has nowhere to move its
spaces *to*, so it would simply left-justify the number and leave the
padding trailing.
[`f_str()`](https://atorus-research.github.io/tplyr2/reference/f_str.md)
warns when you ask for that:

``` r

apply_formats(f_str("XXX", "n"), 12)   # nothing precedes the number
#> Warning: Format string "XXX": format group 1 uses parenthesis hugging (X/A) but
#> has no literal text before it, so there is nothing to hug -- the number will be
#> left-justified with trailing spaces. Use lowercase x/a for a leading group.
#> [1] "12 "
apply_formats(f_str("xxx", "n"), 12)   # what you almost certainly wanted
#> [1] " 12"
```

The format string still builds – the warning tells you the hugging had
no effect rather than blocking the table. v1 raised a hard error here
instead.

### Migrating from Tplyr v1

Beyond those two, **tplyr2’s hugged output is not byte-identical to
v1’s.** v1 moved the displaced spaces to the *left* of the opening
delimiter; tplyr2 moves them to the *right* of the number:

| Format        | Inputs       | Tplyr v1     | tplyr2        |
|:--------------|:-------------|:-------------|:--------------|
| `xx (XXX.x%)` | `12`, `34.5` | `12 (34.5%)` | `12 (34.5% )` |

Both keep the parenthesis flush against the digit and both preserve
total width, so either satisfies the usual mock requirement – but if you
are reconciling tplyr2 output against a v1 table character by character,
hugged cells will differ.

### Hugging with auto-precision

Uppercase `A` is the intersection of the two features: the width comes
from the data *and* the padding is relocated. This is the combination
behind most publication-quality lab tables, because auto-precision is
exactly the situation where the gap after `(` varies from row to row.

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAM",
      settings = layer_settings(
        precision_by = "PARAM",
        precision_on = "AVAL",
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (A.a+2)", "mean", "sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, labs)
show_table(result)
```

| rowlabel1                      | rowlabel2 | res1           | res2           |
|:-------------------------------|:----------|:---------------|:---------------|
| Alanine Aminotransferase (U/L) | Mean (SD) | 28.76 (8.244 ) | 30.49 (7.571 ) |
| Creatinine (mg/dL)             | Mean (SD) | 0.890 (0.1272) | 0.833 (0.1852) |
| Sodium (mmol/L)                | Mean (SD) | 139.1 (4.33  ) | 141.0 (2.98  ) |

The mean uses lowercase `a` – it is the leading number in the cell, so
there is nothing in front of it to hug – while the SD uses `A` on its
integer side so `(` closes up against the first digit. Every cell is
still the same width:

``` r

nchar(result$res1)
#> [1] 14 14 14
```

## Putting It Together

A complete lab summary combining everything in this vignette: precision
measured per parameter, capped so no parameter can run away, the
standard `+1`/`+2` offsets, the SD hugged against its parenthesis, and a
placeholder for groups with nothing to report.

``` r

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAM",
      settings = layer_settings(
        precision_by  = "PARAM",
        precision_on  = "AVAL",
        precision_cap = c(int = 4, dec = 3),
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("a.a+1 (A.a+2)", "mean", "sd"),
          "Median"    = f_str("a.a+1", "median",
                              empty = c(.overall = "NE")),
          "Q1, Q3"    = f_str("a.a+1, a.a+1", "q1", "q3"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

show_table(tplyr_build(spec, labs))
```

| rowlabel1                      | rowlabel2 | res1           | res2           |
|:-------------------------------|:----------|:---------------|:---------------|
| Alanine Aminotransferase (U/L) | n         | 15             | 15             |
| Creatinine (mg/dL)             | n         | 15             | 15             |
| Sodium (mmol/L)                | n         | 15             | 15             |
| Alanine Aminotransferase (U/L) | Mean (SD) | 28.76 (8.244 ) | 30.49 (7.571 ) |
| Creatinine (mg/dL)             | Mean (SD) | 0.890 (0.1272) | 0.833 (0.1852) |
| Sodium (mmol/L)                | Mean (SD) | 139.1 (4.33  ) | 141.0 (2.98  ) |
| Alanine Aminotransferase (U/L) | Median    | 30.80          | 30.70          |
| Creatinine (mg/dL)             | Median    | 0.870          | 0.810          |
| Sodium (mmol/L)                | Median    | 139.0          | 141.0          |
| Alanine Aminotransferase (U/L) | Q1, Q3    | 24.15, 33.35   | 26.75, 35.95   |
| Creatinine (mg/dL)             | Q1, Q3    | 0.820, 0.995   | 0.705, 0.995   |
| Sodium (mmol/L)                | Q1, Q3    | 136.0, 142.0   | 138.5, 143.5   |
| Alanine Aminotransferase (U/L) | Min, Max  |  9.8, 41.5     | 16.5, 46.9     |
| Creatinine (mg/dL)             | Min, Max  | 0.70, 1.11     | 0.54, 1.16     |
| Sodium (mmol/L)                | Min, Max  | 132, 146       | 137, 146       |

## Where to Go From Here

- [`vignette("format_strings")`](https://atorus-research.github.io/tplyr2/articles/format_strings.md)
  – the format string grammar, statistic keywords, rounding, and
  missing-value handling.
- [`vignette("display_conventions")`](https://atorus-research.github.io/tplyr2/articles/display_conventions.md)
  – `<1%` and `>99%` percents, zero-count suppression, statistics as
  separate columns, denominator rows.
- [`vignette("desc")`](https://atorus-research.github.io/tplyr2/articles/desc.md)
  – descriptive statistics layers, custom summaries, and multi-variable
  summaries.
- [`vignette("options")`](https://atorus-research.github.io/tplyr2/articles/options.md)
  – the session-wide `precision_cap` and `IBMRounding`.
