# Shift Layers

## Introduction

Shift tables are a common display in clinical trial reporting. They
summarize the change in a categorical state from one time point to
another – most often, the change in laboratory normal range indicator
from baseline to a post-baseline visit. The rows of a shift table
represent the “from” state (e.g., baseline normal range: Low, Normal,
High), and the columns represent the “to” state (e.g., post-baseline
normal range). Each cell contains a count or percentage of subjects who
moved from one category to another.

These tables give reviewers a quick, matrix-style view of how a
treatment affects lab values relative to their reference ranges.

In tplyr2, shift tables are built with
[`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md).
Under the hood, a shift layer is an abstraction of a count layer. It
reuses much of the same counting and formatting code, but the “column”
dimension of the shift variable is folded into the result columns
alongside the treatment variable. This means each result column
represents a treatment-by-shift-category combination, such as “Placebo
\| Normal” or “Xanomeline High Dose \| High”.

Because of this design, shift layers have a few limitations compared to
count layers:

- No nesting (only single-variable shifts)
- No total rows or missing subject rows
- No risk difference
- No result-based sorting

For most shift table needs, these limitations are not a concern. If you
need a text-style shift display (e.g., rows reading “Low to High”,
“Normal to High”), a standard
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
layer is a better fit.

## A Basic Example

To demonstrate shift layers, we will create a small example dataset that
mimics a typical laboratory analysis dataset with baseline and
post-baseline normal range indicators.

``` r
set.seed(42)

# Create example shift data
adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", sprintf("%03d", 1:30)), each = 2),
  TRTA    = rep(rep(c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
                     each = 10), each = 2),
  PARAM   = rep(c("Creatine Kinase", "Alkaline Phosphatase"), times = 30),
  PARAMCD = rep(c("CK", "ALP"), times = 30),
  VISIT   = "WEEK 24",
  BNRIND  = sample(c("N", "H"), 60, replace = TRUE, prob = c(0.7, 0.3)),
  ANRIND  = sample(c("N", "H"), 60, replace = TRUE, prob = c(0.5, 0.5)),
  stringsAsFactors = FALSE
)
```

With data in hand, building a shift table is straightforward. The key
difference from a count layer is the `target_var` argument: instead of a
single string, you pass a **named character vector** with elements
`"row"` and `"column"`. The `"row"` element identifies the variable that
defines the row categories (the “from” state), and `"column"` identifies
the variable that produces additional column groups (the “to” state).

``` r
spec <- tplyr_spec(
  cols = "TRTA",
  where = PARAMCD == "CK",
  layers = tplyr_layers(
    group_shift(
      c(row = "BNRIND", column = "ANRIND"),
      by = c("PARAM", "VISIT")
    )
  )
)

result <- tplyr_build(spec, adlb)
kable(result[, !grepl("^ord_", names(result))])
```

| rowlabel1       | rowlabel2 | rowlabel3 | res1      | res2      | res3      | res4      | res5      | res6      |
|:----------------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|
| Creatine Kinase | WEEK 24   | H         | 3 (30.0%) | 1 (10.0%) | 1 (10.0%) | 3 (30.0%) | 0 ( 0.0%) | 2 (20.0%) |
| Creatine Kinase | WEEK 24   | N         | 2 (20.0%) | 4 (40.0%) | 4 (40.0%) | 2 (20.0%) | 2 (20.0%) | 6 (60.0%) |

A few things to notice in this output:

- The `rowlabel1` and `rowlabel2` columns are populated by the `by`
  variables (PARAM, VISIT), and `rowlabel3` contains the values of the
  row shift variable (BNRIND).
- Each result column represents a treatment-by-ANRIND combination. The
  column labels (stored as attributes) follow the pattern
  `"Treatment | ANRIND (N=n)"`.
- The default format is `"xx (xx.x%)"`, showing a count and percentage,
  the same default used by count layers.

However, you may notice that the only BNRIND values present in the
output are whatever happened to appear in the data. If a category like
“L” (Low) does not appear in the data, it will be absent from the table
entirely. Similarly, the sort order of rows depends on the alphabetical
order of the character values. Both of these problems have the same
solution: factors.

## Filling Missing Groups Using Factors

In R, factor variables carry two important pieces of information: the
set of all allowable levels and their order. When you convert your shift
variables to factors before building the table, tplyr2 uses the factor
levels to:

1.  **Complete the table** – all levels appear as rows (and in columns),
    even those with zero counts.
2.  **Control sort order** – rows follow the order of the factor levels,
    not alphabetical order.

This is especially important for shift tables, where the standard
presentation order is typically Low, Normal, High.

``` r
# Convert to factors with the desired level order
adlb$BNRIND <- factor(adlb$BNRIND, levels = c("L", "N", "H"))
adlb$ANRIND <- factor(adlb$ANRIND, levels = c("L", "N", "H"))

spec <- tplyr_spec(
  cols = "TRTA",
  where = PARAMCD == "CK",
  layers = tplyr_layers(
    group_shift(
      c(row = "BNRIND", column = "ANRIND"),
      by = c("PARAM", "VISIT"),
      settings = layer_settings(
        format_strings = list(
          n_counts = f_str("xx (xxx.x%)", "n", "pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, adlb)
kable(result[, !grepl("^ord_", names(result))])
```

| rowlabel1       | rowlabel2 | rowlabel3 | res1       | res2      | res3       | res4       | res5      | res6       | res7       | res8      | res9       |
|:----------------|:----------|:----------|:-----------|:----------|:-----------|:-----------|:----------|:-----------|:-----------|:----------|:-----------|
| Creatine Kinase | WEEK 24   | L         | 0 ( 0.0%)  | 0 ( 0.0%) | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%) | 0 ( 0.0%)  | 0 ( 0.0%)  | 0 ( 0.0%) | 0 ( 0.0%)  |
| Creatine Kinase | WEEK 24   | N         | 2 ( 20.0%) | 0 ( 0.0%) | 4 ( 40.0%) | 4 ( 40.0%) | 0 ( 0.0%) | 2 ( 20.0%) | 2 ( 20.0%) | 0 ( 0.0%) | 6 ( 60.0%) |
| Creatine Kinase | WEEK 24   | H         | 3 ( 30.0%) | 0 ( 0.0%) | 1 ( 10.0%) | 1 ( 10.0%) | 0 ( 0.0%) | 3 ( 30.0%) | 0 ( 0.0%)  | 0 ( 0.0%) | 2 ( 20.0%) |

Now the table includes all three levels – L, N, and H – in both the rows
and the column groups, regardless of whether any subjects actually fell
into those categories. Cells with no observations display as
`" 0 ( 0.0%)"`, maintaining consistent alignment. The rows also follow
the factor level order (L, N, H) rather than alphabetical order (H, L,
N).

This pattern of using factors to control completeness and ordering
applies broadly across tplyr2, but it is particularly important for
shift tables where the matrix structure must be complete to be
interpretable.

## Customizing the Format

Like count layers, shift layers accept format strings through
[`layer_settings()`](https://github.com/mstackhouse/tplyr2/reference/layer_settings.md).
You can display just counts, just percentages, or any combination. The
format string system is the same one used across all of tplyr2.

``` r
# Counts only
spec_counts <- tplyr_spec(
  cols = "TRTA",
  where = PARAMCD == "CK",
  layers = tplyr_layers(
    group_shift(
      c(row = "BNRIND", column = "ANRIND"),
      by = c("PARAM", "VISIT"),
      settings = layer_settings(
        format_strings = list(
          n_counts = f_str("xx", "n")
        )
      )
    )
  )
)

result_counts <- tplyr_build(spec_counts, adlb)
kable(result_counts[, !grepl("^ord_", names(result_counts))])
```

| rowlabel1       | rowlabel2 | rowlabel3 | res1 | res2 | res3 | res4 | res5 | res6 | res7 | res8 | res9 |
|:----------------|:----------|:----------|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
| Creatine Kinase | WEEK 24   | L         | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    |
| Creatine Kinase | WEEK 24   | N         | 2    | 0    | 4    | 4    | 0    | 2    | 2    | 0    | 6    |
| Creatine Kinase | WEEK 24   | H         | 3    | 0    | 1    | 1    | 0    | 3    | 0    | 0    | 2    |

## One Thing to Note

The
[`group_shift()`](https://github.com/mstackhouse/tplyr2/reference/group_shift.md)
API is designed specifically for matrix-style shift tables, where
baseline categories form the rows and post-baseline categories form the
columns. This cross-tabulation style is the most common shift table
format in clinical reporting.

However, not all shift displays take this form. Sometimes you may need a
text-based summary where each row describes a specific transition, such
as “Low to High” or “Normal to Normal”. For that style of display,
[`group_count()`](https://github.com/mstackhouse/tplyr2/reference/group_count.md)
is the right tool. You would create a derived variable in your data that
concatenates the baseline and post-baseline values (e.g.,
`paste(BNRIND, "to", ANRIND)`) and then count that variable directly.

## Where to Go From Here

Shift layers share much of their underlying machinery with count layers,
so many of the concepts from the count layer documentation apply here as
well. For further reading:

- The **denominator** vignette covers how denominators are computed and
  how to customize them, which directly affects the percentages
  displayed in shift tables.
- The **sorting and ordering** vignette explains how row and column
  ordering works, including the role of factor levels that we touched on
  in this vignette.
