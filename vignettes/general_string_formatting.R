## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----anatomy------------------------------------------------------------------
# Two format groups separated by the literal " ("  and closing ")"
fmt <- f_str("xx.x (xx.xx)", "mean", "sd")
fmt

## ----fixed_width--------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      by = "Age (years)",
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
kable(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")])

## ----hug_comparison-----------------------------------------------------------
# Standard formatting: spaces inside parentheses
spec_standard <- tplyr_spec(
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

result_standard <- tplyr_build(spec_standard, tplyr_adsl)

# Parenthesis hugging: spaces shift outside the delimiter
spec_hugged <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        format_strings = list(
          n_counts = f_str("xxx (XXX.x%)", "n", "pct")
        )
      )
    )
  )
)

result_hugged <- tplyr_build(spec_hugged, tplyr_adsl)

## ----show_standard------------------------------------------------------------
kable(result_standard[1:6, c("rowlabel1", "res1", "res2", "res3")])

## ----show_hugged--------------------------------------------------------------
kable(result_hugged[1:6, c("rowlabel1", "res1", "res2", "res3")])

## ----auto_precision-----------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT %in% c("Baseline", "Week 12", "Week 24"),
      settings = layer_settings(
        precision_on = "AVAL",
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("a+1.a+1 (a+2.a+2)", "mean", "sd"),
          "Median"     = f_str("a+1.a+1", "median"),
          "Q1, Q3"     = f_str("a+1.a+1, a+1.a+1", "q1", "q3"),
          "Min, Max"   = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")])

## ----empty_example------------------------------------------------------------
fmt_with_empty <- f_str(
  "xx.x (xx.xx)",
  "mean", "sd",
  empty = c(.overall = "   -")
)
fmt_with_empty

