## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----empty-overall------------------------------------------------------------
test_data <- data.frame(
  TRT = c(rep("A", 5), rep("B", 5), rep("C", 3)),
  VAL = c(1.5, 2.3, 3.1, 4.0, 2.7,
          5.2, 6.1, 3.8, 4.4, 7.0,
          NA, NA, NA),
  stringsAsFactors = FALSE
)

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_desc("VAL",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd",
                              empty = c(.overall = "---")),
          "Median"    = f_str("xx.x", "median",
                              empty = c(.overall = "NE"))
        )
      )
    )
  )
)

result <- tplyr_build(spec, test_data)
kable(result[, !grepl("^ord", names(result))])

## ----auto-precision-----------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"),
          "Median"    = f_str("a.a+1", "median"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max"),
          "Missing"   = f_str("xx", "missing")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

## ----cap-layer----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        precision_cap = c(int = 3, dec = 2),
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

## ----cap-global---------------------------------------------------------------
tplyr2_options(precision_cap = c(int = 3, dec = 1))

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

tplyr2_options(precision_cap = NULL)

## ----external-precision-------------------------------------------------------
ext_precision <- data.frame(
  PARAMCD = "URATE",
  max_int = 3L,
  max_dec = 1L,
  stringsAsFactors = FALSE
)

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        precision_data = ext_precision,
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

## ----hug-basic----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "Standard"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Hugged"    = f_str("xx.x (XX.xx)", "mean", "sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## ----hug-auto-----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        format_strings = list(
          "Mean (SD)" = f_str("a.a+1 (A.A+2)", "mean", "sd"),
          "Min [Max]" = f_str("a.a [A.a]", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

## ----full-example-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_desc("AVAL",
      by = "PARAMCD",
      settings = layer_settings(
        precision_by = "PARAMCD",
        precision_on = "AVAL",
        precision_cap = c(int = 4, dec = 3),
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("a.a+1 (A.A+2)", "mean", "sd",
                              empty = c(.overall = "")),
          "Median"    = f_str("a.a+1", "median",
                              empty = c(.overall = "NE")),
          "Q1, Q3"    = f_str("a.a+1, a.a+1", "q1", "q3"),
          "Min, Max"  = f_str("a.a, a.a", "min", "max"),
          "Missing"   = f_str("xx", "missing")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

