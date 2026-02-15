## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----intro-example------------------------------------------------------------
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
          "Min, Max"   = f_str("xx, xx", "min", "max"),
          "Missing"    = f_str("xx", "missing")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## ----quantile-type7-----------------------------------------------------------
# Default Type 7 (R default)
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "Q1, Q3" = f_str("xx.x, xx.x", "q1", "q3")
        )
      )
    )
  )
)

result_type7 <- tplyr_build(spec, tplyr_adsl)
kable(result_type7[, !grepl("^ord", names(result_type7))],
      caption = "Type 7 (R default)")

## ----quantile-type3-----------------------------------------------------------
# Type 3 (matches SAS PROC UNIVARIATE default)
tplyr2_options(quantile_type = 3)

result_type3 <- tplyr_build(spec, tplyr_adsl)
kable(result_type3[, !grepl("^ord", names(result_type3))],
      caption = "Type 3 (SAS-like)")

# Reset to default
tplyr2_options(quantile_type = 7)

## ----custom-layer-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "n"              = f_str("xx", "n"),
          "Mean (SD)"      = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Geometric Mean" = f_str("xx.xx", "geo_mean")
        ),
        custom_summaries = list(
          geo_mean = quote(exp(mean(log(.var[.var > 0]), na.rm = TRUE)))
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## ----custom-session-----------------------------------------------------------
# Register a coefficient of variation summary for the session
tplyr2_options(
  custom_summaries = list(
    cv = quote(sd(.var, na.rm = TRUE) / mean(.var, na.rm = TRUE) * 100)
  )
)

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "CV (%)"     = f_str("xx.x", "cv")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

# Clean up
tplyr2_options(custom_summaries = NULL)

## ----custom-override----------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "Trimmed Mean" = f_str("xx.x", "mean")
        ),
        custom_summaries = list(
          mean = quote(mean(.var, trim = 0.1, na.rm = TRUE))
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## ----multi-target-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc(c("AGE", "HEIGHTBL", "WEIGHTBL"),
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"     = f_str("xx.x", "median"),
          "Q1, Q3"     = f_str("xx.x, xx.x", "q1", "q3"),
          "Min, Max"   = f_str("xx, xx", "min", "max"),
          "Missing"    = f_str("xx", "missing")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## ----multi-target-by----------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc(c("AGE", "WEIGHTBL"),
      by = "Demographics",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"    = f_str("xx.x", "median")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

