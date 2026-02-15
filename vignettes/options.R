## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----view-options-------------------------------------------------------------
tplyr2_options()

## ----ibm-bankers--------------------------------------------------------------
# Create data where rounding makes a visible difference
demo_data <- data.frame(
  TRT = rep(c("Drug", "Placebo"), each = 4),
  VAL = c(2.5, 3.5, 4.5, 5.5, 1.5, 2.5, 3.5, 4.5)
)

spec <- tplyr_spec(
  cols = "TRT",
  layers = tplyr_layers(
    group_desc("VAL",
      settings = layer_settings(
        format_strings = list(
          "Mean" = f_str("x.x", "mean")
        )
      )
    )
  )
)

# Default: banker's rounding
result_bankers <- tplyr_build(spec, demo_data)
kable(result_bankers[, !grepl("^ord", names(result_bankers))],
      caption = "Banker's rounding (default)")

## ----ibm-enabled--------------------------------------------------------------
# IBM rounding
tplyr2_options(IBMRounding = TRUE)

result_ibm <- tplyr_build(spec, demo_data)
kable(result_ibm[, !grepl("^ord", names(result_ibm))],
      caption = "IBM rounding (round-half-away-from-zero)")

# Reset to default
tplyr2_options(IBMRounding = FALSE)

## ----quantile-compare-7-------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "Q1, Q3"  = f_str("xx.xx, xx.xx", "q1", "q3"),
          "IQR"     = f_str("xx.xx", "iqr")
        )
      )
    )
  )
)

# Type 7 (R default)
result_t7 <- tplyr_build(spec, tplyr_adsl)
kable(result_t7[, !grepl("^ord", names(result_t7))],
      caption = "Quantile Type 7 (R default)")

## ----quantile-compare-3-------------------------------------------------------
# Type 3 (SAS-compatible)
tplyr2_options(quantile_type = 3)

result_t3 <- tplyr_build(spec, tplyr_adsl)
kable(result_t3[, !grepl("^ord", names(result_t3))],
      caption = "Quantile Type 3 (SAS-compatible)")

# Reset to default
tplyr2_options(quantile_type = 7)

## ----precision-cap------------------------------------------------------------
# Without a cap, precision is driven entirely by the data
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("WEIGHTBL",
      by = "Weight (kg)",
      settings = layer_settings(
        format_strings = list(
          "Mean (SD)" = f_str("a+1.a+1 (a+2.a+2)", "mean", "sd")
        )
      )
    )
  )
)

result_nocap <- tplyr_build(spec, tplyr_adsl)
kable(result_nocap[, !grepl("^ord", names(result_nocap))],
      caption = "Auto-precision, no cap")

## ----precision-cap-applied----------------------------------------------------
# Apply a global cap: max 3 integer digits, max 1 decimal digit
tplyr2_options(precision_cap = c(int = 3, dec = 1))

result_capped <- tplyr_build(spec, tplyr_adsl)
kable(result_capped[, !grepl("^ord", names(result_capped))],
      caption = "Auto-precision, capped at int=3, dec=1")

# Reset to default
tplyr2_options(precision_cap = NULL)

## ----custom-summaries---------------------------------------------------------
# Register session-level custom summaries
tplyr2_options(
  custom_summaries = list(
    geo_mean = quote(exp(mean(log(.var[.var > 0]), na.rm = TRUE))),
    cv       = quote(sd(.var, na.rm = TRUE) / mean(.var, na.rm = TRUE) * 100)
  )
)

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "n"              = f_str("xx", "n"),
          "Mean (SD)"      = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Geometric Mean" = f_str("xx.xx", "geo_mean"),
          "CV (%)"         = f_str("xx.x", "cv")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## ----custom-summaries-reset---------------------------------------------------
# Reset to default
tplyr2_options(custom_summaries = NULL)

## ----scipen-------------------------------------------------------------------
# Check the current value
tplyr2_options()$tplyr2.scipen

## ----reset-all----------------------------------------------------------------
tplyr2_options(
  IBMRounding     = FALSE,
  quantile_type   = 7L,
  precision_cap   = NULL,
  custom_summaries = NULL,
  scipen          = 9999L
)

# Verify
tplyr2_options()

