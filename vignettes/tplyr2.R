## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX"),
    group_desc("AGE")
  )
)
spec

## -----------------------------------------------------------------------------
layers <- tplyr_layers(
  group_count("SEX", by = "Sex n (%)"),
  group_count("RACE", by = "Race n (%)"),
  group_desc("AGE", by = "Age (Years)")
)

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count("SEX", by = "Sex n (%)"),
    group_desc(
      "AGE",
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

## -----------------------------------------------------------------------------
# Two decimal places for mean, two for SD, with parentheses
f_str("xx.xx (xx.xx)", "mean", "sd")

# Integer count only
f_str("xxx", "n")

# Count with percentage
f_str("xx (xx.x%)", "n", "pct")

## ----eval=FALSE---------------------------------------------------------------
# # n (pct%) -- the default
# f_str("xx (xx.x%)", "n", "pct")
# 
# # Distinct count and percentage
# f_str("xx (xx.x%)", "distinct_n", "distinct_pct")

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc(
      c("AGE", "AVGDD"),
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Min, Max"  = f_str("xx, xx", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(
      "RACE",
      settings = layer_settings(total_row = TRUE)
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(
      "AEBODSYS",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(
      c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, !grepl("^ord", names(result))], 15))

## -----------------------------------------------------------------------------
set.seed(12345)
shift_data <- data.frame(
  USUBJID = paste0("SUBJ", 1:30),
  TRTA = rep(c("Placebo", "Active"), each = 15),
  BNRIND = factor(
    sample(c("LOW", "NORMAL", "HIGH"), 30, replace = TRUE, prob = c(0.2, 0.6, 0.2)),
    levels = c("LOW", "NORMAL", "HIGH")
  ),
  ANRIND = factor(
    sample(c("LOW", "NORMAL", "HIGH"), 30, replace = TRUE, prob = c(0.15, 0.5, 0.35)),
    levels = c("LOW", "NORMAL", "HIGH")
  )
)

spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_shift(
      c(row = "BNRIND", column = "ANRIND"),
      settings = layer_settings(
        format_strings = list(n_counts = f_str("xxx", "n"))
      )
    )
  )
)

result <- tplyr_build(spec, shift_data)
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX"),
    group_desc(
      "AGE",
      settings = layer_settings(
        format_strings = list(
          "n" = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)

# Raw numeric data for the count layer (layer 1)
kable(tplyr_numeric_data(result, layer = 1))

## -----------------------------------------------------------------------------
# Raw numeric data for the desc layer (layer 2)
kable(tplyr_numeric_data(result, layer = 2))

