## ----setup, include=FALSE-----------------------------------------------------
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
    group_desc(
      "AGE",
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

## -----------------------------------------------------------------------------
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
ard <- tplyr_to_ard(result)
kable(head(ard, 15))

## -----------------------------------------------------------------------------
count_ard <- ard[ard$analysis_id == 1, ]
kable(count_ard)

## -----------------------------------------------------------------------------
desc_ard <- ard[ard$analysis_id == 2, ]
kable(head(desc_ard, 20))

## -----------------------------------------------------------------------------
rebuilt <- tplyr_from_ard(ard, spec)
kable(rebuilt[, !grepl("^ord", names(rebuilt))])

## -----------------------------------------------------------------------------
original_sorted <- result[order(result$rowlabel1), ]
rebuilt_sorted  <- rebuilt[order(rebuilt$rowlabel1), ]

all(trimws(original_sorted$res1) == trimws(rebuilt_sorted$res1))

## -----------------------------------------------------------------------------
# Same ARD, different formatting
compact_spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(
      "SEX",
      settings = layer_settings(
        format_strings = list(n_counts = f_str("xx", "n"))
      )
    ),
    group_desc(
      "AGE",
      settings = layer_settings(
        format_strings = list(
          "n"    = f_str("xx", "n"),
          "Mean" = f_str("xx.x", "mean")
        )
      )
    )
  )
)

compact_result <- tplyr_from_ard(ard, compact_spec)
kable(compact_result[, !grepl("^ord", names(compact_result))])

