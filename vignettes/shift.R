## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tplyr2)
library(knitr)

## ----basic-data---------------------------------------------------------------
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

## ----basic-shift--------------------------------------------------------------
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

## ----factor-shift-------------------------------------------------------------
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

## ----format-shift-------------------------------------------------------------
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

