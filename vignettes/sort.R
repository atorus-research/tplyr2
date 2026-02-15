## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----basic_ordering-----------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "ord_layer_index", "ord_layer_1")])

## ----multi_layer--------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE"),
    group_desc("AGE",
      by = "Age (years)",
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"     = f_str("xx.x", "median")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "rowlabel2", "res1", "ord_layer_index", "ord_layer_1")])

## ----reorder_layers-----------------------------------------------------------
# Swap layer order: desc first, then counts
result$sort_key <- ifelse(result$ord_layer_index == 2, 1, 2)
reordered <- result[order(result$sort_key, result$ord_layer_1), ]
kable(reordered[, c("rowlabel1", "rowlabel2", "res1", "ord_layer_index", "ord_layer_1")])

## ----factor_ordering----------------------------------------------------------
adsl <- tplyr_adsl
adsl$DCDECOD <- factor(adsl$DCDECOD, levels = c(
  "COMPLETED",
  "ADVERSE EVENT",
  "WITHDRAWAL BY SUBJECT",
  "PHYSICIAN DECISION",
  "STUDY TERMINATED BY SPONSOR",
  "LACK OF EFFICACY",
  "PROTOCOL VIOLATION",
  "LOST TO FOLLOW-UP",
  "DEATH"
))

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        order_count_method = "byfactor"
      )
    )
  )
)

result <- tplyr_build(spec, adsl)
sorted <- result[order(result$ord_layer_1), ]
kable(sorted[, c("rowlabel1", "res1", "res2", "res3", "ord_layer_1")])

## ----varn_ordering------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE",
      settings = layer_settings(
        order_count_method = "byvarn"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
sorted <- result[order(result$ord_layer_1), ]
kable(sorted[, c("rowlabel1", "res1", "res2", "res3", "ord_layer_1")])

## ----desc_ordering------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
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
kable(result[, c("rowlabel1", "res1", "res2", "res3", "ord_layer_1")])

## ----bycount------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        order_count_method = "bycount"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
sorted <- result[order(result$ord_layer_1), ]
kable(sorted[, c("rowlabel1", "res1", "res2", "res3", "ord_layer_1")])

## ----bycount_ordering_cols----------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        order_count_method = "bycount",
        ordering_cols = "Placebo",
        result_order_var = "n"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
sorted <- result[order(result$ord_layer_1), ]
kable(sorted[, c("rowlabel1", "res1", "res2", "res3", "ord_layer_1")])

## ----nested_basic-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
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
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "ord_layer_index",
                       "ord_layer_1", "ord_layer_2")], 12))

## ----practical----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE",
      by = "Race",
      settings = layer_settings(
        order_count_method = "byvarn"
      )
    ),
    group_desc("AGE",
      by = "Age (years)",
      settings = layer_settings(
        format_strings = list(
          "n"          = f_str("xx", "n"),
          "Mean (SD)"  = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Median"     = f_str("xx.x", "median"),
          "Min, Max"   = f_str("xx, xx", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)

# Sort by layer index, then within-layer order
result <- result[order(result$ord_layer_index, result$ord_layer_1), ]

# Drop ordering columns for display
display_cols <- !grepl("^ord_", names(result))
kable(result[, display_cols])

