## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----simple_count-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])

## ----total_group--------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  total_groups = list(total_group("TRT01P", label = "Total")),
  layers = tplyr_layers(
    group_count("DCDECOD")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3", "res4")])

## ----custom_group-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  custom_groups = list(
    custom_group("TRT01P",
      "Treated" = c("Xanomeline High Dose", "Xanomeline Low Dose")
    )
  ),
  layers = tplyr_layers(
    group_count("DCDECOD")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3", "res4")])

## ----format_strings-----------------------------------------------------------
spec <- tplyr_spec(
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

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])

## ----total_row----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        total_row = TRUE,
        total_row_label = "Overall Total"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])

## ----distinct_counts----------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%) [xxx]", "distinct_n", "distinct_pct", "n")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3")], 10))

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
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 15))

