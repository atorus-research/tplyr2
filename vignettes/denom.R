## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----default------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])

## ----denoms_default_by--------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      by = "SEX"
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 10))

## ----denoms_by_sex------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      by = "SEX",
      settings = layer_settings(
        denoms_by = c("TRT01P", "SEX")
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 10))

## ----denom_where--------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      settings = layer_settings(
        denom_where = quote(DCDECOD != "COMPLETED")
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])

## ----denom_ignore-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("RACE",
      settings = layer_settings(
        denom_ignore = c("AMERICAN INDIAN OR ALASKA NATIVE")
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", "res1", "res2", "res3")])

## ----distinct-----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xx (xx.x%) [xxx]", "distinct_n", "distinct_pct", "n")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3")], 8))

## ----pop_data-----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3")], 8))

## ----header_n-----------------------------------------------------------------
kable(tplyr_header_n(result))

## ----missing_subjects---------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01P")),
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        missing_subjects = TRUE,
        missing_subjects_label = "Not reported",
        format_strings = list(
          n_counts = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)

# Show the "Not reported" row
nr_idx <- which(result$rowlabel1 == "Not reported")
kable(result[nr_idx, c("rowlabel1", "res1", "res2", "res3")])

