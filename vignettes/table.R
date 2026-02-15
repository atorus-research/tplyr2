## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(target_var = "SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(target_var = "AGEGR1")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = c("TRT01P", "SEX"),
  layers = tplyr_layers(
    group_count(target_var = "AGEGR1")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count(target_var = "AGEGR1", by = "Age Group"),
    group_desc(
      target_var = "AGE",
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
kable(result[, c("rowlabel1", "rowlabel2", grep("^res", names(result), value = TRUE))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  total_groups = list(
    total_group("TRT01P", label = "Total")
  ),
  layers = tplyr_layers(
    group_count(target_var = "SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  custom_groups = list(
    custom_group(
      "TRT01P",
      "Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose")
    )
  ),
  layers = tplyr_layers(
    group_count(target_var = "SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  custom_groups = list(
    custom_group(
      "TRT01P",
      "Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose")
    )
  ),
  total_groups = list(
    total_group("TRT01P", label = "Total")
  ),
  layers = tplyr_layers(
    group_count(target_var = "SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count(
      target_var = "AEBODSYS",
      settings = layer_settings(
        distinct_by = "USUBJID"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))], 8))

## -----------------------------------------------------------------------------
header_n <- tplyr_header_n(result)
kable(header_n)

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(
    cols = c("TRTA" = "TRT01A"),
    where = SAFFL == "Y"
  ),
  layers = tplyr_layers(
    group_count(
      target_var = "AEBODSYS",
      settings = layer_settings(
        distinct_by = "USUBJID"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(tplyr_header_n(result))

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count(target_var = "RACE")
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, c("rowlabel1", grep("^res", names(result), value = TRUE))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
  layers = tplyr_layers(
    group_count(
      target_var = "AEDECOD",
      by = "AEBODSYS",
      settings = layer_settings(
        distinct_by = "USUBJID",
        limit_data_by = c("AEBODSYS", "AEDECOD")
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", grep("^res", names(result), value = TRUE))], 10))

