## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----eval = FALSE-------------------------------------------------------------
# # Tplyr v1: data bound at table creation
# t <- tplyr_table(adsl, TRT01P)

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX")
  )
)

# Data provided at build time
result <- tplyr_build(spec, tplyr_adsl)

## ----eval = FALSE-------------------------------------------------------------
# # Tplyr v1: bare symbols              # tplyr2: quoted strings
# group_count(SEX)                       group_count("SEX")
# group_desc(AGE)                        group_desc("AGE")

## ----eval = FALSE-------------------------------------------------------------
# group_count("SEX", where = SAFFL == "Y")

## ----eval = FALSE-------------------------------------------------------------
# # Tplyr v1: piped modifiers
# group_count(RACE) %>%
#   set_format_strings(f_str("xx (xx.x%)", n, pct)) %>%
#   set_distinct_by(USUBJID) %>%
#   set_denoms_by(TRT01P)

## ----eval = FALSE-------------------------------------------------------------
# # tplyr2: declarative settings
# group_count("RACE",
#   settings = layer_settings(
#     format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct")),
#     distinct_by = "USUBJID",
#     denoms_by = "TRT01P"
#   )
# )

## ----eval = FALSE-------------------------------------------------------------
# # v1: bare symbols                     # tplyr2: quoted strings
# f_str("xx (xx.x%)", n, pct)            f_str("xx (xx.x%)", "n", "pct")

## ----eval = FALSE-------------------------------------------------------------
# # Desc layer: named list of format strings
# format_strings = list(
#   "n"         = f_str("xxx", "n"),
#   "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
# )
# # Count layer: key is n_counts
# format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))

## ----eval = FALSE-------------------------------------------------------------
# # Tplyr v1 approach (not evaluated)
# tplyr_table(adsl, TRT01P, where = SAFFL == "Y") %>%
#   add_layer(
#     group_count(SEX, by = "Sex n (%)")
#   ) %>%
#   add_layer(
#     group_desc(AGE, by = "Age (Years)") %>%
#       set_format_strings(
#         "n"         = f_str("xxx", n),
#         "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd),
#         "Median"    = f_str("xx.x", median),
#         "Min, Max"  = f_str("xx, xx", min, max)
#       )
#   ) %>%
#   build()

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count("SEX", by = "Sex n (%)"),
    group_desc("AGE",
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

## ----eval = FALSE-------------------------------------------------------------
# # Tplyr v1 approach (not evaluated)
# tplyr_table(adae, TRTA) %>%
#   add_layer(
#     group_count(vars(AEBODSYS, AEDECOD)) %>%
#       set_distinct_by(USUBJID) %>%
#       set_format_strings(f_str("xxx (xx.x%)", distinct_n, distinct_pct)) %>%
#       set_order_count_method("bycount") %>%
#       set_ordering_cols("Xanomeline High Dose")
#   ) %>%
#   build()

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        order_count_method = "bycount",
        ordering_cols = "Xanomeline High Dose"
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, !grepl("^ord", names(result))], 15))

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX"),
    group_desc("AGE")
  )
)

tmp <- tempfile(fileext = ".json")
tplyr_write_spec(spec, tmp)
spec_loaded <- tplyr_read_spec(tmp)
spec_loaded

## -----------------------------------------------------------------------------
custom_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  data.frame(
    geo_mean = exp(mean(log(vals[vals > 0]), na.rm = TRUE)),
    geo_sd   = exp(sd(log(vals[vals > 0]), na.rm = TRUE))
  )
}

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_analyze("AGE", analyze_fn = custom_fn, settings = layer_settings(
      format_strings = list(
        "Geometric Mean (SD)" = f_str("xx.xx (xx.xx)", "geo_mean", "geo_sd")
      )
    ))
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("SEX"))
)
result <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
row_ids <- generate_row_ids(result)

# Inspect the metadata for one cell, then retrieve its source rows
tplyr_meta_result(result, row_ids[1], "res1")
source_rows <- tplyr_meta_subset(result, row_ids[1], "res1", tplyr_adsl)
kable(head(source_rows[, c("USUBJID", "SEX", "TRT01P")]))

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(group_count("SEX"))
)
result <- tplyr_build(spec, tplyr_adsl)

kable(head(tplyr_to_ard(result), 10))
kable(tplyr_numeric_data(result, layer = 1))

