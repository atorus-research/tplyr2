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

json_path <- tempfile(fileext = ".json")
tplyr_write_spec(spec, json_path)

## -----------------------------------------------------------------------------
loaded_spec <- tplyr_read_spec(json_path)
loaded_spec

## -----------------------------------------------------------------------------
result <- tplyr_build(loaded_spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## -----------------------------------------------------------------------------
yaml_path <- tempfile(fileext = ".yaml")
tplyr_write_spec(spec, yaml_path)

yaml_spec <- tplyr_read_spec(yaml_path)
yaml_result <- tplyr_build(yaml_spec, tplyr_adsl)

# Confirm the results match
identical(
  result[, !grepl("^ord", names(result))],
  yaml_result[, !grepl("^ord", names(yaml_result))]
)

## -----------------------------------------------------------------------------
json_content <- readLines(json_path)
cat(json_content, sep = "\n")

## -----------------------------------------------------------------------------
complex_spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  total_groups = list(total_group("TRT01P", label = "Total")),
  layers = tplyr_layers(
    group_count(
      "RACE",
      by = "Race n (%)",
      settings = layer_settings(
        total_row = TRUE,
        total_row_label = "Total"
      )
    ),
    group_desc(
      c("AGE", "WEIGHTBL"),
      by = "Baseline Measurements",
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
          "Min, Max"  = f_str("xx.x, xx.x", "min", "max")
        )
      )
    )
  )
)

complex_path <- tempfile(fileext = ".json")
tplyr_write_spec(complex_spec, complex_path)

## -----------------------------------------------------------------------------
reloaded <- tplyr_read_spec(complex_path)
complex_result <- tplyr_build(reloaded, tplyr_adsl)
kable(complex_result[, !grepl("^ord", names(complex_result))])

## -----------------------------------------------------------------------------
# Same spec, different data subsets
saffl_result <- tplyr_build(loaded_spec, tplyr_adsl[tplyr_adsl$SAFFL == "Y", ])
ittfl_result <- tplyr_build(loaded_spec, tplyr_adsl[tplyr_adsl$ITTFL == "Y", ])

## ----include=FALSE------------------------------------------------------------
# Clean up temp files
file.remove(json_path, yaml_path, complex_path)

