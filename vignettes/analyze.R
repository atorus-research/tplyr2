## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----format-strings-basic-----------------------------------------------------
geo_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  pos_vals <- vals[!is.na(vals) & vals > 0]
  data.frame(
    geo_mean = exp(mean(log(pos_vals))),
    geo_sd   = exp(sd(log(pos_vals)))
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT == "Baseline",
      analyze_fn = geo_fn,
      settings = layer_settings(
        format_strings = list(
          "Geometric Mean" = f_str("xxx.xx", "geo_mean"),
          "Geometric SD"   = f_str("xxx.xx", "geo_sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

## ----format-strings-combined--------------------------------------------------
summary_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  vals <- vals[!is.na(vals)]
  data.frame(
    n      = length(vals),
    mean   = mean(vals),
    sd     = sd(vals),
    median = median(vals)
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT == "Baseline",
      analyze_fn = summary_fn,
      settings = layer_settings(
        format_strings = list(
          "n"         = f_str("xx", "n"),
          "Mean (SD)" = f_str("xxx.x (xxx.xx)", "mean", "sd"),
          "Median"    = f_str("xxx.xx", "median")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

## ----preformatted-basic-------------------------------------------------------
range_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  vals <- vals[!is.na(vals)]
  data.frame(
    row_label = c("Range", "Ratio (Max/Min)"),
    formatted = c(
      sprintf("%.1f - %.1f", min(vals), max(vals)),
      sprintf("%.2f", max(vals) / min(vals))
    )
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT == "Baseline",
      analyze_fn = range_fn
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

## ----by-variable--------------------------------------------------------------
mean_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  vals <- vals[!is.na(vals)]
  data.frame(
    row_label = "Mean (SD)",
    formatted = sprintf("%.1f (%.2f)", mean(vals), sd(vals))
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = c("Urate (umol/L)", "AVISIT"),
      where = AVISIT %in% c("Baseline", "Week 4", "Week 8"),
      analyze_fn = mean_fn
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

## ----multi-layer--------------------------------------------------------------
geo_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  pos_vals <- vals[!is.na(vals) & vals > 0]
  data.frame(
    geo_mean = exp(mean(log(pos_vals)))
  )
}

spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX",
      by = "Gender",
      settings = layer_settings(
        format_strings = list(
          "n (%)" = f_str("xx (xx.x%)", "n", "pct")
        )
      )
    ),
    group_analyze("AGE",
      by = "Age (years)",
      analyze_fn = geo_fn,
      settings = layer_settings(
        format_strings = list(
          "Geometric Mean" = f_str("xx.xx", "geo_mean")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(result[, !grepl("^ord", names(result))])

## ----error-handling-----------------------------------------------------------
safe_geo_fn <- function(.data, .target_var) {
  vals <- .data[[.target_var]]
  pos_vals <- vals[!is.na(vals) & vals > 0]

  if (length(pos_vals) < 2) {
    return(data.frame(geo_mean = NA_real_, geo_sd = NA_real_))
  }

  data.frame(
    geo_mean = exp(mean(log(pos_vals))),
    geo_sd   = exp(sd(log(pos_vals)))
  )
}

spec <- tplyr_spec(
  cols = "TRTP",
  layers = tplyr_layers(
    group_analyze("AVAL",
      by = "Urate (umol/L)",
      where = AVISIT == "Baseline",
      analyze_fn = safe_geo_fn,
      settings = layer_settings(
        format_strings = list(
          "Geometric Mean" = f_str("xxx.xx", "geo_mean"),
          "Geometric SD"   = f_str("xxx.xx", "geo_sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adlb)
kable(result[, !grepl("^ord", names(result))])

