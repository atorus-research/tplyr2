## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----basic--------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo"))
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1")], 10))

## ----label--------------------------------------------------------------------
attr(result$rdiff1, "label")

## ----multiple-----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(
            c("Xanomeline High Dose", "Placebo"),
            c("Xanomeline Low Dose", "Placebo")
          ),
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1", "rdiff2")], 8))

## ----multiple_labels----------------------------------------------------------
attr(result$rdiff1, "label")
attr(result$rdiff2, "label")

## ----format_pvalue------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          format = f_str("xx.x (xx.x, xx.x) [x.xxxx]", "rdiff", "lower", "upper", "p_value")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1")], 8))

## ----ci90---------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          ci = 0.90,
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "rdiff1")], 8))

## ----distinct-----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%) [xxx]", "distinct_n", "distinct_pct", "n")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
kable(head(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1")], 8))

## ----special_rows-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        total_row = TRUE,
        total_row_label = "Any adverse event",
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
# Show the last few rows including the total row
tail_rows <- tail(result[, c("rowlabel1", "res1", "res2", "res3", "rdiff1")], 5)
kable(tail_rows)

## ----numeric_data-------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        risk_diff = list(
          comparisons = list(c("Xanomeline High Dose", "Placebo")),
          format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adae)
nd <- tplyr_numeric_data(result, layer = 1)
kable(head(nd, 10))

## ----extract_num--------------------------------------------------------------
# Extract the risk difference value (1st number)
result$rdiff_value <- str_extract_num(result$rdiff1, 1)

# Extract the lower CI bound (2nd number)
result$rdiff_lower <- str_extract_num(result$rdiff1, 2)

# Extract the upper CI bound (3rd number)
result$rdiff_upper <- str_extract_num(result$rdiff1, 3)

kable(head(result[, c("rowlabel1", "rdiff1", "rdiff_value", "rdiff_lower", "rdiff_upper")], 8))

