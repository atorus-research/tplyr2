## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tplyr2)
library(knitr)

## ----build_data---------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD",
      by = c(label("Disposition"), "EOSSTT"),
      settings = layer_settings(
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "n", "pct")
        )
      )
    ),
    group_desc("AGE",
      by = label("Age (years)"),
      settings = layer_settings(
        format_strings = list(
          "Mean (SD)" = f_str("xxx.x (xxx.xx)", "mean", "sd"),
          "Median"    = f_str("xxx.x", "median"),
          "Min, Max"  = f_str("xxx, xxx", "min", "max")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 12))

## ----row_masks----------------------------------------------------------------
masked <- apply_row_masks(result)
kable(head(masked[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 12))

## ----row_breaks---------------------------------------------------------------
masked_breaks <- apply_row_masks(result, row_breaks = TRUE)
kable(head(masked_breaks[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")], 14))

## ----collapse-----------------------------------------------------------------
collapsed <- collapse_row_labels(result, "rowlabel1", "rowlabel2", indent = "   ")
kable(head(collapsed[, c("row_label", "res1", "res2", "res3")], 12))

## ----extract_num--------------------------------------------------------------
# Extract the count (first number) from the first result column
counts <- str_extract_num(result$res1, index = 1)
head(counts, 8)

# Extract the percentage (second number)
pcts <- str_extract_num(result$res1, index = 2)
head(pcts, 8)

## ----conditional--------------------------------------------------------------
string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")

# Replace the full string when the percentage (2nd format group) is 0
apply_conditional_format(string, 2, x == 0, " 0        ", full_string = TRUE)

# Replace within the format group when the percentage is less than 1
apply_conditional_format(string, 2, x < 1, "(<1%)")

## ----whitespace---------------------------------------------------------------
original <- c("  5 ( 6.1%)", " 12 (14.6%)", "  3 ( 3.7%)")
replaced <- replace_leading_whitespace(original)

# Show the difference (non-breaking spaces are invisible but present)
nchar(original)
nchar(replaced)

## ----apply_formats------------------------------------------------------------
fmt <- f_str("xxx.x (xxx.xx)", "mean", "sd")
apply_formats(fmt, c(75.3, 68.1, 80.5), c(8.21, 7.55, 9.03))

## ----wrap---------------------------------------------------------------------
ex_text <- c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS")
cat(paste(str_indent_wrap(ex_text, width = 8), collapse = "\n\n"), "\n")

## ----pipeline-----------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRTA",
  layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
      settings = layer_settings(
        distinct_by = "USUBJID",
        format_strings = list(
          n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
        ),
        total_row = TRUE,
        total_row_label = "Any adverse event"
      )
    )
  )
)

output <- tplyr_build(spec, tplyr_adae)

# Post-processing pipeline
display <- output |>
  collapse_row_labels("rowlabel1", "rowlabel2", indent = "   ")

kable(head(display[, c("row_label", "res1", "res2", "res3")], 20))

