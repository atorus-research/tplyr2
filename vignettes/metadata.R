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
  layers = tplyr_layers(
    group_count("SEX"),
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "n" = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)

## -----------------------------------------------------------------------------
result$row_id

## -----------------------------------------------------------------------------
meta <- tplyr_meta_result(result, "1_F", "res1")
meta

## ----error=TRUE---------------------------------------------------------------
try({
result_no_meta <- tplyr_build(spec, tplyr_adsl, metadata = FALSE)
tplyr_meta_result(result_no_meta, "1_F", "res1")
})

## -----------------------------------------------------------------------------
source_rows <- tplyr_meta_subset(result, "1_F", "res1", tplyr_adsl)
nrow(source_rows)
all(source_rows$SEX == "F")
unique(source_rows$TRT01P)

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("DCDECOD", by = "EOSSTT")
  )
)

result <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
kable(head(result[, c("rowlabel1", "rowlabel2", "res1", "res2", "res3")]))

## -----------------------------------------------------------------------------
rid <- result$row_id[1]
meta <- tplyr_meta_result(result, rid, "res1")
meta

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_count("SEX", settings = layer_settings(total_row = TRUE))
  )
)

result <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
total_row <- result[result$rowlabel1 == "Total", ]
meta <- tplyr_meta_result(result, total_row$row_id, "res1")
meta

## -----------------------------------------------------------------------------
source_rows <- tplyr_meta_subset(result, total_row$row_id, "res1", tplyr_adsl)
nrow(source_rows)

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  layers = tplyr_layers(
    group_desc("AGE",
      settings = layer_settings(
        format_strings = list(
          "n" = f_str("xxx", "n"),
          "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
        )
      )
    )
  )
)

result <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
n_meta <- tplyr_meta_result(result, "1_n", "res1")
n_meta

## -----------------------------------------------------------------------------
source_rows <- tplyr_meta_subset(result, "1_n", "res1", tplyr_adsl)
c(n = nrow(source_rows), mean = mean(source_rows$AGE), sd = sd(source_rows$AGE))

## -----------------------------------------------------------------------------
spec <- tplyr_spec(
  cols = "TRT01P",
  where = SAFFL == "Y",
  layers = tplyr_layers(
    group_count("SEX")
  )
)

result <- tplyr_build(spec, tplyr_adsl, metadata = TRUE)
meta <- tplyr_meta_result(result, result$row_id[1], "res1")
meta

## -----------------------------------------------------------------------------
target <- data.frame(
  TRT = c("A", "A", "B"),
  USUBJID = c("S1", "S2", "S3"),
  VAL = c("X", "Y", "X")
)

pop <- data.frame(
  TRT = c("A", "A", "A", "B", "B"),
  USUBJID = c("S1", "S2", "S4", "S3", "S5")
)

spec <- tplyr_spec(
  cols = "TRT",
  pop_data = pop_data(cols = "TRT"),
  layers = tplyr_layers(
    group_count("VAL",
      settings = layer_settings(
        distinct_by = "USUBJID",
        missing_subjects = TRUE,
        missing_subjects_label = "Not in Target"
      )
    )
  )
)

result <- tplyr_build(spec, target, pop_data = pop, metadata = TRUE)
kable(result[, c("rowlabel1", "res1", "res2")])

## -----------------------------------------------------------------------------
ms_row <- result[result$rowlabel1 == "Not in Target", ]
meta <- tplyr_meta_result(result, ms_row$row_id, "res1")
meta

## -----------------------------------------------------------------------------
missing_a <- tplyr_meta_subset(result, ms_row$row_id, "res1",
                                target, pop_data = pop)
missing_a

## ----warning=TRUE-------------------------------------------------------------
tplyr_meta_subset(result, ms_row$row_id, "res1", target)

## ----eval=FALSE---------------------------------------------------------------
# result <- tplyr_build(spec, data, metadata = TRUE)
# source <- tplyr_meta_subset(result, row_id = "1_F", column = "res2", data = data)
# nrow(source)

## ----eval=FALSE---------------------------------------------------------------
# observeEvent(input$table_cell_click, {
#   click <- input$table_cell_click
#   row_id <- result$row_id[click$row]
#   col_name <- names(result)[click$col]
#   source_data <- tplyr_meta_subset(result, row_id, col_name, original_data)
#   output$detail_table <- renderTable(source_data)
# })

