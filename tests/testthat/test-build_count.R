test_that("count layer produces correct output structure", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  expect_s3_class(result, "data.frame")
  expect_true("rowlabel1" %in% names(result))
  expect_true(any(grepl("^res\\d+$", names(result))))
  expect_true("ord_layer_index" %in% names(result))
  expect_true("ord_layer_1" %in% names(result))
})

test_that("count layer produces n (pct%) by default", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Should have rows for each SEX level
  expect_true(nrow(result) >= 2)

  # Values should contain parentheses (n (pct%))
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    expect_true(all(grepl("\\(", result[[col]])))
  }
})

test_that("count layer respects custom format strings", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(
        target_var = "SEX",
        settings = layer_settings(
          format_strings = list(
            "n_counts" = f_str("xxx", "n")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Values should NOT contain parentheses (n only)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    expect_false(any(grepl("\\(", result[[col]])))
  }
})

test_that("count layer handles by labels", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX", by = "Sex n (%)")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  expect_true("rowlabel1" %in% names(result))
  expect_true("rowlabel2" %in% names(result))
  # First label column should be "Sex n (%)"
  expect_true(all(result$rowlabel1 == "Sex n (%)"))
})

test_that("count layer completeness fills zero counts", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "RACE")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Every race level should appear in results, even if zero for some treatments
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    expect_false(any(is.na(result[[col]])))
  }
})

test_that("result columns carry label attributes", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_true(length(res_cols) >= 2)

  # Each result column should have a label attribute with the column value
  for (col in res_cols) {
    lbl <- attr(result[[col]], "label")
    expect_true(!is.null(lbl))
    expect_true(nchar(lbl) > 0)
  }
})

test_that("classify_by separates data vars from labels", {
  col_names <- c("SEX", "TRT01P", "AGE")
  by_info <- tplyr2:::classify_by(c("Demographics", "SEX"), col_names)
  expect_equal(by_info$data_vars, "SEX")
  expect_equal(by_info$labels, "Demographics")
})

test_that("classify_by handles label() objects", {
  col_names <- c("SEX", "TRT01P")
  # Scalar label: label("SEX") should be a label even though SEX is a column
  by_info <- tplyr2:::classify_by(label("SEX"), col_names)
  expect_equal(by_info$labels, "SEX")
  expect_length(by_info$data_vars, 0)

  # Mixed list: use list() to preserve label() class on individual elements
  by_info2 <- tplyr2:::classify_by(list(label("SEX"), "TRT01P"), col_names)
  expect_equal(by_info2$labels, "SEX")
  expect_equal(by_info2$data_vars, "TRT01P")
})

test_that("classify_by handles NULL", {
  by_info <- tplyr2:::classify_by(NULL, c("SEX", "AGE"))
  expect_length(by_info$data_vars, 0)
  expect_length(by_info$labels, 0)
})

# === Phase 2 Tests ===

test_that("distinct_by counts unique subjects", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count(
        target_var = "AESEV",
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

  expect_true(nrow(result) >= 3)  # MILD, MODERATE, SEVERE
  # Values should be formatted distinct counts
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    expect_true(all(grepl("\\(", result[[col]])))
  }
})

test_that("distinct_by produces correct counts", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B"),
    EVENT = c("X", "X", "Y", "X", "Y"),
    SUBJ = c("S1", "S1", "S2", "S3", "S3")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(
        target_var = "EVENT",
        settings = layer_settings(
          distinct_by = "SUBJ",
          format_strings = list(
            n_counts = f_str("xxx", "distinct_n")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # TRT A: EVENT X has 1 distinct (S1), EVENT Y has 1 distinct (S2)
  # TRT B: EVENT X has 1 distinct (S3), EVENT Y has 1 distinct (S3)
  x_row <- result[result$rowlabel1 == "X", ]
  expect_equal(trimws(x_row$res1), "1")  # A: S1
  expect_equal(trimws(x_row$res2), "1")  # B: S3
})

test_that("denoms_by changes denominator grouping", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    GRP = c("G1", "G1", "G2", "G1", "G1", "G2"),
    EVENT = c("X", "Y", "X", "X", "Y", "Y")
  )

  # Default denoms (by cols = TRT): denom is 3 per TRT
  spec1 <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(target_var = "EVENT", by = "GRP")
    )
  )
  result1 <- tplyr_build(spec1, test_data)

  # Custom denoms_by = c("TRT", "GRP"): denom varies by TRT+GRP
  spec2 <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(
        target_var = "EVENT",
        by = "GRP",
        settings = layer_settings(denoms_by = c("TRT", "GRP"))
      )
    )
  )
  result2 <- tplyr_build(spec2, test_data)

  # The percentages should differ between the two specs
  # With denoms_by=TRT: TRT A, G1, X = 1/3
  # With denoms_by=TRT+GRP: TRT A, G1, X = 1/2
  expect_false(identical(result1, result2))
})

test_that("denom_ignore excludes values from denominator", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "A"),
    EVENT = c("X", "Y", "Z", "Z")
  )

  # Without denom_ignore: denom = 4
  spec1 <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(
        target_var = "EVENT",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx.x", "pct"))
        )
      )
    )
  )
  result1 <- tplyr_build(spec1, test_data)

  # With denom_ignore = "Z": denom = 2 (only X and Y counted)
  spec2 <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(
        target_var = "EVENT",
        settings = layer_settings(
          denom_ignore = "Z",
          format_strings = list(n_counts = f_str("xx.x", "pct"))
        )
      )
    )
  )
  result2 <- tplyr_build(spec2, test_data)

  # X: 1/4 = 25% vs 1/2 = 50%
  x_pct1 <- as.numeric(trimws(result1[result1$rowlabel1 == "X", "res1"]))
  x_pct2 <- as.numeric(trimws(result2[result2$rowlabel1 == "X", "res1"]))
  expect_equal(x_pct1, 25)
  expect_equal(x_pct2, 50)
})

test_that("total_row adds a total row", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(
        target_var = "SEX",
        settings = layer_settings(
          total_row = TRUE,
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Should have rows for F, M, and Total
  expect_true("Total" %in% result$rowlabel1)

  # Total row n should equal sum of other rows
  total_row <- result[result$rowlabel1 == "Total", ]
  other_rows <- result[result$rowlabel1 != "Total", ]
  expect_equal(
    as.numeric(trimws(total_row$res1)),
    sum(as.numeric(trimws(other_rows$res1)))
  )
})

test_that("total_row_label customizes label", {
  test_data <- data.frame(
    TRT = c("A", "A", "B"),
    EVENT = c("X", "Y", "X")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(
        target_var = "EVENT",
        settings = layer_settings(
          total_row = TRUE,
          total_row_label = "All Events",
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)
  expect_true("All Events" %in% result$rowlabel1)
})

test_that("keep_levels filters to specified levels", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(
        target_var = "RACE",
        settings = layer_settings(keep_levels = c("WHITE"))
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  expect_equal(nrow(result), 1)
  expect_equal(result$rowlabel1[1], "WHITE")
})

test_that("missing_count adds missing row", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B"),
    EVENT = c("X", "Y", NA, NA, "X", NA)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(
        target_var = "EVENT",
        settings = layer_settings(
          missing_count = list(label = "Missing"),
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  expect_true("Missing" %in% result$rowlabel1)
  missing_row <- result[result$rowlabel1 == "Missing", ]
  expect_equal(as.numeric(trimws(missing_row$res1)), 2)  # 2 NAs in TRT A
  expect_equal(as.numeric(trimws(missing_row$res2)), 1)  # 1 NA in TRT B
})

test_that("Phase 2 checkpoint: AE table with distinct counts", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AESEV",
        settings = layer_settings(
          distinct_by = "USUBJID",
          format_strings = list(
            n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
          ),
          denoms_by = c("TRTA"),
          total_row = TRUE
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  # Should have severity levels + Total
  expect_true("Total" %in% result$rowlabel1)
  expect_true(nrow(result) >= 4)

  # Result columns should have label attributes
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_true(length(res_cols) >= 3)
  for (col in res_cols) {
    expect_true(!is.null(attr(result[[col]], "label")))
  }
})
