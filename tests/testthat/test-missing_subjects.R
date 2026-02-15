# --- Missing subjects tests ---

test_that("missing subjects produces a row when pop_data has extra subjects", {
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
          missing_subjects = TRUE
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  # Missing subjects row should exist
  expect_true("Missing" %in% result$rowlabel1)
})

test_that("missing subjects count is correct", {
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
          format_strings = list(n_counts = f_str("xx", "n")),
          distinct_by = "USUBJID",
          missing_subjects = TRUE
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  missing_row <- result[result$rowlabel1 == "Missing", ]
  expect_equal(nrow(missing_row), 1)

  # A has 1 missing (S4), B has 1 missing (S5)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  a_col <- NULL
  b_col <- NULL
  for (rc in res_cols) {
    lbl <- attr(result[[rc]], "label")
    if (grepl("N=3", lbl)) a_col <- rc  # A has 3 subjects in pop
    if (grepl("N=2", lbl)) b_col <- rc  # B has 2 subjects in pop
  }
  if (!is.null(a_col)) {
    expect_equal(trimws(missing_row[[a_col]]), "1")
  }
  if (!is.null(b_col)) {
    expect_equal(trimws(missing_row[[b_col]]), "1")
  }
})

test_that("missing subjects with custom label", {
  target <- data.frame(
    TRT = c("A"),
    USUBJID = c("S1"),
    VAL = c("X")
  )
  pop <- data.frame(
    TRT = c("A", "A"),
    USUBJID = c("S1", "S2")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = TRUE,
          missing_subjects_label = "Not Reported"
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  expect_true("Not Reported" %in% result$rowlabel1)
})

test_that("missing subjects with zero missing returns zero row", {
  data <- data.frame(
    TRT = c("A", "B"),
    USUBJID = c("S1", "S2"),
    VAL = c("X", "X")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx", "n")),
          distinct_by = "USUBJID",
          missing_subjects = TRUE
        )
      )
    )
  )
  # Same data for pop and target: 0 missing
  result <- tplyr_build(spec, data, pop_data = data)
  missing_row <- result[result$rowlabel1 == "Missing", ]
  expect_equal(nrow(missing_row), 1)
  # Values should be 0
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (rc in res_cols) {
    expect_equal(trimws(missing_row[[rc]]), "0")
  }
})

test_that("missing subjects returns NULL when no pop_data", {
  data <- data.frame(
    TRT = c("A", "B"),
    USUBJID = c("S1", "S2"),
    VAL = c("X", "X")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = TRUE
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  # No pop_data -> no missing subjects row
  expect_false("Missing" %in% result$rowlabel1)
})

test_that("missing subjects without distinct_by uses row-level", {
  target <- data.frame(
    TRT = c("A", "A", "B"),
    VAL = c("X", "Y", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B", "B"),
    VAL = c("X", "Y", "Z", "W", "X", "Y", "Z")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx", "n")),
          missing_subjects = TRUE
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  missing_row <- result[result$rowlabel1 == "Missing", ]
  expect_equal(nrow(missing_row), 1)
  # A: 4 pop rows - 2 target rows = 2 missing
  # B: 3 pop rows - 1 target row = 2 missing
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  a_col <- NULL
  b_col <- NULL
  for (rc in res_cols) {
    lbl <- attr(result[[rc]], "label")
    if (grepl("N=4", lbl)) a_col <- rc
    if (grepl("N=3", lbl)) b_col <- rc
  }
  if (!is.null(a_col)) {
    expect_equal(trimws(missing_row[[a_col]]), "2")
  }
  if (!is.null(b_col)) {
    expect_equal(trimws(missing_row[[b_col]]), "2")
  }
})

test_that("missing_subjects = FALSE does not add row", {
  target <- data.frame(
    TRT = c("A"),
    USUBJID = c("S1"),
    VAL = c("X")
  )
  pop <- data.frame(
    TRT = c("A", "A"),
    USUBJID = c("S1", "S2")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = FALSE
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  expect_false("Missing" %in% result$rowlabel1)
})
