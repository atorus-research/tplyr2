test_that("shift layer produces correct output structure", {
  shift_data <- data.frame(
    TRT = rep(c("A", "B"), each = 9),
    BNRIND = factor(rep(c("L", "N", "H"), each = 3, times = 2), levels = c("L", "N", "H")),
    ANRIND = factor(rep(c("L", "N", "H"), times = 6), levels = c("L", "N", "H"))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"))
    )
  )
  result <- tplyr_build(spec, shift_data)

  expect_s3_class(result, "data.frame")
  expect_true("rowlabel1" %in% names(result))
  expect_true(any(grepl("^res\\d+$", names(result))))
  expect_true("ord_layer_index" %in% names(result))

  # Should have 3 rows (L, N, H) for BNRIND
  expect_equal(nrow(result), 3)

  # Should have 6 res columns: 2 TRT × 3 ANRIND
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_equal(length(res_cols), 6)
})

test_that("shift count accuracy matches manual cross-tabulation", {
  shift_data <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B", "B"),
    BNRIND = c("L", "L", "N", "H", "L", "N", "N"),
    ANRIND = c("L", "N", "H", "H", "L", "N", "H")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, shift_data)

  # TRT A: BNRIND=L, ANRIND=L → 1; BNRIND=L, ANRIND=N → 1
  l_row <- result[result$rowlabel1 == "L", ]
  # Find which res cols correspond to A_L, A_N, etc.
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))

  a_l_col <- res_cols[grepl("^A \\| L", labels)]
  a_n_col <- res_cols[grepl("^A \\| N", labels)]
  a_h_col <- res_cols[grepl("^A \\| H", labels)]

  expect_equal(as.numeric(trimws(l_row[[a_l_col]])), 1)
  expect_equal(as.numeric(trimws(l_row[[a_n_col]])), 1)
  expect_equal(as.numeric(trimws(l_row[[a_h_col]])), 0)
})

test_that("shift data completion fills all row × col × treatment combos", {
  shift_data <- data.frame(
    TRT = c("A", "B"),
    BNRIND = c("L", "H"),
    ANRIND = c("N", "L")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, shift_data)

  # Should have rows for both L and H (all row_var values)
  expect_true("L" %in% result$rowlabel1)
  expect_true("H" %in% result$rowlabel1)

  # No NAs in result columns
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    expect_false(any(is.na(result[[col]])))
  }
})

test_that("shift factor ordering is respected", {
  shift_data <- data.frame(
    TRT = c("A", "A", "A"),
    BNRIND = factor(c("H", "N", "L"), levels = c("L", "N", "H")),
    ANRIND = factor(c("L", "N", "H"), levels = c("L", "N", "H"))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, shift_data)

  # Row order should follow factor levels: L, N, H
  expect_equal(result$rowlabel1, c("L", "N", "H"))
})

test_that("shift column label attributes", {
  shift_data <- data.frame(
    TRT = rep(c("A", "B"), each = 3),
    BNRIND = rep(c("L", "N", "H"), 2),
    ANRIND = rep(c("L", "N", "H"), 2)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, shift_data)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    lbl <- attr(result[[col]], "label")
    expect_true(!is.null(lbl))
    expect_true(nchar(lbl) > 0)
    # Labels should be TRT | ANRIND (N=n) format
    expect_true(grepl(" \\| ", lbl))
    expect_true(grepl("\\(N=\\d+\\)", lbl))
  }
})

test_that("shift denominator control", {
  shift_data <- data.frame(
    TRT = c("A", "A", "A", "A"),
    BNRIND = c("L", "L", "N", "H"),
    ANRIND = c("L", "N", "H", "H")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx.x", "pct"))
        )
      )
    )
  )
  result <- tplyr_build(spec, shift_data)

  # Default denom = by TRT (4 obs in TRT A)
  # BNRIND=L, ANRIND=L: 1/4 = 25%
  l_row <- result[result$rowlabel1 == "L", ]
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  a_l_col <- res_cols[grepl("^A \\| L", labels)]
  expect_equal(as.numeric(trimws(l_row[[a_l_col]])), 25.0, tolerance = 0.1)
})

test_that("shift with by variables", {
  shift_data <- data.frame(
    TRT = c("A", "A", "A", "A"),
    PARAM = c("LAB1", "LAB1", "LAB2", "LAB2"),
    BNRIND = c("L", "N", "L", "N"),
    ANRIND = c("N", "H", "L", "N")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        by = "PARAM",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, shift_data)

  expect_true("rowlabel1" %in% names(result))
  expect_true("rowlabel2" %in% names(result))
  # rowlabel1 = PARAM value, rowlabel2 = BNRIND value
  expect_true("LAB1" %in% result$rowlabel1)
  expect_true("LAB2" %in% result$rowlabel1)
})

test_that("shift format strings", {
  shift_data <- data.frame(
    TRT = rep("A", 4),
    BNRIND = c("L", "L", "N", "H"),
    ANRIND = c("L", "N", "H", "H")
  )
  # n (pct) format
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
        )
      )
    )
  )
  result <- tplyr_build(spec, shift_data)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  # Values should contain parentheses
  for (col in res_cols) {
    vals <- result[[col]]
    non_empty <- vals[trimws(vals) != "0 ( 0.0%)"]
    if (length(non_empty) > 0) {
      expect_true(any(grepl("\\(", non_empty)))
    }
  }
})

test_that("shift validation: unnamed target_var errors", {
  expect_error(
    group_shift(c("BNRIND", "ANRIND")),
    "must have names"
  )
})

test_that("shift validation: wrong names errors", {
  expect_error(
    group_shift(c(a = "BNRIND", b = "ANRIND")),
    "must have names 'row' and 'column'"
  )
})

test_that("shift validation: wrong length errors", {
  expect_error(
    group_shift(c(row = "BNRIND")),
    "must be a character vector of length 2"
  )
})

test_that("shift integrates with multiple layers", {
  data(tplyr_adsl, package = "tplyr2")
  shift_data <- data.frame(
    TRT01P = rep(c("Placebo", "Xanomeline High Dose"), each = 4),
    BNRIND = c("L", "L", "N", "H", "L", "N", "N", "H"),
    ANRIND = c("L", "N", "H", "H", "N", "N", "H", "L")
  )
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX"),
      group_shift(c(row = "BNRIND", column = "ANRIND"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )

  # Count layer uses tplyr_adsl, shift needs shift_data —
  # but both use the same data argument. Use shift_data with
  # a mock SEX column
  combined_data <- data.frame(
    TRT01P = c("A", "A", "B", "B"),
    SEX = c("M", "F", "M", "F"),
    BNRIND = c("L", "N", "L", "H"),
    ANRIND = c("N", "H", "L", "N")
  )

  result <- tplyr_build(spec, combined_data)

  # Should have rows from both layers
  expect_true(any(result$ord_layer_index == 1))  # Count layer
  expect_true(any(result$ord_layer_index == 2))  # Shift layer
})
