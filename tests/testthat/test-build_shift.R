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

# Issue #13: shift layer orders res columns by the shift column's factor levels
test_that("shift res columns follow the column variable's factor levels", {
  shift_data <- data.frame(
    TRT = rep("A", 9),
    BNRIND = factor(rep(c("L", "N", "H"), each = 3), levels = c("L", "N", "H")),
    # Non-alphabetical factor order for the shift (column) variable
    ANRIND = factor(rep(c("H", "N", "L"), times = 3), levels = c("H", "N", "L"))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_shift(c(row = "BNRIND", column = "ANRIND")))
  )
  result <- tplyr_build(spec, shift_data)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labs <- vapply(res_cols, function(c) attr(result[[c]], "label"), character(1))
  # Column labels should be ordered H, N, L (the ANRIND factor levels)
  expect_true(grepl("H", labs[1]))
  expect_true(grepl("N", labs[2]))
  expect_true(grepl("L", labs[3]))
})

# Issue #18: column-wise (per-shift-column) denominator
test_that("shift_denom='column' gives per-from-group percentages and header N", {
  d <- data.frame(
    TRT  = "A",
    BASE = c(rep("N", 8), rep("H", 2)),
    POST = c(rep("N", 7), "H", "H", "H")
  )
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_shift(c(row = "POST", column = "BASE"), settings = layer_settings(
      shift_denom = "column",
      format_strings = list(n_counts = f_str("xx (xxx%)", "n", "pct")))))), d)

  res_cols <- grep("^res\\d+$", names(b), value = TRUE)
  labs <- vapply(res_cols, function(c) attr(b[[c]], "label"), character(1))
  # Header N reflects per-baseline-group sizes: H -> 2, N -> 8
  expect_true(any(grepl("\\(N=2\\)", labs)))
  expect_true(any(grepl("\\(N=8\\)", labs)))

  # H-baseline column: 2/2 = 100%; N-baseline column: 7/8 = 88%
  h_col <- res_cols[grepl("H", labs)]
  n_col <- res_cols[grepl("\\| N", labs)]
  expect_equal(trimws(b[[h_col]][b$rowlabel1 == "H"]), "2 (100%)")
  expect_equal(trimws(b[[n_col]][b$rowlabel1 == "N"]), "7 ( 88%)")
})

test_that("shift default denom (total) is unchanged", {
  d <- data.frame(
    TRT  = "A",
    BASE = c(rep("N", 8), rep("H", 2)),
    POST = c(rep("N", 7), "H", "H", "H")
  )
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_shift(c(row = "POST", column = "BASE"), settings = layer_settings(
      format_strings = list(n_counts = f_str("xx (xxx%)", "n", "pct")))))), d)
  res_cols <- grep("^res\\d+$", names(b), value = TRUE)
  labs <- vapply(res_cols, function(c) attr(b[[c]], "label"), character(1))
  # Arm total denominator = 10 for every column
  expect_true(all(grepl("\\(N=10\\)", labs)))
})

# Coverage: shift edge branches
test_that("group_shift errors when target_var lacks row/column names", {
  expect_error(group_shift(c("BR", "AR")), "row.*column|names")
})

test_that("shift supports distinct_by, denom_where, denom_ignore, and a where filter", {
  d <- data.frame(
    TRT = rep("A", 8),
    ID  = rep(1:4, each = 2),
    KEEP = rep(c("y", "n"), 4),
    BR = rep(c("N", "H"), each = 4),
    AR = rep(c("N", "H"), 4)
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_shift(c(row = "AR", column = "BR"), where = KEEP == "y",
      settings = layer_settings(
        distinct_by = "ID",
        denom_where = quote(KEEP == "y"),
        denom_ignore = "X"))))
  result <- tplyr_build(spec, d)
  expect_true(any(grepl("^res\\d+$", names(result))))
})

test_that("shift works with a by variable and no spec columns", {
  d <- data.frame(
    SEX = factor(rep(c("F", "M"), each = 6), levels = c("M", "F")),
    BR = rep(c("N", "H"), 6),
    AR = rep(c("N", "H", "N"), 4)
  )
  spec <- tplyr_spec(cols = character(0), layers = tplyr_layers(
    group_shift(c(row = "AR", column = "BR"), by = "SEX")))
  result <- tplyr_build(spec, d)
  expect_true("rowlabel1" %in% names(result))
  expect_true(any(grepl("^res\\d+$", names(result))))
})

# Issue #28: shift_denom="column" scopes the denominator within each by group
test_that("shift_denom='column' with a by variable uses per-by-group denominators", {
  d <- data.frame(TRT = "A",
    VISIT = c(rep("V1", 5), rep("V2", 3)),
    BASE  = c("N","N","N","N","H", "N","N","H"),
    POST  = c("N","N","N","H","H", "N","H","H"))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_shift(c(row = "POST", column = "BASE"), by = "VISIT",
      settings = layer_settings(shift_denom = "column",
        format_strings = list(n_counts = f_str("xx (xxx%)", "n", "pct")))))), d)

  res_cols <- grep("^res\\d+$", names(b), value = TRUE)
  labs <- vapply(res_cols, function(c) attr(b[[c]], "label"), character(1))
  n_col <- res_cols[grepl("\\| N", labs)]

  # V1 Normal-baseline denom = 4 (3 stay N -> 75%); V2 = 2 (1 stays N -> 50%)
  v1_nn <- b[[n_col]][b$rowlabel1 == "V1" & b$rowlabel2 == "N"]
  v2_nn <- b[[n_col]][b$rowlabel1 == "V2" & b$rowlabel2 == "N"]
  expect_equal(trimws(v1_nn), "3 ( 75%)")
  expect_equal(trimws(v2_nn), "1 ( 50%)")

  # Header falls back to the arm N (a single header can't show per-by-group N)
  expect_false(any(grepl("\\(N=6\\)", labs)))
})

# Issue #31: shift layers honor zero_count_display (like group_count)
test_that("group_shift honors zero_count_display", {
  d <- data.frame(TRT = "A", BASE = c("N", "N", "H"), POST = c("N", "H", "H"))

  mk <- function(mode) {
    spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_shift(c(row = "POST", column = "BASE"), settings = layer_settings(
        zero_count_display = mode,
        format_strings = list(n_counts = f_str("xx (xxx%)", "n", "pct"))))))
    tplyr_build(spec, d)
  }

  res_of <- function(b, from, to) {
    rc <- grep("^res\\d+$", names(b), value = TRUE)
    labs <- vapply(rc, function(c) attr(b[[c]], "label"), character(1))
    col <- rc[grepl(paste0("\\| ", from, " "), labs)]  # label: "A | <from> (N=n)"
    trimws(b[[col]][b$rowlabel1 == to])
  }

  full <- mk("full")
  count_only <- mk("count_only")
  blank <- mk("blank")

  # Cell from=H (baseline) to=N (post) is zero (the one High-baseline subject
  # stayed High), with the arm total (3) as the default denominator.
  expect_equal(res_of(full, "H", "N"), "0 (  0%)")
  expect_equal(res_of(count_only, "H", "N"), "0")
  expect_equal(res_of(blank, "H", "N"), "")
})

test_that("group_shift honors pct_lt threshold", {
  # 1 of 254 baseline-Normal subjects -> 0.39% -> "<1"
  d <- data.frame(TRT = "A",
                  BASE = rep("N", 254),
                  POST = c("H", rep("N", 253)))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_shift(c(row = "POST", column = "BASE"), settings = layer_settings(
      pct_lt = 1,
      format_strings = list(n_counts = f_str("xxx (xxx%)", "n", "pct")))))), d)
  rc <- grep("^res\\d+$", names(b), value = TRUE)
  hcol <- rc[1]
  expect_true(any(grepl("<1", b[[hcol]])))
})

# Issue #35: denom_row emits the per-column-group denominator as an integer row
test_that("group_shift denom_row emits an n row above the shift-to rows", {
  df <- data.frame(
    TRTP = factor(rep("A", 12), levels = "A"),
    BNRIND = factor(c(rep("N", 8), rep("H", 4)), levels = c("N", "H")),
    ANRIND = factor(c("N","N","H","N","N","N","H","H","H","N","H","H"), levels = c("N", "H")))
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_shift(c(row = "ANRIND", column = "BNRIND"), settings = layer_settings(
      shift_denom = "column", denom_row = TRUE,
      format_strings = list(n_counts = f_str("xx (xxx%)", "n", "pct")))))), df)
  # Capture labels before row subsetting (which drops column attributes)
  rc <- grep("^res\\d+$", names(b), value = TRUE)
  labs <- vapply(rc, function(c) attr(b[[c]], "label"), character(1))
  n_col <- rc[grepl("\\| N ", labs)]
  h_col <- rc[grepl("\\| H ", labs)]
  b <- b[order(b$ord_layer_1), ]
  # First row is the denom row, labelled "n", with the baseline group sizes
  expect_equal(b$rowlabel1[1], "n")
  expect_equal(trimws(b[[n_col]][1]), "8")
  expect_equal(trimws(b[[h_col]][1]), "4")
})

# Issue #55: absent baseline group must render 0 (not literal "NA"), and the
# denom row can carry its own format independent of the n_counts width.
.denom_row_absent_data <- function() {
  d <- data.frame(
    TRTP   = factor(rep(c("Pbo", "Act"), each = 8), levels = c("Pbo", "Act")),
    PARAM  = rep(c("A", "B"), 8),
    BNRIND = factor(c("N","N","H","N","N","N","H","N", "N","N","H","N","N","N","H","N"),
                    levels = c("N", "H")),
    ANRIND = factor(c("N","H","H","N","N","H","N","N", "N","H","H","N","N","H","N","N"),
                    levels = c("N", "H")),
    stringsAsFactors = FALSE)
  d$BNRIND[d$PARAM == "B"] <- "N"   # PARAM B: baseline High entirely absent
  d
}

test_that("denom_row zero-fills an absent baseline group instead of NA (#55)", {
  d <- .denom_row_absent_data()
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_shift(c(row = "ANRIND", column = "BNRIND"), by = "PARAM",
      settings = layer_settings(
        shift_denom = "column", zero_count_display = "count_only",
        format_strings = list(n_counts = f_str("xx(xxx%)", "n", "pct")),
        denom_row = TRUE, denom_row_label = "n")))), d)

  res_cols <- grep("^res\\d+$", names(b), value = TRUE)
  denom <- b[trimws(b$rowlabel2) == "n", ]
  cells <- unlist(lapply(res_cols, function(rc) trimws(denom[[rc]])))
  # never the literal string "NA"; the absent (PARAM B, High) group reads 0
  expect_false(any(cells == "NA"))
  b_row <- denom[trimws(denom$rowlabel1) == "B", ]
  expect_true(any(vapply(res_cols, function(rc) trimws(b_row[[rc]]) == "0", logical(1))))
})

test_that("denom_row_format gives the n row its own width (#55)", {
  d <- .denom_row_absent_data()
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_shift(c(row = "ANRIND", column = "BNRIND"), by = "PARAM",
      settings = layer_settings(
        shift_denom = "column", zero_count_display = "count_only",
        format_strings = list(n_counts = f_str("xx(xxx%)", "n", "pct")),
        denom_row = TRUE, denom_row_label = "n",
        denom_row_format = f_str("xx", "n"))))), d)

  res_cols <- grep("^res\\d+$", names(b), value = TRUE)
  denom <- b[trimws(b$rowlabel2) == "n", ]
  # 2-char field (independent of the 8-char n_counts cells), right-justified
  widths <- unlist(lapply(res_cols, function(rc) nchar(denom[[rc]])))
  expect_true(all(widths == 2))
  b_row <- denom[trimws(denom$rowlabel1) == "B", ]
  vals <- vapply(res_cols, function(rc) b_row[[rc]], character(1))
  expect_true(all(vals %in% c(" 2", " 4", " 0")))
})

test_that("denom_row_format must be a single-variable f_str (#55)", {
  d <- .denom_row_absent_data()
  expect_error(
    tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
      group_shift(c(row = "ANRIND", column = "BNRIND"),
        settings = layer_settings(denom_row = TRUE,
          denom_row_format = "xx")))), d),
    "denom_row_format must be an f_str"
  )
  expect_error(
    tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
      group_shift(c(row = "ANRIND", column = "BNRIND"),
        settings = layer_settings(denom_row = TRUE,
          denom_row_format = f_str("xx (xx)", "n", "pct"))))), d),
    "exactly one"
  )
})
