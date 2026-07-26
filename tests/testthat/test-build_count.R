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

# Issue #13: count layer respects cols factor levels for res-column order
test_that("count layer orders res columns by cols factor levels", {
  data(tplyr_adsl, package = "tplyr2")
  d <- tplyr_adsl
  d$TRT01P <- factor(d$TRT01P,
                     levels = c("Xanomeline High Dose", "Xanomeline Low Dose", "Placebo"))

  labels_of <- function(b) {
    res_cols <- grep("^res\\d+$", names(b), value = TRUE)
    vapply(res_cols, function(c) attr(b[[c]], "label"), character(1))
  }

  bc <- tplyr_build(tplyr_spec(cols = "TRT01P",
                               layers = tplyr_layers(group_count("SEX"))), d)
  bd <- tplyr_build(tplyr_spec(cols = "TRT01P",
                               layers = tplyr_layers(group_desc("AGE"))), d)

  # Count must follow factor levels, and match desc on the same spec
  expect_equal(unname(labels_of(bc)),
               c("Xanomeline High Dose (N=84)",
                 "Xanomeline Low Dose (N=84)",
                 "Placebo (N=86)"))
  expect_equal(unname(labels_of(bc)), unname(labels_of(bd)))
})

test_that("count layer falls back to alphabetical when cols is not a factor", {
  data(tplyr_adsl, package = "tplyr2")
  d <- tplyr_adsl
  d$TRT01P <- as.character(d$TRT01P)

  b <- tplyr_build(tplyr_spec(cols = "TRT01P",
                              layers = tplyr_layers(group_count("SEX"))), d)
  res_cols <- grep("^res\\d+$", names(b), value = TRUE)
  labs <- vapply(res_cols, function(c) attr(b[[c]], "label"), character(1))
  expect_equal(unname(labs),
               c("Placebo (N=86)",
                 "Xanomeline High Dose (N=84)",
                 "Xanomeline Low Dose (N=84)"))
})

# Issue #14: '<1%' / '>99%' percent display and zero-count display
test_that("pct_lt renders nonzero percents that round to 0 as '<1'", {
  data(tplyr_adsl, package = "tplyr2")
  d <- tplyr_adsl
  d$FLG <- "Common"
  d$FLG[1] <- "Rare"  # 1 of 254 = 0.39%

  spec <- tplyr_spec(cols = character(0), layers = tplyr_layers(
    group_count("FLG", settings = layer_settings(
      format_strings = list(n_counts = f_str("xxx (xx%)", "n", "pct")),
      pct_lt = 1, pct_gt = 99))))
  b <- tplyr_build(spec, d)

  rare <- trimws(b$res1[b$rowlabel1 == "Rare"])
  common <- trimws(b$res1[b$rowlabel1 == "Common"])
  expect_equal(rare, "1 (<1%)")
  expect_equal(common, "253 (>99%)")
})

test_that("pct_lt leaves percents that round up to the threshold alone", {
  # 0.6 rounds to 1 at integer precision, so it should NOT become '<1'
  fmt <- f_str("xx (xx%)", "n", "pct")
  out <- apply_formats(fmt, c(1L, 1L), c(0.4, 0.6), lt = 1, lt_gt_group = 2)
  expect_true(grepl("<1", out[1]))
  expect_false(grepl("<", out[2]))
  expect_true(grepl("1%", out[2]))
})

test_that("zero_count_display controls how zero cells render", {
  data(tplyr_adsl, package = "tplyr2")

  mk <- function(mode) {
    spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
      group_count("RACE", settings = layer_settings(
        format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct")),
        zero_count_display = mode))))
    tplyr_build(spec, tplyr_adsl)
  }

  full <- mk("full")
  count_only <- mk("count_only")
  blank <- mk("blank")

  # AMERICAN INDIAN OR ALASKA NATIVE has a zero cell under Placebo
  row <- "AMERICAN INDIAN OR ALASKA NATIVE"
  zc_full <- full$res1[full$rowlabel1 == row]
  zc_count <- count_only$res1[count_only$rowlabel1 == row]
  zc_blank <- blank$res1[blank$rowlabel1 == row]

  expect_true(grepl("%", zc_full))
  expect_false(grepl("%", zc_count))
  expect_equal(trimws(zc_count), "0")
  expect_equal(trimws(zc_blank), "")
})

# Issue #24: total/missing rows with a by variable are labelled and placed correctly
test_that("total_row with a by variable labels each group's total and places it last", {
  d <- data.frame(
    TRTP = rep(c("A", "B"), each = 9),
    VISIT = factor(rep(c("Baseline", "Week 2", "Week 12"), 6),
                   levels = c("Baseline", "Week 2", "Week 12")),
    RESP = rep(c("Y", "N", "Y"), 6)
  )
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_count("RESP", by = "VISIT", settings = layer_settings(total_row = TRUE)))), d)
  b <- b[order(b$ord_layer_1), ]

  # Each VISIT group has its own Total row, labelled with the VISIT value
  total_rows <- b[b$rowlabel2 == "Total", ]
  expect_equal(nrow(total_rows), 3)
  expect_setequal(total_rows$rowlabel1, c("Baseline", "Week 2", "Week 12"))
  # No blank by-labels
  expect_false(any(b$rowlabel1 == ""))

  # Within each VISIT, Total is the last row
  for (v in c("Baseline", "Week 2", "Week 12")) {
    grp <- b[b$rowlabel1 == v, ]
    expect_equal(grp$rowlabel2[nrow(grp)], "Total")
  }
})

test_that("special rows sort after normal rows (Missing before Total)", {
  d <- data.frame(
    TRTP = rep(c("A", "B"), each = 6),
    RESP = c("Y", "N", "Y", NA, "Y", "N", "Y", "N", "Y", "Y", NA, "N")
  )
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_count("RESP", settings = layer_settings(
      total_row = TRUE, missing_count = list(label = "Missing"))))), d)
  b <- b[order(b$ord_layer_1), ]
  # Order: category rows, then Missing, then Total
  labs <- b$rowlabel1
  expect_equal(labs[(length(labs) - 1):length(labs)], c("Missing", "Total"))
  expect_true(which(labs == "Missing") < which(labs == "Total"))
})

# Issue #33: missing_count always emits a zero-filled Missing row
test_that("missing_count shows a zero-filled Missing row when there are no missings", {
  d <- data.frame(TRT = rep(c("A", "B"), each = 4),
                  V = c("Completed","Completed","Early","Early",
                        "Completed","Completed","Completed","Early"))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx (xxx%)", "n", "pct")),
      missing_count = list(label = "Missing"))))), d)
  miss <- b[b$rowlabel1 == "Missing", ]
  expect_equal(nrow(miss), 1)
  expect_equal(trimws(miss$res1), "0 (  0%)")
  expect_equal(trimws(miss$res2), "0 (  0%)")
})

test_that("missing_count zero-fills columns with no missings when only some have them", {
  d <- data.frame(TRT = rep(c("A", "B"), each = 4),
                  V = c("Completed", NA, "Early", "Early",
                        "Completed","Completed","Completed","Early"))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx (xxx%)", "n", "pct")),
      missing_count = list(label = "Missing"))))), d)
  miss <- b[b$rowlabel1 == "Missing", ]
  expect_equal(trimws(miss$res1), "1 ( 25%)")   # A has one missing
  expect_equal(trimws(miss$res2), "0 (  0%)")   # B has none -> zero-filled
})

# --- Single-proportion confidence interval keywords (#44) ---

test_that("count layer ci_lower/ci_upper match binom.test on the cell counts", {
  d <- data.frame(
    TRT = rep(c("A", "B"), each = 40),
    V = c(rep("Y", 12), rep("N", 28), rep("Y", 20), rep("N", 20))
  )
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      ci_method = "clopper_pearson",
      format_strings = list(n_counts = f_str(
        "xx (xx.x%) [xx.x, xx.x]", "n", "pct", "ci_lower", "ci_upper")))))), d)

  # Y row: A has 12/40, B has 20/40
  yrow <- b[b$rowlabel1 == "Y", ]
  bt_a <- binom.test(12, 40)$conf.int * 100
  expect_match(yrow$res1,
               sprintf("\\[\\s*%s,\\s*%s\\]", formatC(bt_a[1], format = "f", digits = 1),
                       formatC(bt_a[2], format = "f", digits = 1)))
  bt_b <- binom.test(20, 40)$conf.int * 100
  expect_match(yrow$res2,
               sprintf("\\[\\s*%s,\\s*%s\\]", formatC(bt_b[1], format = "f", digits = 1),
                       formatC(bt_b[2], format = "f", digits = 1)))
})

test_that("count layer wilson keyword matches prop.test", {
  d <- data.frame(TRT = rep("A", 40),
                  V = c(rep("Y", 12), rep("N", 28)))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      ci_method = "wilson",
      format_strings = list(n_counts = f_str(
        "xx [xx.x, xx.x]", "n", "ci_lower", "ci_upper")))))), d)
  yrow <- b[b$rowlabel1 == "Y", ]
  pt <- prop.test(12, 40, correct = FALSE)$conf.int * 100
  expect_match(yrow$res1,
               sprintf("\\[\\s*%s,\\s*%s\\]", formatC(pt[1], format = "f", digits = 1),
                       formatC(pt[2], format = "f", digits = 1)))
})

test_that("distinct_ci keywords use distinct_n / distinct_total", {
  d <- data.frame(
    TRT = rep("A", 6),
    ID = c("s1", "s1", "s2", "s3", "s4", "s5"),   # 5 distinct subjects
    V = c("Y", "Y", "Y", "N", "N", "N")           # Y: subjects s1,s2 -> 2/5
  )
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      distinct_by = "ID",
      ci_method = "clopper_pearson",
      format_strings = list(n_counts = f_str(
        "xx [xx.x, xx.x]", "distinct_n", "distinct_ci_lower",
        "distinct_ci_upper")))))), d)
  yrow <- b[b$rowlabel1 == "Y", ]
  bt <- binom.test(2, 5)$conf.int * 100
  expect_match(yrow$res1,
               sprintf("\\[\\s*%s,\\s*%s\\]",
                       formatC(bt[1], format = "f", digits = 1),
                       formatC(bt[2], format = "f", digits = 1)))
})

test_that("ci_level = 0.90 flows through to the displayed bounds", {
  d <- data.frame(TRT = rep("A", 40), V = c(rep("Y", 12), rep("N", 28)))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      ci_method = "clopper_pearson", ci_level = 0.90,
      format_strings = list(n_counts = f_str(
        "xx [xx.x, xx.x]", "n", "ci_lower", "ci_upper")))))), d)
  yrow <- b[b$rowlabel1 == "Y", ]
  bt <- binom.test(12, 40, conf.level = 0.90)$conf.int * 100
  expect_match(yrow$res1,
               sprintf("\\[\\s*%s,\\s*%s\\]", formatC(bt[1], format = "f", digits = 1),
                       formatC(bt[2], format = "f", digits = 1)))
})

test_that("zero-count and 100% cells format the CI sanely", {
  d <- data.frame(TRT = c(rep("A", 40), rep("B", 40)),
                  V = c(rep("Y", 40), rep("N", 40)))  # A all Y, B all N
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      ci_method = "clopper_pearson",
      format_strings = list(n_counts = f_str(
        "xx (xx.x%) [xx.x, xx.x]", "n", "pct", "ci_lower", "ci_upper")))))), d)
  yrow <- b[b$rowlabel1 == "Y", ]
  # A: 40/40 -> upper 100.0, lower ~91.2 ; B: 0/40 -> lower 0.0
  expect_match(yrow$res1, "\\[91\\.2, 100\\.0\\]")
  expect_match(yrow$res2, "\\[ 0\\.0, ")
})

test_that("Total row carries a CI just like pct", {
  d <- data.frame(TRT = rep("A", 40),
                  V = c(rep("Y", 12), rep("Z", 8), rep("N", 20)))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      total_row = TRUE, ci_method = "clopper_pearson",
      format_strings = list(n_counts = f_str(
        "xx (xx.x%) [xx.x, xx.x]", "n", "pct", "ci_lower", "ci_upper")))))), d)
  trow <- b[b$rowlabel1 == "Total", ]
  expect_equal(nrow(trow), 1)
  # Total is 40/40 -> 100% with an upper bound of 100.0
  expect_match(trow$res1, "\\[91\\.2, 100\\.0\\]")
})

test_that("CI keywords work in stat_columns layout", {
  d <- data.frame(TRT = rep("A", 40), V = c(rep("Y", 12), rep("N", 28)))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      ci_method = "clopper_pearson",
      stat_columns = list(
        "n (%)" = f_str("xx (xx.x%)", "n", "pct"),
        "95% CI" = f_str("[xx.x, xx.x]", "ci_lower", "ci_upper")))))), d)
  yrow <- b[b$rowlabel1 == "Y", ]
  bt <- binom.test(12, 40)$conf.int * 100
  expect_match(yrow$res2,
               sprintf("\\[\\s*%s,\\s*%s\\]", formatC(bt[1], format = "f", digits = 1),
                       formatC(bt[2], format = "f", digits = 1)))
})

test_that("CI is not computed when no format references a CI keyword", {
  d <- data.frame(TRT = rep("A", 40), V = c(rep("Y", 12), rep("N", 28)))
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))))))
  b <- tplyr_build(spec, d)
  nd <- attr(b, "numeric_data")[["1"]]
  expect_false("ci_lower" %in% names(nd))
})

test_that("nested count layer supports CI keywords", {
  d <- data.frame(
    TRT = rep("A", 40),
    BODSYS = rep(c("SYS1", "SYS2"), each = 20),
    PT = c(rep("PT1", 12), rep("PT2", 8), rep("PT3", 10), rep("PT4", 10))
  )
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count(c("BODSYS", "PT"), settings = layer_settings(
      ci_method = "clopper_pearson",
      format_strings = list(n_counts = f_str(
        "xx (xx.x%) [xx.x, xx.x]", "n", "pct", "ci_lower", "ci_upper")))))), d)
  # All cells should carry a bracketed CI
  expect_true(all(grepl("\\[", b$res1[nzchar(trimws(b$res1))])))
})
