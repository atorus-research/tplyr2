test_that("nested count produces correct output structure", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count(c("AEBODSYS", "AEDECOD"))
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  expect_s3_class(result, "data.frame")
  expect_true("rowlabel1" %in% names(result))
  expect_true("rowlabel2" %in% names(result))
  expect_true(any(grepl("^res\\d+$", names(result))))
  expect_true("ord_layer_index" %in% names(result))
  expect_true("ord_layer_1" %in% names(result))
  expect_true("ord_layer_2" %in% names(result))
})

test_that("nested count row labels are correct", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "A"),
    SOC = c("CARDIAC", "CARDIAC", "GI", "GI"),
    PT  = c("AFIB", "TACHY", "NAUSEA", "VOMIT")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Outer rows (SOC level): rowlabel1 has SOC value, rowlabel2 is blank
  outer_rows <- result[result$rowlabel2 == "", ]
  expect_true(nrow(outer_rows) >= 2)
  expect_true("CARDIAC" %in% outer_rows$rowlabel1)
  expect_true("GI" %in% outer_rows$rowlabel1)

  # Inner rows (PT level): rowlabel1 has SOC, rowlabel2 has clean PT (no indentation)
  inner_rows <- result[result$rowlabel2 != "", ]
  expect_true(nrow(inner_rows) >= 4)
  # Inner values should NOT be indented in raw output
  expect_true(all(!grepl("^\\s", inner_rows$rowlabel2)))
})

test_that("nested count accuracy: outer = sum of inner", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B"),
    SOC = c("CARDIAC", "CARDIAC", "GI", "CARDIAC", "GI"),
    PT  = c("AFIB", "TACHY", "NAUSEA", "AFIB", "NAUSEA")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # For TRT A, CARDIAC: outer = 2 (AFIB + TACHY)
  cardiac_outer <- result[result$rowlabel1 == "CARDIAC" & result$rowlabel2 == "", ]
  expect_equal(as.numeric(trimws(cardiac_outer$res1)), 2)

  # Inner rows for CARDIAC in TRT A: AFIB=1, TACHY=1
  cardiac_inner <- result[result$rowlabel1 == "CARDIAC" & result$rowlabel2 != "", ]
  inner_sum <- sum(as.numeric(trimws(cardiac_inner$res1)))
  expect_equal(inner_sum, 2)
})

test_that("nested count with distinct_by", {
  test_data <- data.frame(
    TRT  = c("A", "A", "A", "A", "B", "B"),
    SOC  = c("CARDIAC", "CARDIAC", "CARDIAC", "GI", "CARDIAC", "GI"),
    PT   = c("AFIB", "AFIB", "TACHY", "NAUSEA", "AFIB", "NAUSEA"),
    SUBJ = c("S1", "S1", "S2", "S3", "S4", "S5")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
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

  # TRT A, CARDIAC outer: 2 distinct subjects (S1, S2)
  cardiac_outer <- result[result$rowlabel1 == "CARDIAC" & result$rowlabel2 == "", ]
  expect_equal(as.numeric(trimws(cardiac_outer$res1)), 2)

  # TRT A, CARDIAC > AFIB: 1 distinct subject (S1, counted once despite 2 rows)
  afib_row <- result[result$rowlabel1 == "CARDIAC" & grepl("AFIB", result$rowlabel2), ]
  expect_equal(as.numeric(trimws(afib_row$res1)), 1)
})

test_that("nested count default denominator", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    SOC = c("CARDIAC", "CARDIAC", "GI", "CARDIAC", "GI", "GI"),
    PT  = c("AFIB", "TACHY", "NAUSEA", "AFIB", "NAUSEA", "VOMIT")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          format_strings = list(
            n_counts = f_str("xx.x", "pct")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Default denom = by TRT (3 each)
  # TRT A, CARDIAC outer: 2/3 = 66.7%
  cardiac_outer <- result[result$rowlabel1 == "CARDIAC" & result$rowlabel2 == "", ]
  expect_equal(as.numeric(trimws(cardiac_outer$res1)), 66.7, tolerance = 0.1)
})

test_that("nested count per-level denominator", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    SOC = c("CARDIAC", "CARDIAC", "GI", "CARDIAC", "GI", "GI"),
    PT  = c("AFIB", "TACHY", "NAUSEA", "AFIB", "NAUSEA", "VOMIT")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          format_strings = list(
            n_counts = f_str("xx.x", "pct")
          ),
          denoms_by = list(
            c("TRT"),              # Level 1: % of arm
            c("TRT", "SOC")        # Level 2: % of body system
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Level 2: TRT A, CARDIAC, AFIB: 1/2 = 50% (denom = CARDIAC count in TRT A = 2)
  afib_row <- result[result$rowlabel1 == "CARDIAC" & grepl("AFIB", result$rowlabel2), ]
  expect_equal(as.numeric(trimws(afib_row$res1)), 50.0, tolerance = 0.1)
})

test_that("nested count data completion produces no NAs", {
  test_data <- data.frame(
    TRT = c("A", "A", "B"),
    SOC = c("CARDIAC", "CARDIAC", "CARDIAC"),
    PT  = c("AFIB", "TACHY", "AFIB")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    expect_false(any(is.na(result[[col]])))
  }
})

test_that("nested count does not create impossible parent-child combos", {
  test_data <- data.frame(
    TRT = c("A", "A", "B"),
    SOC = c("CARDIAC", "GI", "CARDIAC"),
    PT  = c("AFIB", "NAUSEA", "AFIB")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # AFIB should only appear under CARDIAC, not under GI
  gi_inner <- result[result$rowlabel1 == "GI" & result$rowlabel2 != "", ]
  expect_false(any(grepl("AFIB", gi_inner$rowlabel2)))

  # NAUSEA should only appear under GI, not under CARDIAC
  cardiac_inner <- result[result$rowlabel1 == "CARDIAC" & result$rowlabel2 != "", ]
  expect_false(any(grepl("NAUSEA", cardiac_inner$rowlabel2)))
})

test_that("nested count correct row interleaving order", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "A"),
    SOC = c("CARDIAC", "CARDIAC", "GI", "GI"),
    PT  = c("AFIB", "TACHY", "NAUSEA", "VOMIT")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Verify interleaving: outer row comes before its inner rows
  cardiac_idx <- which(result$rowlabel1 == "CARDIAC" & result$rowlabel2 == "")
  cardiac_inner_idx <- which(result$rowlabel1 == "CARDIAC" & result$rowlabel2 != "")
  expect_true(all(cardiac_inner_idx > cardiac_idx))

  gi_idx <- which(result$rowlabel1 == "GI" & result$rowlabel2 == "")
  gi_inner_idx <- which(result$rowlabel1 == "GI" & result$rowlabel2 != "")
  expect_true(all(gi_inner_idx > gi_idx))

  # CARDIAC group should come before GI group (alphabetical)
  expect_true(max(cardiac_inner_idx) < gi_idx)
})

test_that("nested count total row", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B"),
    SOC = c("CARDIAC", "CARDIAC", "GI", "CARDIAC", "GI"),
    PT  = c("AFIB", "TACHY", "NAUSEA", "AFIB", "NAUSEA")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          total_row = TRUE,
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  expect_true("Total" %in% result$rowlabel1)
  total_row <- result[result$rowlabel1 == "Total", ]
  expect_equal(as.numeric(trimws(total_row$res1)), 3)  # TRT A: 3 total
  expect_equal(as.numeric(trimws(total_row$res2)), 2)  # TRT B: 2 total
})

test_that("nested count with by variables", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "A"),
    SOC = c("CARDIAC", "CARDIAC", "GI", "GI"),
    PT  = c("AFIB", "TACHY", "NAUSEA", "VOMIT")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        by = "AE Summary",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Should have 3 rowlabel columns: by label, SOC, PT
  expect_true("rowlabel1" %in% names(result))
  expect_true("rowlabel2" %in% names(result))
  expect_true("rowlabel3" %in% names(result))
  expect_true(all(result$rowlabel1 == "AE Summary"))
})

test_that("nested count keep_levels filters outermost", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "A"),
    SOC = c("CARDIAC", "CARDIAC", "GI", "GI"),
    PT  = c("AFIB", "TACHY", "NAUSEA", "VOMIT")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          keep_levels = "CARDIAC",
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  expect_true(all(result$rowlabel1 == "CARDIAC"))
  expect_false("GI" %in% result$rowlabel1)
})

test_that("single target_var backward compatibility", {
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
  expect_true(nrow(result) >= 2)
})

test_that("nested count with tplyr_adae produces realistic results", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count(c("AEBODSYS", "AEDECOD"),
        settings = layer_settings(
          distinct_by = "USUBJID",
          format_strings = list(
            n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
          ),
          denoms_by = c("TRTA")
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  # Should have body system rows and preferred term rows
  outer_rows <- result[result$rowlabel2 == "", ]
  inner_rows <- result[result$rowlabel2 != "", ]
  expect_true(nrow(outer_rows) >= 5)
  expect_true(nrow(inner_rows) >= 10)

  # Result columns should have labels
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_true(length(res_cols) >= 3)
  for (col in res_cols) {
    expect_true(!is.null(attr(result[[col]], "label")))
  }
})

test_that("nested count result columns carry label attributes", {
  test_data <- data.frame(
    TRT = c("A", "B"),
    SOC = c("CARDIAC", "GI"),
    PT  = c("AFIB", "NAUSEA")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count(c("SOC", "PT"),
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    lbl <- attr(result[[col]], "label")
    expect_true(!is.null(lbl))
    expect_true(nchar(lbl) > 0)
  }
})
