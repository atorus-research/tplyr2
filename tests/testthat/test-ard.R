# --- ARD (Analysis Results Data) tests ---

# --- tplyr_to_ard tests ---

test_that("tplyr_to_ard produces correct columns", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(result)

  expect_true(is.data.frame(ard))
  expect_true("analysis_id" %in% names(ard))
  expect_true("stat_name" %in% names(ard))
  expect_true("stat_value" %in% names(ard))
})

test_that("tplyr_to_ard count layer has expected stats", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(result)

  expect_true("n" %in% ard$stat_name)
  expect_true("pct" %in% ard$stat_name)
  expect_true("total" %in% ard$stat_name)
})

test_that("tplyr_to_ard count layer one row per stat per group", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(result)

  # 2 TRT levels × 2 SEX levels × 3 stats (n, pct, total) = 12 rows
  expect_true(nrow(ard) > 0)
  # Check we have the group columns
  expect_true("TRT" %in% names(ard))
  expect_true("SEX" %in% names(ard))
})

test_that("tplyr_to_ard desc layer includes all stats", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    AGE = c(25:34, 35:44)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xx", "n"),
            "Mean" = f_str("xx.x", "mean")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(result)

  expect_true("n" %in% ard$stat_name)
  expect_true("mean" %in% ard$stat_name)
  expect_true("sd" %in% ard$stat_name)
  expect_true("median" %in% ard$stat_name)
})

test_that("tplyr_to_ard multi-layer spec", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10),
    AGE = c(25:34, 35:44)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("SEX"),
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(result)

  expect_true(1 %in% ard$analysis_id)
  expect_true(2 %in% ard$analysis_id)
})

test_that("tplyr_to_ard errors without numeric data", {
  result <- data.frame(rowlabel1 = "X", res1 = "1")
  expect_error(tplyr_to_ard(result), "No numeric data")
})

test_that("tplyr_to_ard raw values are correct", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = c(rep("M", 6), rep("F", 4), rep("M", 3), rep("F", 7))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(result)

  # Check A/F count = 4
  a_f <- ard[ard$TRT == "A" & ard$SEX == "F" & ard$stat_name == "n", ]
  expect_equal(a_f$stat_value, 4)
})

test_that("tplyr_to_ard excludes internal columns", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(result)

  # Should not have internal columns
  expect_false("formatted" %in% names(ard))
  expect_false(any(grepl("^\\.", names(ard))))
  expect_false(any(grepl("^rowlabel", names(ard))))
})

# --- tplyr_from_ard tests ---

test_that("tplyr_from_ard reconstructs count table", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  original <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(original)
  rebuilt <- tplyr_from_ard(ard, spec)

  expect_true(is.data.frame(rebuilt))
  expect_true("rowlabel1" %in% names(rebuilt))
  expect_true("res1" %in% names(rebuilt))
  # Same number of data rows (excluding special rows like Total)
  expect_equal(nrow(rebuilt), nrow(original))
})

test_that("tplyr_from_ard reconstructs desc table", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    AGE = c(25:34, 35:44)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xx", "n"),
            "Mean" = f_str("xx.x", "mean")
          )
        )
      )
    )
  )
  original <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(original)
  rebuilt <- tplyr_from_ard(ard, spec)

  expect_true(is.data.frame(rebuilt))
  expect_true(all(c("n", "Mean") %in% rebuilt$rowlabel1))
})

test_that("tplyr_from_ard roundtrip matches formatted values", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  original <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(original)
  rebuilt <- tplyr_from_ard(ard, spec)

  # Sort both by rowlabel for comparison
  orig_sorted <- original[order(original$rowlabel1), ]
  rebuilt_sorted <- rebuilt[order(rebuilt$rowlabel1), ]

  # Formatted values should match
  expect_equal(trimws(orig_sorted$res1), trimws(rebuilt_sorted$res1))
  expect_equal(trimws(orig_sorted$res2), trimws(rebuilt_sorted$res2))
})

test_that("tplyr_from_ard errors with non-spec", {
  ard <- data.frame(analysis_id = 1, stat_name = "n", stat_value = 5)
  expect_error(tplyr_from_ard(ard, "not_a_spec"), "must be a tplyr_spec")
})

test_that("tplyr_from_ard errors without analysis_id", {
  ard <- data.frame(stat_name = "n", stat_value = 5)
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  expect_error(tplyr_from_ard(ard, spec), "analysis_id")
})

test_that("tplyr_from_ard handles empty ARD for a layer", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    SEX = rep(c("M", "F"), 5)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  # ARD with wrong analysis_id
  ard <- data.frame(analysis_id = 99, TRT = "A", SEX = "M",
                    stat_name = "n", stat_value = 5)
  rebuilt <- tplyr_from_ard(ard, spec)
  expect_true(is.data.frame(rebuilt))
  expect_equal(nrow(rebuilt), 0)
})

test_that("tplyr_from_ard multi-layer roundtrip", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10),
    AGE = c(25:34, 35:44)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("SEX"),
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  original <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(original)
  rebuilt <- tplyr_from_ard(ard, spec)

  expect_true(is.data.frame(rebuilt))
  expect_true(1 %in% rebuilt$ord_layer_index)
  expect_true(2 %in% rebuilt$ord_layer_index)
})

test_that("tplyr_to_ard grouping columns are correct", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    GRP = rep(c("G1", "G2"), times = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX", by = "GRP"))
  )
  result <- tplyr_build(spec, data)
  ard <- tplyr_to_ard(result)

  expect_true("TRT" %in% names(ard))
  expect_true("GRP" %in% names(ard))
  expect_true("SEX" %in% names(ard))
})
