# --- Risk difference tests ---

test_that("basic risk difference produces rdiff column", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 50),
    VAL = c(
      rep("X", 30), rep("Y", 20),  # A: X=30, Y=20
      rep("X", 20), rep("Y", 30)   # B: X=20, Y=30
    )
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          risk_diff = list(
            comparisons = list(c("A", "B")),
            format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_true("rdiff1" %in% names(result))
  expect_true(!is.null(attr(result$rdiff1, "label")))
  expect_equal(attr(result$rdiff1, "label"), "A vs B")
})

test_that("risk difference values are correct", {
  # Construct data with known proportions
  data <- data.frame(
    TRT = rep(c("Trt", "Ctrl"), each = 100),
    VAL = c(
      rep("Yes", 60), rep("No", 40),  # Trt: 60%
      rep("Yes", 40), rep("No", 60)   # Ctrl: 40%
    )
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          risk_diff = list(
            comparisons = list(c("Trt", "Ctrl")),
            format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)

  # Check the "Yes" row has rdiff = 20.0% (60% - 40%)
  yes_row <- result[result$rowlabel1 == "Yes", ]
  rd_val <- str_extract_num(yes_row$rdiff1, 1)
  expect_equal(rd_val, 20.0, tolerance = 0.1)
})

test_that("risk difference with default format", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 20),
    VAL = c(rep("X", 15), rep("Y", 5), rep("X", 10), rep("Y", 10))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          risk_diff = list(
            comparisons = list(c("A", "B"))
            # format not specified, should use default
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_true("rdiff1" %in% names(result))
  # Default format should produce values
  expect_true(any(nchar(trimws(result$rdiff1)) > 0))
})

test_that("risk difference with multiple comparisons", {
  data <- data.frame(
    TRT = rep(c("High", "Low", "Placebo"), each = 30),
    VAL = c(
      rep("Yes", 20), rep("No", 10),
      rep("Yes", 15), rep("No", 15),
      rep("Yes", 10), rep("No", 20)
    )
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          risk_diff = list(
            comparisons = list(
              c("High", "Placebo"),
              c("Low", "Placebo")
            ),
            format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_true("rdiff1" %in% names(result))
  expect_true("rdiff2" %in% names(result))
  expect_equal(attr(result$rdiff1, "label"), "High vs Placebo")
  expect_equal(attr(result$rdiff2, "label"), "Low vs Placebo")
})

test_that("risk difference special rows have empty rdiff", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    VAL = c(rep("X", 7), rep("Y", 3), rep("X", 5), rep("Y", 5))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          total_row = TRUE,
          risk_diff = list(
            comparisons = list(c("A", "B")),
            format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  total_row <- result[result$rowlabel1 == "Total", ]
  expect_equal(nrow(total_row), 1)
  # Total row should have empty rdiff
  expect_equal(trimws(total_row$rdiff1), "")
})

test_that("risk difference with zero-cell data handles gracefully", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    VAL = c(rep("X", 10), rep("Y", 10))  # A: all X, B: all Y
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          risk_diff = list(
            comparisons = list(c("A", "B")),
            format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
          )
        )
      )
    )
  )
  # Should not error
  result <- tplyr_build(spec, data)
  expect_true("rdiff1" %in% names(result))
})

test_that("risk difference without risk_diff setting produces no rdiff columns", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    VAL = c(rep("X", 3), rep("Y", 2), rep("X", 2), rep("Y", 3))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  expect_false(any(grepl("^rdiff", names(result))))
})

test_that("risk difference rdiff column preserves across multi-layer harmonization", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    VAL = c(rep("X", 7), rep("Y", 3), rep("X", 5), rep("Y", 5)),
    AGE = c(30:39, 40:49)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          risk_diff = list(
            comparisons = list(c("A", "B")),
            format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
          )
        )
      ),
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_true("rdiff1" %in% names(result))
  # Desc layer rows should have empty rdiff1
  desc_rows <- result[result$ord_layer_index == 2, ]
  expect_true(all(desc_rows$rdiff1 == ""))
})

test_that("risk difference with by variable", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 20),
    GRP = rep(c("G1", "G2"), times = 20),
    VAL = c(
      rep("X", 8), rep("Y", 2), rep("X", 6), rep("Y", 4),
      rep("X", 5), rep("Y", 5), rep("X", 3), rep("Y", 7)
    )
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL", by = "GRP",
        settings = layer_settings(
          risk_diff = list(
            comparisons = list(c("A", "B")),
            format = f_str("xx.x", "rdiff")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_true("rdiff1" %in% names(result))
})

# --- compute_risk_diff unit tests ---

test_that("compute_risk_diff returns correct structure", {
  counts <- data.table::data.table(
    TRT = c("A", "A", "B", "B"),
    VAL = c("X", "Y", "X", "Y"),
    n = c(30, 20, 20, 30),
    total = c(50, 50, 50, 50),
    pct = c(60, 40, 40, 60)
  )
  rd_config <- list(
    comparisons = list(c("A", "B")),
    ci = 0.95
  )
  result <- tplyr2:::compute_risk_diff(counts, "TRT", "VAL", character(0), rd_config)
  expect_true(is.data.frame(result) || data.table::is.data.table(result))
  expect_true("rdiff" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("p_value" %in% names(result))
})

test_that("compute_risk_diff calculates correct difference", {
  counts <- data.table::data.table(
    TRT = c("A", "A", "B", "B"),
    VAL = c("X", "Y", "X", "Y"),
    n = c(60, 40, 40, 60),
    total = c(100, 100, 100, 100),
    pct = c(60, 40, 40, 60)
  )
  rd_config <- list(
    comparisons = list(c("A", "B")),
    ci = 0.95
  )
  result <- tplyr2:::compute_risk_diff(counts, "TRT", "VAL", character(0), rd_config)
  # For "X": 60% - 40% = 20%
  x_row <- result[result$VAL == "X", ]
  expect_equal(x_row$rdiff, 20.0, tolerance = 0.01)
  # CI should include 20
  expect_true(x_row$lower < 20)
  expect_true(x_row$upper > 20)
})
