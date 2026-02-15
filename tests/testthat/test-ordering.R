# --- compute_var_order tests ---

test_that("compute_var_order with factor uses factor levels", {
  vals <- factor(c("B", "A", "C"), levels = c("C", "A", "B"))
  result <- tplyr2:::compute_var_order(vals)
  expect_equal(result, c(3, 2, 1))
})

test_that("compute_var_order with character uses alphabetical", {
  vals <- c("Banana", "Apple", "Cherry")
  result <- tplyr2:::compute_var_order(vals)
  expect_equal(result, c(2, 1, 3))
})

test_that("compute_var_order with VARN companion", {
  vals <- c("RACE1", "RACE2", "RACE3")
  dt <- data.table::data.table(
    RACE = c("RACE1", "RACE2", "RACE3"),
    RACEN = c(3, 1, 2)
  )
  result <- tplyr2:::compute_var_order(vals, var_name = "RACE", data_dt = dt)
  expect_equal(result, c(3, 1, 2))
})

test_that("compute_var_order byfactor method", {
  vals <- factor(c("B", "A"), levels = c("A", "B"))
  result <- tplyr2:::compute_var_order(vals, method = "byfactor")
  expect_equal(result, c(2, 1))
})

test_that("compute_var_order bycount method", {
  vals <- c("A", "B", "C")
  counts <- c(10, 30, 20)
  result <- tplyr2:::compute_var_order(vals, method = "bycount", count_values = counts)
  # Higher count = lower sort key (descending)
  expect_equal(result, c(-10, -30, -20))
})

test_that("compute_var_order alphabetical method", {
  vals <- factor(c("B", "A"), levels = c("B", "A"))  # Factor with non-alpha order
  result <- tplyr2:::compute_var_order(vals, method = "alphabetical")
  # Should use alphabetical, not factor order
  expect_equal(result, c(2, 1))
})

# --- Column rename tests ---

test_that("rename_ord_columns renames correctly", {
  dt <- data.table::data.table(
    ordindx = c(1, 2),
    ord1 = c(1, 1),
    ord2 = c(NA, NA),
    res1 = c("a", "b")
  )
  tplyr2:::rename_ord_columns(dt)
  expect_true("ord_layer_index" %in% names(dt))
  expect_true("ord_layer_1" %in% names(dt))
  expect_true("ord_layer_2" %in% names(dt))
  expect_false("ordindx" %in% names(dt))
  expect_false("ord1" %in% names(dt))
})

# --- Output column name convention tests ---

test_that("build output has ord_layer_index and ord_layer_1", {
  data <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)
  expect_true("ord_layer_index" %in% names(result))
  expect_true("ord_layer_1" %in% names(result))
})

test_that("nested output has ord_layer_2", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    AEBODSYS = c("SOC1", "SOC1", "SOC2", "SOC2"),
    AEDECOD = c("PT1", "PT2", "PT3", "PT4")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count(c("AEBODSYS", "AEDECOD")))
  )
  result <- tplyr_build(spec, data)
  expect_true("ord_layer_2" %in% names(result))
})

# --- Order count method integration tests ---

test_that("order_count_method = byfactor respects factor levels", {
  data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    VAL = factor(c("Z", "Y", "X", "Z", "Y", "X"), levels = c("X", "Y", "Z"))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          order_count_method = "byfactor"
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  # Factor levels are X, Y, Z so ord_layer_1 should be 1, 2, 3
  expect_equal(result$ord_layer_1, c(1, 2, 3))
  # Row order should follow factor: X, Y, Z
  expect_equal(result$rowlabel1, c("X", "Y", "Z"))
})

test_that("default ordering preserves existing behavior", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    VAL = c("X", "Y", "X", "Y")
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)
  # Default should be alphabetical
  expect_equal(result$rowlabel1, c("X", "Y"))
  expect_true(all(!is.na(result$ord_layer_1)))
})

test_that("desc layer ordering preserves stat order", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    AGE = c(30, 40, 50, 60)
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
  # ord_layer_1 should be 1, 2 (stat order)
  expect_equal(result$ord_layer_1, c(1, 2))
})

test_that("multi-layer output has correct ord_layer_index", {
  data <- data.frame(
    TRT = c("A", "B"),
    VAL = c("X", "Y"),
    AGE = c(30, 40)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL"),
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_true(1 %in% result$ord_layer_index)
  expect_true(2 %in% result$ord_layer_index)
})
