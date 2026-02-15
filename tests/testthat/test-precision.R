# --- collect_precision_values tests ---

test_that("collect_precision_values handles integers", {
  result <- tplyr2:::collect_precision_values(c(1, 12, 100))
  expect_equal(result$max_int, 3L)
  expect_equal(result$max_dec, 0L)
})

test_that("collect_precision_values handles mixed precision", {
  result <- tplyr2:::collect_precision_values(c(1.5, 12.34, 100.1))
  expect_equal(result$max_int, 3L)
  expect_equal(result$max_dec, 2L)
})

test_that("collect_precision_values handles empty vector", {
  result <- tplyr2:::collect_precision_values(numeric(0))
  expect_equal(result$max_int, 1L)
  expect_equal(result$max_dec, 0L)
})

test_that("collect_precision_values handles single value", {
  result <- tplyr2:::collect_precision_values(3.14159)
  expect_equal(result$max_int, 1L)
  expect_true(result$max_dec >= 4L)  # At least 4 significant decimal places
})

test_that("collect_precision_values handles negative values", {
  result <- tplyr2:::collect_precision_values(c(-5.2, 10.33))
  expect_equal(result$max_int, 2L)  # 10 has 2 digits
  expect_equal(result$max_dec, 2L)
})

# --- resolve_precision tests ---

test_that("resolve_precision with auto=TRUE uses collected precision", {
  group <- list(
    int = list(width = 1L, auto = TRUE, offset = 0L, hug = FALSE),
    dec = list(width = 1L, auto = TRUE, offset = 0L, hug = FALSE),
    has_decimal = TRUE
  )
  result <- tplyr2:::resolve_precision(group, 3L, 2L)
  expect_equal(result$int_width, 3L)
  expect_equal(result$dec_width, 2L)
})

test_that("resolve_precision applies offset", {
  group <- list(
    int = list(width = 1L, auto = TRUE, offset = 0L, hug = FALSE),
    dec = list(width = 1L, auto = TRUE, offset = 2L, hug = FALSE),
    has_decimal = TRUE
  )
  result <- tplyr2:::resolve_precision(group, 3L, 1L)
  expect_equal(result$int_width, 3L)
  expect_equal(result$dec_width, 3L)  # 1 + 2 offset
})

test_that("resolve_precision with auto=FALSE uses fixed width", {
  group <- list(
    int = list(width = 5L, auto = FALSE, offset = 0L, hug = FALSE),
    dec = list(width = 3L, auto = FALSE, offset = 0L, hug = FALSE),
    has_decimal = TRUE
  )
  result <- tplyr2:::resolve_precision(group, 10L, 10L)
  expect_equal(result$int_width, 5L)
  expect_equal(result$dec_width, 3L)
})

test_that("resolve_precision enforces minimum widths", {
  group <- list(
    int = list(width = 1L, auto = TRUE, offset = 0L, hug = FALSE),
    dec = list(width = 1L, auto = TRUE, offset = 0L, hug = FALSE),
    has_decimal = TRUE
  )
  result <- tplyr2:::resolve_precision(group, 0L, 0L)
  expect_equal(result$int_width, 1L)
  expect_equal(result$dec_width, 0L)
})

# --- collect_precision tests ---

test_that("collect_precision works without grouping", {
  dt <- data.table::data.table(VAL = c(1.5, 12.34, 100))
  result <- tplyr2:::collect_precision(dt, "VAL")
  expect_equal(result$max_int, 3L)
  expect_equal(result$max_dec, 2L)
})

test_that("collect_precision works with precision_by", {
  dt <- data.table::data.table(
    PARAM = c("ALT", "ALT", "AST", "AST"),
    VAL = c(1.5, 12.3, 100.123, 5.1)
  )
  result <- tplyr2:::collect_precision(dt, "VAL", precision_by = "PARAM")
  expect_true("PARAM" %in% names(result))
  alt_prec <- result[PARAM == "ALT"]
  ast_prec <- result[PARAM == "AST"]
  expect_equal(alt_prec$max_int, 2L)
  expect_equal(alt_prec$max_dec, 1L)
  expect_equal(ast_prec$max_int, 3L)
  expect_equal(ast_prec$max_dec, 3L)
})

test_that("collect_precision uses external precision_data", {
  dt <- data.table::data.table(VAL = c(1.5, 12.34))
  prec_data <- data.frame(max_int = 5L, max_dec = 3L)
  result <- tplyr2:::collect_precision(dt, "VAL", precision_data = prec_data)
  expect_equal(result$max_int, 5L)
  expect_equal(result$max_dec, 3L)
})

test_that("collect_precision validates precision_data columns", {
  dt <- data.table::data.table(VAL = c(1.5))
  bad_data <- data.frame(int_w = 5L, dec_w = 3L)
  expect_error(
    tplyr2:::collect_precision(dt, "VAL", precision_data = bad_data),
    "max_int.*max_dec"
  )
})

# --- Precision capping tests ---

test_that("precision_cap limits precision", {
  dt <- data.table::data.table(VAL = c(1.12345, 12345.6))
  result <- tplyr2:::collect_precision(dt, "VAL", precision_cap = c(int = 3, dec = 2))
  expect_equal(result$max_int, 3L)
  expect_equal(result$max_dec, 2L)
})

test_that("global precision cap is used as fallback", {
  dt <- data.table::data.table(VAL = c(1.12345, 12345.6))
  withr::with_options(list(tplyr2.precision_cap = c(int = 2, dec = 1)), {
    result <- tplyr2:::collect_precision(dt, "VAL")
    expect_equal(result$max_int, 2L)
    expect_equal(result$max_dec, 1L)
  })
})

test_that("layer-level cap overrides global cap", {
  dt <- data.table::data.table(VAL = c(1.12345, 12345.6))
  withr::with_options(list(tplyr2.precision_cap = c(int = 1, dec = 1)), {
    result <- tplyr2:::collect_precision(dt, "VAL", precision_cap = c(int = 4, dec = 3))
    expect_equal(result$max_int, 4L)
    expect_equal(result$max_dec, 3L)
  })
})

# --- End-to-end auto-precision in desc layer ---

test_that("auto-precision formats desc layer correctly", {
  data <- data.frame(
    TRT = c(rep("A", 5), rep("B", 5)),
    VAL = c(1.5, 12.34, 3.1, 8.99, 5.0,
            10.1, 20.5, 15.33, 12.0, 8.7)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VAL",
        settings = layer_settings(
          format_strings = list(
            "Mean (SD)" = f_str("a.a+1 (a.a+2)", "mean", "sd")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)  # One stat row
  # The formatted values should use auto-precision from data
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_true(length(res_cols) >= 2)
  # Values should contain decimal points (precision > 0)
  for (col in res_cols) {
    expect_true(grepl("\\.", result[[col]]))
  }
})

test_that("auto-precision with precision_cap limits decimal places", {
  data <- data.frame(
    TRT = rep("A", 5),
    VAL = c(1.12345, 2.23456, 3.34567, 4.45678, 5.56789)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VAL",
        settings = layer_settings(
          format_strings = list(
            "Mean" = f_str("a.a", "mean")
          ),
          precision_cap = c(dec = 2)
        )
      )
    )
  )
  result <- tplyr_build(spec, data)

  # With cap of 2 decimal places and a.a format (no offset),
  # should have exactly 2 decimal places
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  val <- trimws(result[[res_cols[1]]])
  dec_part <- sub(".*\\.", "", val)
  expect_equal(nchar(dec_part), 2)
})

test_that("auto-precision with fixed and auto groups mixed", {
  data <- data.frame(
    TRT = rep("A", 3),
    VAL = c(1.5, 12.3, 8.0)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VAL",
        settings = layer_settings(
          format_strings = list(
            # First group fixed (xx), second group auto (a.a+1)
            "n (Mean)" = f_str("xx (a.a+1)", "n", "mean")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)

  expect_s3_class(result, "data.frame")
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  # Should have parentheses in the output
  expect_true(grepl("\\(", result[[res_cols[1]]]))
})
