# --- Analyze layer tests ---

test_that("group_analyze creates correct class", {
  fn <- function(.data, .target_var) {
    data.frame(row_label = "n", formatted = as.character(nrow(.data)))
  }
  layer <- group_analyze("VAL", analyze_fn = fn)
  expect_s3_class(layer, "tplyr_analyze_layer")
  expect_s3_class(layer, "tplyr_layer")
  expect_equal(layer$layer_type, "analyze")
  expect_true(is.function(layer$analyze_fn))
})

test_that("group_analyze errors on non-function", {
  expect_error(group_analyze("VAL", analyze_fn = "not_a_function"),
               "must be a function")
})

test_that("group_analyze captures where expression", {
  fn <- function(.data, .target_var) {
    data.frame(row_label = "n", formatted = as.character(nrow(.data)))
  }
  layer <- group_analyze("VAL", where = FLAG == "Y", analyze_fn = fn)
  expect_false(is.null(layer$where))
})

test_that("analyze layer with format_strings", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    AGE = c(25:34, 35:44)
  )
  fn <- function(.data, .target_var) {
    vals <- .data[[.target_var]]
    data.frame(mean_val = mean(vals), n_val = length(vals))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("AGE",
        analyze_fn = fn,
        settings = layer_settings(
          format_strings = list(
            "Mean" = f_str("xx.x", "mean_val"),
            "N" = f_str("xx", "n_val")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_s3_class(result, "data.frame")
  expect_true("rowlabel1" %in% names(result))
  expect_true(all(c("Mean", "N") %in% result$rowlabel1))
  expect_true("res1" %in% names(result))
  expect_true("res2" %in% names(result))
})

test_that("analyze layer format_strings produce correct values", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    VAL = c(rep(10, 5), rep(20, 5))
  )
  fn <- function(.data, .target_var) {
    vals <- .data[[.target_var]]
    data.frame(mean_val = mean(vals))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL",
        analyze_fn = fn,
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean_val"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  mean_row <- result[result$rowlabel1 == "Mean", ]
  # A group mean = 10.0, B group mean = 20.0
  expect_true(grepl("10\\.0", mean_row$res1))
  expect_true(grepl("20\\.0", mean_row$res2))
})

test_that("analyze layer with formatted column (no format_strings)", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    VAL = 1:10
  )
  fn <- function(.data, .target_var) {
    vals <- .data[[.target_var]]
    data.frame(
      row_label = c("Count", "Sum"),
      formatted = c(as.character(length(vals)), as.character(sum(vals)))
    )
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL", analyze_fn = fn)
    )
  )
  result <- tplyr_build(spec, data)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Count", "Sum") %in% result$rowlabel1))
})

test_that("analyze layer with by data variable", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    GRP = rep(c("G1", "G2"), times = 10),
    VAL = 1:20
  )
  fn <- function(.data, .target_var) {
    data.frame(
      row_label = "n",
      formatted = as.character(nrow(.data))
    )
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL", by = "GRP", analyze_fn = fn)
    )
  )
  result <- tplyr_build(spec, data)
  expect_true("rowlabel1" %in% names(result))
  expect_true("rowlabel2" %in% names(result))
  expect_true(all(c("G1", "G2") %in% result$rowlabel1))
})

test_that("analyze layer with by label", {
  data <- data.frame(
    TRT = c("A", "B"),
    VAL = c(10, 20)
  )
  fn <- function(.data, .target_var) {
    data.frame(row_label = "n", formatted = as.character(nrow(.data)))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL", by = "Custom Stats", analyze_fn = fn)
    )
  )
  result <- tplyr_build(spec, data)
  expect_true("Custom Stats" %in% result$rowlabel1)
})

test_that("analyze layer with where filter", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    FLAG = rep(c("Y", "N"), 10),
    VAL = 1:20
  )
  fn <- function(.data, .target_var) {
    data.frame(row_label = "n", formatted = as.character(nrow(.data)))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL", where = FLAG == "Y", analyze_fn = fn)
    )
  )
  result <- tplyr_build(spec, data)
  expect_s3_class(result, "data.frame")
  # Each TRT group should have 5 rows with FLAG == "Y"
  # So formatted values should be "5"
  expect_true(any(grepl("5", result$res1)))
})

test_that("analyze layer numeric_data attribute", {
  data <- data.frame(
    TRT = c("A", "B"),
    VAL = c(10, 20)
  )
  fn <- function(.data, .target_var) {
    vals <- .data[[.target_var]]
    data.frame(mean_val = mean(vals), n_val = length(vals))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL",
        analyze_fn = fn,
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean_val"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  nd <- tplyr_numeric_data(result, layer = 1)
  expect_true(is.data.frame(nd))
  expect_true("mean_val" %in% names(nd))
  expect_true("n_val" %in% names(nd))
})

test_that("analyze layer in multi-layer spec", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10),
    AGE = c(25:34, 35:44)
  )
  fn <- function(.data, .target_var) {
    vals <- .data[[.target_var]]
    data.frame(mean_val = mean(vals))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("SEX"),
      group_analyze("AGE",
        analyze_fn = fn,
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean_val"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_true(1 %in% result$ord_layer_index)
  expect_true(2 %in% result$ord_layer_index)
})

test_that("analyze_fn must return data.frame", {
  data <- data.frame(TRT = c("A", "B"), VAL = c(1, 2))
  fn <- function(.data, .target_var) {
    list(value = 1)
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL", analyze_fn = fn)
    )
  )
  expect_error(tplyr_build(spec, data), "must return a data.frame")
})

test_that("analyze_fn must return formatted column when no format_strings", {
  data <- data.frame(TRT = c("A", "B"), VAL = c(1, 2))
  fn <- function(.data, .target_var) {
    data.frame(row_label = "n", value = nrow(.data))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL", analyze_fn = fn)
    )
  )
  expect_error(tplyr_build(spec, data), "formatted")
})

test_that("analyze_fn must return row_label column when no format_strings", {
  data <- data.frame(TRT = c("A", "B"), VAL = c(1, 2))
  fn <- function(.data, .target_var) {
    data.frame(formatted = "1")
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("VAL", analyze_fn = fn)
    )
  )
  expect_error(tplyr_build(spec, data), "row_label")
})

test_that("analyze layer without cols", {
  data <- data.frame(VAL = 1:10)
  fn <- function(.data, .target_var) {
    vals <- .data[[.target_var]]
    data.frame(row_label = "Mean", formatted = sprintf("%.1f", mean(vals)))
  }
  spec <- tplyr_spec(
    cols = character(0),
    layers = tplyr_layers(
      group_analyze("VAL", analyze_fn = fn)
    )
  )
  result <- tplyr_build(spec, data)
  expect_equal(nrow(result), 1)
  expect_equal(result$rowlabel1, "Mean")
})

test_that("analyze layer serialization roundtrip", {
  fn <- function(.data, .target_var) {
    vals <- .data[[.target_var]]
    data.frame(mean_val = mean(vals))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_analyze("AGE",
        analyze_fn = fn,
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean_val"))
        )
      )
    )
  )

  scratch_dir <- file.path(tempdir(), "tplyr2_analyze_tests")
  dir.create(scratch_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(scratch_dir, "analyze_roundtrip.json")
  on.exit(unlink(scratch_dir, recursive = TRUE), add = TRUE)

  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_s3_class(spec2$layers[[1]], "tplyr_analyze_layer")
  expect_true(is.function(spec2$layers[[1]]$analyze_fn))
  expect_equal(spec2$layers[[1]]$layer_type, "analyze")
})

test_that("analyze layer validation catches missing analyze_fn", {
  # Manually construct a broken analyze layer (bypassing constructor)
  bad_layer <- structure(
    list(target_var = "VAL", by = NULL, where = NULL,
         analyze_fn = NULL, settings = layer_settings(),
         layer_type = "analyze"),
    class = c("tplyr_analyze_layer", "tplyr_layer")
  )
  spec <- structure(
    list(cols = "TRT", where = NULL, layers = list(bad_layer),
         settings = NULL),
    class = "tplyr_spec"
  )
  expect_error(
    tplyr2:::validate_spec(spec),
    "analyze layer must have a valid analyze_fn"
  )
})
