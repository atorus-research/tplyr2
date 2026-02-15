test_that("desc layer produces correct output structure", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_desc(target_var = "AGE")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  expect_s3_class(result, "data.frame")
  expect_true("rowlabel1" %in% names(result))
  expect_true(any(grepl("^res\\d+$", names(result))))
  expect_true("ord_layer_index" %in% names(result))
})

test_that("desc layer uses default format strings", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_desc(target_var = "AGE")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Default format strings produce 6 rows
  expect_equal(nrow(result), 6)

  # Check stat labels
  stat_labels <- result$rowlabel1
  expect_true("n" %in% stat_labels)
  expect_true("Mean (SD)" %in% stat_labels)
  expect_true("Median" %in% stat_labels)
  expect_true("Q1, Q3" %in% stat_labels)
  expect_true("Min, Max" %in% stat_labels)
  expect_true("Missing" %in% stat_labels)
})

test_that("desc layer computes correct statistics", {
  # Create small known dataset
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    VALUE = c(10, 20, 30, 40, 50, 60)
  )

  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc(
        target_var = "VALUE",
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xxx", "n"),
            "Mean" = f_str("xx.x", "mean")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # n should be 3 for both groups
  n_row <- result[result$rowlabel1 == "n", ]
  expect_equal(trimws(n_row$res1), "3")
  expect_equal(trimws(n_row$res2), "3")

  # Mean of A = 20, Mean of B = 50
  mean_row <- result[result$rowlabel1 == "Mean", ]
  expect_equal(trimws(mean_row$res1), "20.0")
  expect_equal(trimws(mean_row$res2), "50.0")
})

test_that("desc layer respects custom format strings", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_desc(
        target_var = "AGE",
        settings = layer_settings(
          format_strings = list(
            "N" = f_str("xxx", "n"),
            "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  expect_equal(nrow(result), 2)
  expect_equal(result$rowlabel1, c("N", "Mean (SD)"))
})

test_that("desc layer handles by labels", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_desc(
        target_var = "AGE",
        by = "Age (Years)",
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xxx", "n"),
            "Mean" = f_str("xx.x", "mean")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  expect_true("rowlabel1" %in% names(result))
  expect_true("rowlabel2" %in% names(result))
  expect_true(all(result$rowlabel1 == "Age (Years)"))
})

test_that("desc layer handles NA values in target variable", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "A"),
    VALUE = c(10, 20, NA, 30)
  )

  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc(
        target_var = "VALUE",
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xxx", "n"),
            "Missing" = f_str("xxx", "missing")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  n_row <- result[result$rowlabel1 == "n", ]
  missing_row <- result[result$rowlabel1 == "Missing", ]
  expect_equal(trimws(n_row$res1), "3")
  expect_equal(trimws(missing_row$res1), "1")
})

test_that("desc result columns carry label attributes", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_desc(target_var = "AGE")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  for (col in res_cols) {
    lbl <- attr(result[[col]], "label")
    expect_true(!is.null(lbl))
  }
})

# --- Custom Summaries ---

test_that("custom summary computes correctly", {
  test_data <- data.frame(
    TRT = c("A", "A", "A"),
    VALUE = c(10, 20, 30)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        settings = layer_settings(
          format_strings = list(
            "Geo Mean" = f_str("xx.xx", "geo_mean")
          ),
          custom_summaries = list(
            geo_mean = quote(exp(mean(log(.var[.var > 0]), na.rm = TRUE)))
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  expect_equal(nrow(result), 1)
  expected <- exp(mean(log(c(10, 20, 30))))
  actual <- as.numeric(trimws(result$res1))
  expect_equal(actual, round(expected, 2), tolerance = 0.01)
})

test_that("custom summary error returns NA", {
  test_data <- data.frame(
    TRT = rep("A", 3),
    VALUE = c(10, 20, 30)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        settings = layer_settings(
          format_strings = list(
            "Bad" = f_str("xx.xx", "bad_stat")
          ),
          custom_summaries = list(
            bad_stat = quote(stop("intentional error"))
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Should produce output (NA formatted as spaces)
  expect_equal(nrow(result), 1)
})

test_that("global custom summaries via option", {
  test_data <- data.frame(
    TRT = rep("A", 3),
    VALUE = c(10, 20, 30)
  )
  withr::with_options(list(tplyr2.custom_summaries = list(
    cv = quote(sd(.var, na.rm = TRUE) / mean(.var, na.rm = TRUE) * 100)
  )), {
    spec <- tplyr_spec(
      cols = "TRT",
      layers = tplyr_layers(
        group_desc("VALUE",
          settings = layer_settings(
            format_strings = list("CV" = f_str("xx.x", "cv"))
          )
        )
      )
    )
    result <- tplyr_build(spec, test_data)

    expect_equal(nrow(result), 1)
    actual <- as.numeric(trimws(result$res1))
    expected <- sd(c(10, 20, 30)) / mean(c(10, 20, 30)) * 100
    expect_equal(actual, round(expected, 1), tolerance = 0.1)
  })
})

test_that("layer custom summary overrides global", {
  test_data <- data.frame(
    TRT = rep("A", 3),
    VALUE = c(10, 20, 30)
  )
  withr::with_options(list(tplyr2.custom_summaries = list(
    my_stat = quote(0)
  )), {
    spec <- tplyr_spec(
      cols = "TRT",
      layers = tplyr_layers(
        group_desc("VALUE",
          settings = layer_settings(
            format_strings = list("My Stat" = f_str("xx.x", "my_stat")),
            custom_summaries = list(
              my_stat = quote(mean(.var, na.rm = TRUE))
            )
          )
        )
      )
    )
    result <- tplyr_build(spec, test_data)

    # Should use layer value (mean=20), not global (0)
    actual <- as.numeric(trimws(result$res1))
    expect_equal(actual, 20.0, tolerance = 0.1)
  })
})

test_that("custom summary overrides built-in stat", {
  test_data <- data.frame(
    TRT = rep("A", 4),
    VALUE = c(10, 20, 30, 40)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean")),
          custom_summaries = list(
            mean = quote(42)  # Override built-in mean
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  actual <- as.numeric(trimws(result$res1))
  expect_equal(actual, 42.0)
})

# --- Desc Denominators ---

test_that("desc layer provides total and pct", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B"),
    VALUE = c(10, 20, 30, 40, 50)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        settings = layer_settings(
          format_strings = list(
            "n (pct)" = f_str("xx (xx.x%)", "n", "pct")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  expect_equal(nrow(result), 1)
  # TRT A: n=3, total=3, pct=100
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  a_col <- res_cols[grepl("^A", labels)]
  expect_true(grepl("100.0%", result[[a_col]]))
})

test_that("desc denoms_by changes denominator grouping", {
  test_data <- data.frame(
    TRT = c(rep("A", 5), rep("B", 3)),
    SEX = c("M", "M", "M", "F", "F", "M", "M", "F"),
    VALUE = c(10, 20, 30, 40, 50, 60, 70, 80)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        by = "SEX",
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xxx", "n"),
            "Total" = f_str("xxx", "total")
          ),
          denoms_by = c("TRT")
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Total for TRT A should be 5 regardless of SEX
  total_rows <- result[result$rowlabel2 == "Total", ]
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  a_col <- res_cols[grepl("^A", labels)]
  expect_equal(trimws(total_rows[[a_col]][1]), "5")
})

test_that("desc pct computation is correct", {
  test_data <- data.frame(
    TRT = c(rep("A", 4)),
    VALUE = c(10, 20, 30, NA)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        settings = layer_settings(
          format_strings = list(
            "pct" = f_str("xx.x", "pct")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # n=3 (non-NA), total=4 (all rows), pct = 3/4*100 = 75%
  actual <- as.numeric(trimws(result$res1))
  expect_equal(actual, 75.0)
})

# --- Multiple Target Variables ---

test_that("multi-target desc produces rows for each variable", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    AGE = c(25, 30, 35, 40, 45, 50),
    WEIGHT = c(70, 80, 90, 60, 75, 85)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc(c("AGE", "WEIGHT"),
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xxx", "n"),
            "Mean" = f_str("xx.x", "mean")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # 2 vars x 2 stats = 4 rows
  expect_equal(nrow(result), 4)
  # Variable names appear as row labels
  expect_true("AGE" %in% result$rowlabel1)
  expect_true("WEIGHT" %in% result$rowlabel1)
})

test_that("multi-target desc ordering is correct", {
  test_data <- data.frame(
    TRT = rep("A", 3),
    AGE = c(25, 30, 35),
    WEIGHT = c(70, 80, 90)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc(c("AGE", "WEIGHT"),
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xxx", "n"),
            "Mean" = f_str("xx.x", "mean")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # AGE stats should come before WEIGHT stats
  age_rows <- which(result$rowlabel1 == "AGE")
  weight_rows <- which(result$rowlabel1 == "WEIGHT")
  expect_true(max(age_rows) < min(weight_rows))
})

test_that("multi-target desc has variable name as rowlabel", {
  test_data <- data.frame(
    TRT = rep("A", 3),
    AGE = c(25, 30, 35),
    WEIGHT = c(70, 80, 90)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc(c("AGE", "WEIGHT"),
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # rowlabel1 = variable name, rowlabel2 = stat label
  expect_true("rowlabel1" %in% names(result))
  expect_true("rowlabel2" %in% names(result))
  expect_equal(result$rowlabel1, c("AGE", "WEIGHT"))
  expect_equal(result$rowlabel2, c("Mean", "Mean"))
})

test_that("multi-target desc computes correct values per variable", {
  test_data <- data.frame(
    TRT = rep("A", 3),
    AGE = c(10, 20, 30),
    WEIGHT = c(100, 200, 300)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc(c("AGE", "WEIGHT"),
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xxx.x", "mean"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  age_row <- result[result$rowlabel1 == "AGE", ]
  weight_row <- result[result$rowlabel1 == "WEIGHT", ]
  expect_equal(as.numeric(trimws(age_row$res1)), 20.0)
  expect_equal(as.numeric(trimws(weight_row$res1)), 200.0)
})

test_that("multi-target desc works with by labels", {
  test_data <- data.frame(
    TRT = rep("A", 3),
    AGE = c(25, 30, 35),
    WEIGHT = c(70, 80, 90)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc(c("AGE", "WEIGHT"),
        by = "Demographics",
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean"))
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  expect_true("rowlabel1" %in% names(result))
  expect_true("rowlabel2" %in% names(result))
  expect_true("rowlabel3" %in% names(result))
  # rowlabel1 = by label, rowlabel2 = variable name, rowlabel3 = stat label
  expect_true(all(result$rowlabel1 == "Demographics"))
  expect_equal(result$rowlabel2, c("AGE", "WEIGHT"))
})

# --- Stats as Columns ---

test_that("stats_as_columns transposes treatment groups to rows", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    VALUE = c(10, 20, 30, 40, 50, 60)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xxx", "n"),
            "Mean" = f_str("xx.x", "mean")
          ),
          stats_as_columns = TRUE
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Treatment groups become rows
  expect_true(nrow(result) >= 2)
  # Stat names become columns
  expect_true("n" %in% names(result))
  expect_true("Mean" %in% names(result))
})

test_that("stats_as_columns values match non-transposed", {
  test_data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    VALUE = c(10, 20, 30, 40, 50, 60)
  )
  fmts <- list("n" = f_str("xxx", "n"), "Mean" = f_str("xx.x", "mean"))

  # Non-transposed
  spec_normal <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE", settings = layer_settings(format_strings = fmts))
    )
  )
  normal <- tplyr_build(spec_normal, test_data)

  # Transposed
  spec_trans <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        settings = layer_settings(format_strings = fmts, stats_as_columns = TRUE)
      )
    )
  )
  transposed <- tplyr_build(spec_trans, test_data)

  # Get normal values for comparison
  res_cols <- grep("^res\\d+$", names(normal), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(normal[[col]], "label"), character(1))
  a_col <- res_cols[grepl("^A", labels)]
  b_col <- res_cols[grepl("^B", labels)]

  a_n <- normal[normal$rowlabel1 == "n", a_col]
  b_n <- normal[normal$rowlabel1 == "n", b_col]

  # Transposed should have these same values accessible
  a_row <- transposed[grepl("^A", transposed$rowlabel1), ]
  b_row <- transposed[grepl("^B", transposed$rowlabel1), ]

  expect_equal(trimws(a_row[["n"]]), trimws(a_n))
  expect_equal(trimws(b_row[["n"]]), trimws(b_n))
})

test_that("stats_as_columns with by label", {
  test_data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    VALUE = c(10, 20, 30, 40)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        by = "Test Label",
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean")),
          stats_as_columns = TRUE
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Should have the by label preserved and treatment groups as rows
  expect_true("rowlabel1" %in% names(result))
  expect_true("Mean" %in% names(result))
})

test_that("stats_as_columns FALSE produces normal output", {
  test_data <- data.frame(
    TRT = rep("A", 3),
    VALUE = c(10, 20, 30)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("VALUE",
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean")),
          stats_as_columns = FALSE
        )
      )
    )
  )
  result <- tplyr_build(spec, test_data)

  # Normal structure: res columns, not stat-named columns
  expect_true(any(grepl("^res\\d+$", names(result))))
})
