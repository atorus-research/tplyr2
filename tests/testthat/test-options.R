# --- tplyr2_options tests ---

test_that("tplyr2_options returns defaults when no options set", {
  # Clear any options that might be set
  old <- options(
    tplyr2.IBMRounding = NULL,
    tplyr2.quantile_type = NULL,
    tplyr2.scipen = NULL
  )
  on.exit(do.call(options, old), add = TRUE)

  result <- tplyr2_options()
  expect_equal(result$tplyr2.IBMRounding, FALSE)
  expect_equal(result$tplyr2.quantile_type, 7L)
  expect_equal(result$tplyr2.scipen, 9999L)
  expect_null(result$tplyr2.precision_cap)
  expect_null(result$tplyr2.custom_summaries)
})

test_that("tplyr2_options sets and returns previous values", {
  old <- options(tplyr2.IBMRounding = NULL)
  on.exit(do.call(options, old), add = TRUE)

  prev <- tplyr2_options(IBMRounding = TRUE)
  expect_equal(getOption("tplyr2.IBMRounding"), TRUE)

  # Restore
  tplyr2_options(IBMRounding = FALSE)
  expect_equal(getOption("tplyr2.IBMRounding"), FALSE)
})

test_that("tplyr2_options reflects custom values", {
  old <- options(tplyr2.quantile_type = NULL)
  on.exit(do.call(options, old), add = TRUE)

  tplyr2_options(quantile_type = 2L)
  result <- tplyr2_options()
  expect_equal(result$tplyr2.quantile_type, 2L)

  # Clean up
  options(tplyr2.quantile_type = NULL)
})

test_that("scipen is overridden during tplyr_build", {
  old_scipen <- getOption("scipen")
  on.exit(options(scipen = old_scipen), add = TRUE)

  # Set scipen to something low
  options(scipen = -5)

  data <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)

  # Build should succeed and scipen should be restored after
  expect_true(is.data.frame(result))
  expect_equal(getOption("scipen"), -5)
})

# --- get_data_labels tests ---

test_that("get_data_labels extracts labels", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  attr(df$x, "label") <- "My X Variable"
  attr(df$y, "label") <- "My Y Variable"

  result <- get_data_labels(df)
  expect_equal(result[["x"]], "My X Variable")
  expect_equal(result[["y"]], "My Y Variable")
})

test_that("get_data_labels returns NA for unlabeled columns", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  attr(df$x, "label") <- "Labeled"

  result <- get_data_labels(df)
  expect_equal(result[["x"]], "Labeled")
  expect_true(is.na(result[["y"]]))
})

test_that("get_data_labels errors on non-data.frame", {
  expect_error(get_data_labels(1:5), "data must be a data.frame")
})
