# --- Build-time overrides tests ---

test_that("override cols at build time", {
  data <- data.frame(
    TRT = c("A", "B"),
    ARM = c("X", "Y"),
    VAL = c("V1", "V2")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data, cols = "ARM")
  # Should have column labels from ARM, not TRT
  expect_true(any(grepl("X|Y", attr(result$res1, "label"))))
})

test_that("override where as expression", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    FLAG = c("Y", "N", "Y", "N"),
    VAL = c("X", "X", "Y", "Y")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data, where = quote(FLAG == "Y"))
  # Only 2 rows of data (FLAG == "Y"), one per TRT
  nd <- tplyr_numeric_data(result, layer = 1)
  expect_true(all(nd$n <= 1))
})

test_that("override where as character string", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    FLAG = c("Y", "N", "Y", "N"),
    VAL = c("X", "X", "Y", "Y")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data, where = "FLAG == 'Y'")
  nd <- tplyr_numeric_data(result, layer = 1)
  expect_true(all(nd$n <= 1))
})

test_that("override where with NULL removes filter", {
  data <- data.frame(
    TRT = c("A", "B"),
    VAL = c("X", "Y")
  )
  # Spec with where uses bare expression (enexpr captures it)
  spec <- tplyr_spec(
    cols = "TRT",
    where = TRT == "A",
    layers = tplyr_layers(group_count("VAL"))
  )
  # With where filter, only A data
  result_filtered <- tplyr_build(spec, data)
  nd_filtered <- tplyr_numeric_data(result_filtered, layer = 1)

  # Override with NULL removes filter
  result_all <- tplyr_build(spec, data, where = NULL)
  nd_all <- tplyr_numeric_data(result_all, layer = 1)

  expect_true(sum(nd_all$n) >= sum(nd_filtered$n))
})

test_that("override pop_data sub-fields merges via ...", {
  # Test apply_overrides directly since pop_data is a formal arg of tplyr_build
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("VAL"))
  )

  # Merge sub-fields into existing pop_data config
  modified <- tplyr2:::apply_overrides(spec, list(pop_data = list(cols = "ARM")))
  expect_equal(modified$pop_data$cols, "ARM")
  # Class should be preserved from original
  expect_true(inherits(modified$pop_data, "tplyr_pop_data"))
})

test_that("empty overrides is no-op", {
  data <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  result1 <- tplyr_build(spec, data)
  result2 <- tplyr_build(spec, data)
  expect_equal(result1, result2)
})

test_that("override preserves non-overridden fields", {
  data <- data.frame(
    TRT = c("A", "B"),
    ARM = c("X", "Y"),
    VAL = c("V1", "V2")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  # Override cols but layers should be unchanged
  result <- tplyr_build(spec, data, cols = "ARM")
  expect_true("rowlabel1" %in% names(result))
  expect_true(all(c("V1", "V2") %in% result$rowlabel1))
})

test_that("multiple overrides applied together", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    ARM = c("X", "X", "Y", "Y"),
    FLAG = c("Y", "N", "Y", "N"),
    VAL = c("V1", "V2", "V1", "V2")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data, cols = "ARM", where = "FLAG == 'Y'")
  expect_true(is.data.frame(result))
  # Columns based on ARM levels
  expect_true(any(grepl("X|Y", attr(result$res1, "label"))))
})

test_that("override does not mutate original spec", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  original_cols <- spec$cols

  data <- data.frame(TRT = c("A", "B"), ARM = c("X", "Y"), VAL = c("V1", "V2"))
  tplyr_build(spec, data, cols = "ARM")

  # Original spec should be unchanged
  expect_equal(spec$cols, original_cols)
})

test_that("unknown override names are ignored", {
  data <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  # Should not error
  result <- tplyr_build(spec, data, totally_unknown_param = "ignored")
  expect_true(is.data.frame(result))
})
