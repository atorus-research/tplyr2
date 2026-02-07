test_that("tplyr_build returns a data.frame", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)
  expect_s3_class(result, "data.frame")
})

test_that("tplyr_build applies global where filter", {
  test_data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    SEX = c("M", "F", "M", "F"),
    FLAG = c("Y", "N", "Y", "Y")
  )

  spec <- tplyr_spec(
    cols = "TRT",
    where = FLAG == "Y",
    layers = tplyr_layers(
      group_count(target_var = "SEX")
    )
  )
  result <- tplyr_build(spec, test_data)

  # Only 3 rows pass the filter (FLAG == "Y")
  # So total for A = 1 (only M), total for B = 2 (M and F)
  m_row <- result[result$rowlabel1 == "M", ]
  expect_true(grepl("1", m_row$res1))
})

test_that("tplyr_build sorts layers correctly", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX"),
      group_desc(
        target_var = "AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Count layer (index 1) should come before desc layer (index 2)
  layer_indices <- result$ord_layer_index
  is_sorted <- all(diff(layer_indices) >= 0)
  expect_true(is_sorted)

  # First rows should have ordindx = 1
  expect_equal(result$ord_layer_index[1], 1)
  # Last row should have ordindx = 2
  expect_equal(result$ord_layer_index[nrow(result)], 2)
})

test_that("tplyr_build handles multiple layers", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX", by = "Sex n (%)"),
      group_count(target_var = "RACE", by = "Race n (%)"),
      group_desc(
        target_var = "AGE",
        by = "Age (Years)",
        settings = layer_settings(
          format_strings = list("n" = f_str("xxx", "n"), "Mean" = f_str("xx.x", "mean"))
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Should have all three layers
  expect_true(1 %in% result$ord_layer_index)
  expect_true(2 %in% result$ord_layer_index)
  expect_true(3 %in% result$ord_layer_index)
})

test_that("apply_overrides works", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("SEX")))
  updated <- tplyr2:::apply_overrides(spec, list(cols = "TRT01A"))
  expect_equal(updated$cols, "TRT01A")
})

test_that("apply_overrides is no-op with no overrides", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("SEX")))
  updated <- tplyr2:::apply_overrides(spec, list())
  expect_equal(updated$cols, "TRT01P")
})

test_that("harmonize_and_bind handles empty list", {
  result <- tplyr2:::harmonize_and_bind(list())
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("tplyr_build errors on unknown layer type", {
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = list(structure(list(), class = "tplyr_unknown"))
  )
  expect_error(tplyr_build(spec, data.frame(TRT01P = "A")), "not a tplyr_layer")
})
