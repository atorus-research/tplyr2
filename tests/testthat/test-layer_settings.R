test_that("layer_settings creates settings objects", {
  s <- layer_settings()
  expect_s3_class(s, "tplyr_layer_settings")
  expect_null(s$format_strings)
  expect_false(s$total_row)
  expect_equal(s$indentation, "  ")
})

test_that("layer_settings accepts custom format strings", {
  s <- layer_settings(
    format_strings = list(
      "n_counts" = f_str("xxx (xx.x%)", "n", "pct")
    )
  )
  expect_length(s$format_strings, 1)
  expect_s3_class(s$format_strings[[1]], "tplyr_f_str")
})

test_that("layer_settings accepts all parameters", {
  s <- layer_settings(
    total_row = TRUE,
    total_row_label = "Overall",
    name = "my_layer",
    distinct_by = "USUBJID"
  )
  expect_true(s$total_row)
  expect_equal(s$total_row_label, "Overall")
  expect_equal(s$name, "my_layer")
  expect_equal(s$distinct_by, "USUBJID")
})
