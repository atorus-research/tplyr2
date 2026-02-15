test_that("label() creates tplyr_label objects", {
  lbl <- label("Sex n (%)")
  expect_s3_class(lbl, "tplyr_label")
  expect_true(is_label(lbl))
  expect_equal(as.character(lbl), "Sex n (%)")
})

test_that("is_label returns FALSE for plain strings", {
  expect_false(is_label("plain string"))
})

test_that("group_count creates count layers", {
  layer <- group_count(target_var = "SEX")
  expect_s3_class(layer, "tplyr_count_layer")
  expect_s3_class(layer, "tplyr_layer")
  expect_equal(layer$target_var, "SEX")
  expect_equal(layer$layer_type, "count")
})

test_that("group_count captures by parameter", {
  layer <- group_count(target_var = "SEX", by = c("Demographics", "AGEGR1"))
  expect_equal(layer$by, c("Demographics", "AGEGR1"))
})

test_that("group_count captures where expression", {
  layer <- group_count(target_var = "SEX", where = SAFFL == "Y")
  expect_true(!is.null(layer$where))
})

test_that("group_count uses default layer_settings", {
  layer <- group_count(target_var = "SEX")
  expect_s3_class(layer$settings, "tplyr_layer_settings")
})

test_that("group_desc creates desc layers", {
  layer <- group_desc(target_var = "AGE")
  expect_s3_class(layer, "tplyr_desc_layer")
  expect_s3_class(layer, "tplyr_layer")
  expect_equal(layer$target_var, "AGE")
  expect_equal(layer$layer_type, "desc")
})

test_that("group_shift creates shift layers", {
  layer <- group_shift(target_var = c(row = "BNRIND", column = "ANRIND"))
  expect_s3_class(layer, "tplyr_shift_layer")
  expect_s3_class(layer, "tplyr_layer")
  expect_equal(layer$layer_type, "shift")
})

test_that("tplyr_layers validates inputs", {
  expect_error(
    tplyr_layers("not a layer"),
    "must be tplyr_layer objects"
  )
})

test_that("tplyr_layers wraps multiple layers", {
  layers <- tplyr_layers(
    group_count(target_var = "SEX"),
    group_desc(target_var = "AGE")
  )
  expect_length(layers, 2)
  expect_s3_class(layers[[1]], "tplyr_count_layer")
  expect_s3_class(layers[[2]], "tplyr_desc_layer")
})

test_that("is_tplyr_layer works", {
  expect_true(is_tplyr_layer(group_count("SEX")))
  expect_false(is_tplyr_layer("not a layer"))
})
