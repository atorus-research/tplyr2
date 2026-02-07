test_that("tplyr_spec creates spec objects", {
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(group_count(target_var = "SEX"))
  )
  expect_s3_class(spec, "tplyr_spec")
  expect_equal(spec$cols, "TRT01P")
  expect_length(spec$layers, 1)
})

test_that("tplyr_spec captures where expression", {
  spec <- tplyr_spec(
    cols = "TRT01P",
    where = SAFFL == "Y",
    layers = tplyr_layers(group_count(target_var = "SEX"))
  )
  expect_true(!is.null(spec$where))
})

test_that("is_tplyr_spec works", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("SEX")))
  expect_true(is_tplyr_spec(spec))
  expect_false(is_tplyr_spec(list()))
})

test_that("print.tplyr_spec displays correctly", {
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX"),
      group_desc(target_var = "AGE")
    )
  )
  expect_output(print(spec), "tplyr2 table specification")
  expect_output(print(spec), "Column variables: TRT01P")
  expect_output(print(spec), "Layers: 2")
})
