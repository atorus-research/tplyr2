# --- tplyr_numeric_data tests ---

test_that("tplyr_numeric_data returns numeric data for count layer", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    VAL = c("X", "Y", "X", "Y")
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)

  nd <- tplyr_numeric_data(result, layer = 1)
  expect_true(is.data.frame(nd))
  expect_true("n" %in% names(nd))
  expect_true("pct" %in% names(nd))
  expect_true("VAL" %in% names(nd))
})

test_that("tplyr_numeric_data returns numeric data for desc layer", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    AGE = c(30, 40, 50, 60)
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_desc("AGE")))
  result <- tplyr_build(spec, data)

  nd <- tplyr_numeric_data(result, layer = 1)
  expect_true(is.data.frame(nd))
  expect_true("mean" %in% names(nd))
  expect_true("sd" %in% names(nd))
  expect_true("n" %in% names(nd))
})

test_that("tplyr_numeric_data returns all layers when layer = NULL", {
  data <- data.frame(
    TRT = c("A", "B"),
    VAL = c("X", "Y"),
    AGE = c(30, 40)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL"),
      group_desc("AGE", settings = layer_settings(
        format_strings = list("n" = f_str("xx", "n"))
      ))
    )
  )
  result <- tplyr_build(spec, data)

  nd <- tplyr_numeric_data(result)
  expect_true(is.list(nd))
  expect_equal(length(nd), 2)
  expect_true("1" %in% names(nd))
  expect_true("2" %in% names(nd))
})

test_that("tplyr_numeric_data returns NULL for invalid layer", {
  data <- data.frame(TRT = "A", VAL = "X")
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)

  expect_null(tplyr_numeric_data(result, layer = 99))
})

test_that("tplyr_stats_data filters by statistic", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    AGE = c(30, 40, 50, 60)
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_desc("AGE")))
  result <- tplyr_build(spec, data)

  sd <- tplyr_stats_data(result, layer = 1, statistic = "mean")
  expect_true(is.data.frame(sd))
  expect_true("mean" %in% names(sd))
})

test_that("tplyr_stats_data returns NULL for missing statistic", {
  data <- data.frame(TRT = "A", VAL = "X")
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)

  expect_null(tplyr_stats_data(result, layer = 1, statistic = "nonexistent"))
})

test_that("numeric data preserved for shift layer", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    BNRIND = factor(c("N", "H", "N", "L"), levels = c("L", "N", "H")),
    ANRIND = factor(c("H", "N", "L", "N"), levels = c("L", "N", "H"))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"))
    )
  )
  result <- tplyr_build(spec, data)
  nd <- tplyr_numeric_data(result, layer = 1)
  expect_true(is.data.frame(nd))
  expect_true("n" %in% names(nd))
})

test_that("numeric data count values match expectations", {
  data <- data.frame(
    TRT = c("A", "A", "A", "B", "B"),
    VAL = c("X", "X", "Y", "X", "Y")
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)

  nd <- tplyr_numeric_data(result, layer = 1)
  # A: X=2, Y=1; B: X=1, Y=1
  a_x <- nd[nd$TRT == "A" & nd$VAL == "X", "n"]
  expect_equal(a_x, 2)
})

test_that("numeric data for nested count layer", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    SOC = c("S1", "S1", "S1", "S2"),
    PT = c("P1", "P2", "P1", "P3")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count(c("SOC", "PT")))
  )
  result <- tplyr_build(spec, data)
  nd <- tplyr_numeric_data(result, layer = 1)
  expect_true(is.data.frame(nd))
  expect_true("n" %in% names(nd))
})
