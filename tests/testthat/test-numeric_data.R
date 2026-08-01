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
  # Pins the documented contract: grouping columns plus the one statistic.
  # The old assertion ("mean" %in% names) also passed on the whole frame (#79).
  expect_equal(names(sd), c("TRT", "mean"))
  expect_equal(sd$mean, c(35, 55))
  # Every statistic remains reachable through tplyr_numeric_data()
  expect_true(length(names(tplyr_numeric_data(result, 1))) > 2)
})

test_that("tplyr_stats_data keeps grouping columns for every layer type (#79)", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("AGEGR1", by = "SEX"),
    group_desc("AGE", by = "SEX")))
  built <- tplyr_build(spec, tplyr_adsl)

  cnt <- tplyr_stats_data(built, 1, "pct")
  expect_equal(names(cnt), c("TRT01P", "SEX", "AGEGR1", "pct"))

  dsc <- tplyr_stats_data(built, 2, "median")
  expect_equal(names(dsc), c("TRT01P", "SEX", "median"))
})

test_that("tplyr_stats_data does not duplicate a numeric grouping column (#79)", {
  # "n" is both a statistic name and, for the count layer, adjacent to the
  # grouping columns — requesting it must not emit it twice.
  spec <- tplyr_spec(cols = "TRT01P",
                     layers = tplyr_layers(group_count("AGEGR1")))
  built <- tplyr_build(spec, tplyr_adsl)
  out <- tplyr_stats_data(built, 1, "n")
  expect_equal(names(out), c("TRT01P", "AGEGR1", "n"))
  expect_false(anyDuplicated(names(out)) > 0)
})

test_that("a numeric grouping variable is not mistaken for a statistic (#79)", {
  # Type-based inference would drop VISITNUM from the grouping columns; the
  # builders tag them explicitly instead.
  d <- data.frame(TRT = rep(c("A", "B"), each = 4),
                  VISITNUM = rep(c(1, 2), 4),
                  AGE = c(30, 40, 50, 60, 35, 45, 55, 65))
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("AGE", by = "VISITNUM")))
  out <- tplyr_stats_data(tplyr_build(spec, d), 1, "mean")
  expect_equal(names(out), c("TRT", "VISITNUM", "mean"))
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

# Coverage: numeric-data getters and NULL branches
test_that("tplyr_numeric_data returns NULL when absent and filters by layer", {
  d <- data.frame(TRT = rep(c("A","B"), each = 5), V = rep(c("X","Y"), 5))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("V"))), d)
  nd <- tplyr_numeric_data(b)
  expect_true(is.null(nd) || is.data.frame(nd) || is.list(nd))
  # No numeric_data attribute -> NULL
  expect_null(tplyr_numeric_data(data.frame(x = 1)))
})

test_that("tplyr_stats_data returns NULL without numeric data", {
  expect_null(tplyr_stats_data(data.frame(x = 1), 1, "n"))
})
