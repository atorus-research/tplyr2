# --- compute_var_order tests ---

test_that("compute_var_order with factor uses factor levels", {
  vals <- factor(c("B", "A", "C"), levels = c("C", "A", "B"))
  result <- tplyr2:::compute_var_order(vals)
  expect_equal(result, c(3, 2, 1))
})

test_that("compute_var_order with character uses alphabetical", {
  vals <- c("Banana", "Apple", "Cherry")
  result <- tplyr2:::compute_var_order(vals)
  expect_equal(result, c(2, 1, 3))
})

test_that("compute_var_order with VARN companion", {
  vals <- c("RACE1", "RACE2", "RACE3")
  dt <- data.table::data.table(
    RACE = c("RACE1", "RACE2", "RACE3"),
    RACEN = c(3, 1, 2)
  )
  result <- tplyr2:::compute_var_order(vals, var_name = "RACE", data_dt = dt)
  expect_equal(result, c(3, 1, 2))
})

test_that("compute_var_order byfactor method", {
  vals <- factor(c("B", "A"), levels = c("A", "B"))
  result <- tplyr2:::compute_var_order(vals, method = "byfactor")
  expect_equal(result, c(2, 1))
})

test_that("compute_var_order bycount method", {
  vals <- c("A", "B", "C")
  counts <- c(10, 30, 20)
  result <- tplyr2:::compute_var_order(vals, method = "bycount", count_values = counts)
  # Higher count = lower sort key (descending)
  expect_equal(result, c(-10, -30, -20))
})

test_that("compute_var_order alphabetical method", {
  vals <- factor(c("B", "A"), levels = c("B", "A"))  # Factor with non-alpha order
  result <- tplyr2:::compute_var_order(vals, method = "alphabetical")
  # Should use alphabetical, not factor order
  expect_equal(result, c(2, 1))
})

# --- Column rename tests ---

test_that("rename_ord_columns renames correctly", {
  dt <- data.table::data.table(
    ordindx = c(1, 2),
    ord1 = c(1, 1),
    ord2 = c(NA, NA),
    res1 = c("a", "b")
  )
  tplyr2:::rename_ord_columns(dt)
  expect_true("ord_layer_index" %in% names(dt))
  expect_true("ord_layer_1" %in% names(dt))
  expect_true("ord_layer_2" %in% names(dt))
  expect_false("ordindx" %in% names(dt))
  expect_false("ord1" %in% names(dt))
})

# --- Output column name convention tests ---

test_that("build output has ord_layer_index and ord_layer_1", {
  data <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)
  expect_true("ord_layer_index" %in% names(result))
  expect_true("ord_layer_1" %in% names(result))
})

test_that("nested output has ord_layer_2", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    AEBODSYS = c("SOC1", "SOC1", "SOC2", "SOC2"),
    AEDECOD = c("PT1", "PT2", "PT3", "PT4")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count(c("AEBODSYS", "AEDECOD")))
  )
  result <- tplyr_build(spec, data)
  expect_true("ord_layer_2" %in% names(result))
})

# --- Order count method integration tests ---

test_that("order_count_method = byfactor respects factor levels", {
  # Levels are deliberately NON-alphabetical so factor order != alphabetical
  # order (issue #16): the target column is coerced to character upstream, so
  # the byfactor branch must recover levels from the source data.
  data <- data.frame(
    TRT = c("A", "A", "A", "B", "B", "B"),
    VAL = factor(c("mid", "lo", "hi", "mid", "lo", "hi"),
                 levels = c("lo", "mid", "hi"))
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("VAL", settings = layer_settings(order_count_method = "byfactor"))))
  result <- tplyr_build(spec, data)
  result <- result[order(result$ord_layer_1), ]
  # Row order follows factor levels lo, mid, hi (not alphabetical hi/lo/mid)
  expect_equal(result$rowlabel1, c("lo", "mid", "hi"))
})

test_that("byfactor places special rows after factor-ordered categories", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 6),
    VAL = factor(rep(c("mid", "lo", "hi", NA, "lo", "hi"), 2),
                 levels = c("lo", "mid", "hi"))
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("VAL", settings = layer_settings(
      order_count_method = "byfactor",
      total_row = TRUE, missing_count = list(label = "Missing")))))
  result <- tplyr_build(spec, data)
  result <- result[order(result$ord_layer_1), ]
  expect_equal(result$rowlabel1, c("lo", "mid", "hi", "Missing", "Total"))
})

test_that("compute_var_order recovers factor levels from data_dt for character input", {
  dt <- data.table::data.table(
    G = factor(c("hi", "lo", "mid"), levels = c("lo", "mid", "hi"))
  )
  keys <- tplyr2:::compute_var_order(c("hi", "lo", "mid"), var_name = "G",
                                     data_dt = dt, method = "byfactor")
  expect_equal(keys, c(3, 1, 2))
})

test_that("nested count orders outer categories by factor levels", {
  data <- data.frame(
    TRT = rep("A", 6),
    SEV = factor(c("severe", "mild", "moderate", "severe", "mild", "moderate"),
                 levels = c("severe", "mild", "moderate")),
    PT  = c("b", "a", "c", "a", "c", "b")
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count(c("SEV", "PT"))))
  result <- tplyr_build(spec, data)
  result <- result[order(result$ord_layer_1, result$ord_layer_2), ]
  outer <- result$rowlabel1[result$rowlabel2 == ""]
  expect_equal(outer, c("severe", "mild", "moderate"))
})

test_that("default ordering preserves existing behavior", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    VAL = c("X", "Y", "X", "Y")
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)
  # Default should be alphabetical
  expect_equal(result$rowlabel1, c("X", "Y"))
  expect_true(all(!is.na(result$ord_layer_1)))
})

test_that("desc layer ordering preserves stat order", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    AGE = c(30, 40, 50, 60)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list(
            "n" = f_str("xx", "n"),
            "Mean" = f_str("xx.x", "mean")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  # ord_layer_1 should be 1, 2 (stat order)
  expect_equal(result$ord_layer_1, c(1, 2))
})

test_that("multi-layer output has correct ord_layer_index", {
  data <- data.frame(
    TRT = c("A", "B"),
    VAL = c("X", "Y"),
    AGE = c(30, 40)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL"),
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  expect_true(1 %in% result$ord_layer_index)
  expect_true(2 %in% result$ord_layer_index)
})

# Coverage: order_count_method = "bycount"
test_that("order_count_method = bycount orders by descending count", {
  data <- data.frame(
    TRT = rep("A", 60),
    VAL = c(rep("rare", 5), rep("common", 40), rep("mid", 15))
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("VAL", settings = layer_settings(order_count_method = "bycount"))))
  result <- tplyr_build(spec, data)
  result <- result[order(result$ord_layer_1), ]
  # Highest count first: common (40), mid (15), rare (5)
  expect_equal(result$rowlabel1, c("common", "mid", "rare"))
})

test_that("order_count_method = bycount with ordering_cols + result_order_var", {
  data <- data.frame(
    TRT = rep(c("A", "B"), c(30, 30)),
    VAL = c(rep(c("x", "y", "z"), c(5, 10, 15)), rep(c("x", "y", "z"), c(20, 5, 5)))
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("VAL", settings = layer_settings(
      order_count_method = "bycount", ordering_cols = "A", result_order_var = "n"))))
  result <- tplyr_build(spec, data)
  expect_true(all(!is.na(result$ord_layer_1)))
  expect_equal(nrow(result), 3)
})

# Issue #24: count layers order by-group rows by the by variable's factor levels
test_that("count orders by-group rows by factor levels", {
  d <- data.frame(
    TRTP = rep(c("A", "B"), each = 9),
    VISIT = factor(rep(c("Week 2", "Week 12", "Baseline"), 6),
                   levels = c("Baseline", "Week 2", "Week 12")),
    RESP = rep(c("Y", "N", "Y"), 6)
  )
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_count("RESP", by = "VISIT"))), d)
  b <- b[order(b$ord_layer_1), ]
  expect_equal(unique(b$rowlabel1), c("Baseline", "Week 2", "Week 12"))
})

test_that("count by-group order respects a VARN companion", {
  d <- data.frame(
    TRTP = "A",
    AVISIT = rep(c("Week 2", "Week 12", "Baseline"), 4),
    AVISITN = rep(c(2, 12, 0), 4),
    RESP = rep(c("Y", "N"), 6)
  )
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_count("RESP", by = "AVISIT"))), d)
  b <- b[order(b$ord_layer_1), ]
  expect_equal(unique(b$rowlabel1), c("Baseline", "Week 2", "Week 12"))
})

test_that("count by-group order falls back to alphabetical for non-factor", {
  d <- data.frame(TRTP = "A", G = rep(c("zeta", "alpha", "mid"), 4),
                  RESP = rep(c("Y", "N"), 6))
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_count("RESP", by = "G"))), d)
  b <- b[order(b$ord_layer_1), ]
  expect_equal(unique(b$rowlabel1), c("alpha", "mid", "zeta"))
})

# --- order_count_method = "bycount" and companions (#57) ---

.bycount_data <- function() {
  # totals across arms: Apple=24, Zebra=23, Mango=13
  # col A: Zebra=20, Apple=6, Mango=4 ; col B: Apple=18, Mango=9, Zebra=3
  data.frame(
    TRT = factor(rep(c("A", "B"), each = 30), levels = c("A", "B")),
    AE  = factor(c(rep("Zebra", 20), rep("Apple", 6), rep("Mango", 4),
                   rep("Apple", 18), rep("Mango", 9), rep("Zebra", 3)),
                 levels = c("Apple", "Mango", "Zebra")),
    stringsAsFactors = FALSE
  )
}

test_that("order_count_method = 'bycount' sorts by descending total count (#57)", {
  d <- .bycount_data()
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("AE", settings = layer_settings(order_count_method = "bycount")))), d)
  b <- b[order(b$ord_layer_1), ]
  expect_equal(b$rowlabel1, c("Apple", "Zebra", "Mango"))
})

test_that("bycount keeps the total row last (#57)", {
  d <- .bycount_data()
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("AE", settings = layer_settings(
      order_count_method = "bycount", total_row = TRUE)))), d)
  b <- b[order(b$ord_layer_1), ]
  expect_equal(b$rowlabel1[nrow(b)], "Total")
  expect_equal(b$rowlabel1[1:3], c("Apple", "Zebra", "Mango"))
})

test_that("ordering_cols sorts bycount by a specific column's count (#57)", {
  d <- .bycount_data()
  ord <- function(oc) {
    b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_count("AE", settings = layer_settings(
        order_count_method = "bycount", ordering_cols = oc)))), d)
    b[order(b$ord_layer_1), ]$rowlabel1
  }
  expect_equal(ord("A"), c("Zebra", "Apple", "Mango"))   # col A: 20,6,4
  expect_equal(ord("B"), c("Apple", "Mango", "Zebra"))   # col B: 18,9,3
})

test_that("bycount blocks by-groups instead of interleaving them (#57)", {
  d <- .bycount_data()
  d$GRP <- factor(rep(c("G1", "G2"), 30), levels = c("G1", "G2"))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("AE", by = "GRP", settings = layer_settings(
      order_count_method = "byfactor")))), d)
  b <- b[order(b$ord_layer_1), ]
  # every G1 row comes before every G2 row (blocked, not interleaved)
  expect_true(max(which(b$rowlabel1 == "G1")) < min(which(b$rowlabel1 == "G2")))
})

test_that("the default ordering respects target factor levels (#57)", {
  d <- data.frame(
    TRT = factor(rep("A", 30)),
    AE  = factor(rep(c("Zebra", "Apple", "Mango"), 10),
                 levels = c("Zebra", "Apple", "Mango")))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("AE"))), d)   # no order_count_method -> default
  b <- b[order(b$ord_layer_1), ]
  expect_equal(b$rowlabel1, c("Zebra", "Apple", "Mango"))  # factor order, not alphabetical
})

test_that("outer_sort_position = 'desc' reverses nested outer ordering (#57)", {
  d <- data.frame(
    TRT = factor(rep("A", 40)),
    SOC = factor(rep(c("Alpha", "Beta", "Gamma"), c(16, 14, 10)),
                 levels = c("Alpha", "Beta", "Gamma")),
    PT  = factor(rep(c("p1", "p2"), 20)))
  outer_order <- function(osp) {
    b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_count(c("SOC", "PT"), settings = layer_settings(
        outer_sort_position = osp)))), d)
    b <- b[order(b$ord_layer_1), ]
    unique(b$rowlabel1)
  }
  expect_equal(outer_order("asc"),  c("Alpha", "Beta", "Gamma"))
  expect_equal(outer_order("desc"), c("Gamma", "Beta", "Alpha"))
  # invalid value errors
  expect_error(
    tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_count(c("SOC", "PT"), settings = layer_settings(
        outer_sort_position = "sideways")))), d),
    "outer_sort_position must be"
  )
})
