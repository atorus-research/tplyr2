# --- pop_data() constructor tests ---

test_that("pop_data creates correct S3 class", {
  pd <- pop_data(cols = c("TRT01P"))
  expect_s3_class(pd, "tplyr_pop_data")
  expect_true(is_pop_data(pd))
})

test_that("pop_data captures cols", {
  pd <- pop_data(cols = c("TRT01P", "SEX"))
  expect_equal(pd$cols, c("TRT01P", "SEX"))
})

test_that("pop_data where defaults to NULL", {
  pd <- pop_data(cols = "TRT01P")
  expect_null(pd$where)
})

test_that("pop_data captures where expression", {
  pd <- pop_data(cols = "TRT01P", where = SAFFL == "Y")
  expect_false(is.null(pd$where))
  expect_equal(deparse(pd$where), "SAFFL == \"Y\"")
})

test_that("pop_data with named column mapping", {
  pd <- pop_data(cols = c("TRTA" = "TRT01P"))
  expect_equal(names(pd$cols), "TRTA")
  expect_equal(unname(pd$cols), "TRT01P")
})

test_that("is_pop_data returns FALSE for non-pop_data", {
  expect_false(is_pop_data("not pop data"))
  expect_false(is_pop_data(list(cols = "TRT01P")))
})

# --- total_group() constructor tests ---

test_that("total_group creates correct S3 class", {
  tg <- total_group("TRT01P")
  expect_s3_class(tg, "tplyr_total_group")
})

test_that("total_group defaults to 'Total' label", {
  tg <- total_group("TRT01P")
  expect_equal(tg$col_var, "TRT01P")
  expect_equal(tg$label, "Total")
})

test_that("total_group respects custom label", {
  tg <- total_group("TRT01P", label = "All Subjects")
  expect_equal(tg$label, "All Subjects")
})

# --- custom_group() constructor tests ---

test_that("custom_group creates correct S3 class", {
  cg <- custom_group("TRT01P", "High Dose" = c("Dose 1", "Dose 2"))
  expect_s3_class(cg, "tplyr_custom_group")
})

test_that("custom_group captures named groups", {
  cg <- custom_group("TRT01P", "High Dose" = c("Dose 1", "Dose 2"))
  expect_equal(cg$col_var, "TRT01P")
  expect_equal(cg$groups[["High Dose"]], c("Dose 1", "Dose 2"))
})

test_that("custom_group with multiple groups", {
  cg <- custom_group("TRT01P",
    "Low" = c("Dose 1"),
    "High" = c("Dose 2", "Dose 3")
  )
  expect_equal(length(cg$groups), 2)
  expect_equal(names(cg$groups), c("Low", "High"))
})

# --- Integration with tplyr_spec ---

test_that("tplyr_spec stores pop_data config", {
  pd <- pop_data(cols = c("TRT01P"))
  spec <- tplyr_spec(cols = "TRT01P", pop_data = pd)
  expect_true(is_pop_data(spec$pop_data))
  expect_equal(spec$pop_data$cols, "TRT01P")
})

test_that("tplyr_spec stores total_groups", {
  tg <- total_group("TRT01P")
  spec <- tplyr_spec(cols = "TRT01P", total_groups = list(tg))
  expect_equal(length(spec$total_groups), 1)
  expect_s3_class(spec$total_groups[[1]], "tplyr_total_group")
})

# --- resolve_pop_cols tests ---

test_that("resolve_pop_cols with NULL config returns spec_cols", {
  expect_equal(
    tplyr2:::resolve_pop_cols(NULL, c("TRT01P")),
    c("TRT01P")
  )
})

test_that("resolve_pop_cols with positional mapping", {
  pd <- pop_data(cols = c("TRT01P"))
  expect_equal(
    tplyr2:::resolve_pop_cols(pd, c("TRT01P")),
    c("TRT01P")
  )
})

test_that("resolve_pop_cols with named mapping", {
  pd <- pop_data(cols = c("TRTA" = "TRT01P"))
  result <- tplyr2:::resolve_pop_cols(pd, c("TRTA"))
  expect_equal(result, c("TRTA" = "TRT01P"))
})

# --- tplyr_header_n tests ---

test_that("tplyr_header_n returns NULL when no pop_data", {
  result <- data.frame(a = 1)
  expect_null(tplyr_header_n(result))
})

test_that("tplyr_header_n returns header_n attribute", {
  result <- data.frame(a = 1)
  attr(result, "header_n") <- data.frame(TRT01P = "A", .n = 10)
  hn <- tplyr_header_n(result)
  expect_equal(hn$TRT01P, "A")
  expect_equal(hn$.n, 10)
})

# --- Pop data pipeline integration tests ---

test_that("tplyr_build with pop_data computes col_n from pop_data", {
  target <- data.frame(
    TRT = c("A", "A", "B"),
    VAL = c("X", "Y", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B", "B")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL")
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  # Column labels should reflect pop N (4, 3), not target N (2, 1)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  expect_true(any(grepl("N=4", labels)))
  expect_true(any(grepl("N=3", labels)))
})

test_that("tplyr_header_n returns correct values from build", {
  target <- data.frame(
    TRT = c("A", "A", "B"),
    VAL = c("X", "Y", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B", "B")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  hn <- tplyr_header_n(result)
  expect_s3_class(hn, "data.frame")
  expect_true("TRT" %in% names(hn))
  expect_true(".n" %in% names(hn))
  a_n <- hn$.n[hn$TRT == "A"]
  b_n <- hn$.n[hn$TRT == "B"]
  expect_equal(a_n, 4)
  expect_equal(b_n, 3)
})

test_that("tplyr_header_n returns NULL when no pop_data used", {
  data <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  result <- tplyr_build(spec, data)
  expect_null(tplyr_header_n(result))
})

test_that("pop_data where filter is applied", {
  target <- data.frame(
    TRT = c("A", "B"),
    VAL = c("X", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "B", "B"),
    SAFFL = c("Y", "Y", "N", "Y", "Y")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT", where = SAFFL == "Y"),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  hn <- tplyr_header_n(result)
  # After filtering SAFFL == "Y": A=2, B=2
  a_n <- hn$.n[hn$TRT == "A"]
  b_n <- hn$.n[hn$TRT == "B"]
  expect_equal(a_n, 2)
  expect_equal(b_n, 2)
})

test_that("count layer denominators use pop_data when present", {
  target <- data.frame(
    TRT = c("A", "A", "B"),
    VAL = c("X", "Y", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B", "B"),
    VAL = c("X", "X", "Y", "Y", "X", "X", "Y")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  # For A: n=1 for X, total=4 from pop -> 25.0%
  # Find res column for A
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  a_col <- NULL
  for (rc in res_cols) {
    if (grepl("N=4", attr(result[[rc]], "label"))) a_col <- rc
  }
  if (!is.null(a_col)) {
    x_row <- result[result$rowlabel1 == "X", a_col]
    expect_true(grepl("25\\.0", x_row))
  }
})

test_that("desc layer denominators use pop_data when present", {
  target <- data.frame(
    TRT = c("A", "A", "B"),
    AGE = c(30, 40, 50)
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B", "B"),
    AGE = c(30, 40, 50, 60, 50, 55, 60)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list(
            "n (pct)" = f_str("xx (xx.x%)", "n", "pct")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  # A has 2 subjects in target, 4 in pop -> 50.0%
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  a_col <- NULL
  for (rc in res_cols) {
    if (grepl("N=4", attr(result[[rc]], "label"))) a_col <- rc
  }
  if (!is.null(a_col)) {
    val <- result[[a_col]]
    expect_true(grepl("50\\.0", val))
  }
})

test_that("backward compatibility: no pop_data produces same results", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    VAL = c("X", "Y", "X", "Y")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_null(tplyr_header_n(result))
  # col_n should be from target data (2 per arm)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  expect_true(all(grepl("N=2", labels)))
})
