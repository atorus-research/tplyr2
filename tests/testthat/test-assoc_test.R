# --- assoc_test() constructor ---

test_that("assoc_test validates its arguments", {
  expect_error(assoc_test(fn = "notafn"), "must be a function")
  expect_error(assoc_test(fn = function(d) 1, format = "x.xxx"), "f_str")
  expect_error(assoc_test(fn = function(d) 1, format = f_str("xx xx", "a", "b")),
               "exactly one variable")
})

test_that("assoc_test object prints", {
  at <- assoc_test(fn = function(d) 0.5)
  expect_output(print(at), "association test")
  expect_s3_class(at, "tplyr_assoc_test")
})

# --- count layer ---

test_that("count assoc_test emits one p-value per by-group on the first row", {
  set.seed(3)
  d <- data.frame(
    TRT = factor(rep(c("A", "B"), each = 30), levels = c("A", "B")),
    PARAM = factor(rep(c("ALT", "AST"), 30), levels = c("ALT", "AST")),
    RESP = factor(sample(c("N", "H"), 60, replace = TRUE), levels = c("N", "H")))
  at <- assoc_test(fn = function(.d) fisher.test(table(.d$TRT, .d$RESP))$p.value,
                   format = f_str("x.xxx", "p"), label = "p-value")
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("RESP", by = "PARAM", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx (xxx%)", "n", "pct")),
      assoc_test = at)))), d)

  expect_true("pval1" %in% names(b))
  expect_equal(attr(b$pval1, "label"), "p-value")
  b <- b[order(b$ord_layer_1), ]
  # exactly one non-blank p-value per PARAM
  alt <- b[b$rowlabel1 == "ALT", ]
  ast <- b[b$rowlabel1 == "AST", ]
  expect_equal(sum(trimws(alt$pval1) != ""), 1)
  expect_equal(sum(trimws(ast$pval1) != ""), 1)
  # value is on the first row of each group
  expect_true(trimws(alt$pval1[1]) != "")
})

test_that("count assoc_test with no by variable emits a single p-value", {
  d <- data.frame(TRT = factor(rep(c("A", "B"), each = 10)),
                  RESP = factor(rep(c("N", "H"), 10)))
  at <- assoc_test(fn = function(.d) fisher.test(table(.d$TRT, .d$RESP))$p.value)
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("RESP", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")), assoc_test = at)))), d)
  expect_equal(sum(trimws(b$pval1) != ""), 1)
})

test_that("assoc_test renders NA / errors as blank", {
  d <- data.frame(TRT = factor(rep(c("A", "B"), each = 4)),
                  RESP = factor(rep(c("N", "H"), 4)))
  at <- assoc_test(fn = function(.d) stop("boom"))   # always errors -> NA -> blank
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("RESP", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")), assoc_test = at)))), d)
  expect_true(all(trimws(b$pval1) == ""))
})

# --- shift layer ---

test_that("shift assoc_test emits a p-value column", {
  df <- data.frame(
    TRTP = factor(rep(c("P", "A"), each = 8), levels = c("P", "A")),
    BNRIND = factor(rep(c("N", "H"), 8), levels = c("N", "H")),
    ANRIND = factor(rep(c("N", "H", "N", "N"), 4), levels = c("N", "H")))
  at <- assoc_test(fn = function(.d) fisher.test(table(.d$TRTP, .d$ANRIND))$p.value,
                   label = "p")
  b <- tplyr_build(tplyr_spec(cols = "TRTP", layers = tplyr_layers(
    group_shift(c(row = "ANRIND", column = "BNRIND"), settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")), assoc_test = at)))), df)
  expect_true("pval1" %in% names(b))
  expect_equal(sum(trimws(b$pval1) != ""), 1)
})

# --- validation & serialization ---

test_that("validate rejects a non-assoc_test object", {
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(assoc_test = list(fn = identity)))))
  expect_error(tplyr2:::validate_spec(spec), "assoc_test")
})

test_that("assoc_test survives JSON serialization", {
  scratch <- file.path(tempdir(), "tplyr2_assoc"); dir.create(scratch, showWarnings = FALSE)
  at <- assoc_test(fn = function(.d) fisher.test(table(.d$TRT, .d$RESP))$p.value,
                   format = f_str("x.xxx", "p"), label = "p-value [1]")
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("RESP", settings = layer_settings(assoc_test = at))))
  path <- file.path(scratch, "assoc.json")
  tplyr_write_spec(spec, path)
  s <- tplyr_read_spec(path)$layers[[1]]$settings$assoc_test
  expect_s3_class(s, "tplyr_assoc_test")
  expect_equal(s$label, "p-value [1]")
  expect_true(is.function(s$fn))
})
