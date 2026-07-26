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

# --- pairwise / per-level mode (#40) ---

# Shared inline data mirroring the issue reprex
.assoc_pairwise_data <- function() {
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:15),
    TRT = c(rep("Placebo", 4), rep("Low", 5), rep("High", 6)),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S01", "S02", "S03", "S05", "S06", "S07", "S08",
                "S10", "S11", "S12", "S13", "S14"),
    TRT = c("Placebo", "Placebo", "Placebo", "Low", "Low", "Low", "Low",
            "High", "High", "High", "High", "High"),
    AEDECOD = c("HEADACHE", "HEADACHE", "NAUSEA", "HEADACHE", "NAUSEA",
                "NAUSEA", "NAUSEA", "HEADACHE", "HEADACHE", "HEADACHE",
                "NAUSEA", "NAUSEA"),
    stringsAsFactors = FALSE
  )
  list(adsl = adsl, adae = adae)
}

test_that("assoc_test constructor validates pairwise arguments", {
  # reference outside pairwise mode is rejected
  expect_error(assoc_test(fn = function(m) 1, reference = "A"),
               "only used in pairwise")
  # label length must match comparisons
  expect_error(
    assoc_test(fn = function(m) 1, comparisons = c("Low", "High"),
               label = c("a", "b", "c")),
    "one string per comparison"
  )
  # list comparisons with multi-element entry
  expect_error(
    assoc_test(fn = function(m) 1, comparisons = list(c("Low", "High"))),
    "single arm level"
  )
  # a valid pairwise object
  at <- assoc_test(fn = function(m) 1, reference = "Placebo",
                   comparisons = c("Low", "High"))
  expect_true(at$pairwise)
  expect_equal(at$comparisons, c("Low", "High"))
  expect_null(at$label)
  expect_output(print(at), "pairwise association test")
})

test_that("pairwise assoc_test reproduces the issue's Fisher p-values", {
  dat <- .assoc_pairwise_data()
  at <- assoc_test(
    fn = function(m) fisher.test(m)$p.value,
    reference = "Placebo", comparisons = c("Low", "High"),
    format = f_str("x.xxx", "p")
  )
  spec <- tplyr_spec(
    cols = "TRT", pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        stat_columns = list("n" = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")),
        assoc_test = at)))
  )
  b <- tplyr_build(spec, dat$adae, pop_data = dat$adsl)
  disp <- as.data.frame(as_display(b))

  expect_true(all(c("pval1", "pval2") %in% names(disp)))
  hd <- disp[disp$rowlabel1 == "HEADACHE", ]
  na <- disp[disp$rowlabel1 == "NAUSEA", ]
  expect_equal(trimws(hd$pval1), "0.524")
  expect_equal(trimws(hd$pval2), "1.000")
  expect_equal(trimws(na$pval1), "0.524")
  expect_equal(trimws(na$pval2), "1.000")
  # value on EVERY target-level row
  expect_true(all(trimws(disp$pval1) != ""))
  expect_true(all(trimws(disp$pval2) != ""))
  # default per-comparison labels
  expect_equal(attr(b$pval1, "label"), "Placebo vs Low")
  expect_equal(attr(b$pval2, "label"), "Placebo vs High")
})

test_that("pairwise assoc_test respects custom labels and default reference", {
  dat <- .assoc_pairwise_data()
  # default reference = first level of TRT (factor level order)
  dat$adae$TRT <- factor(dat$adae$TRT, levels = c("Placebo", "Low", "High"))
  at <- assoc_test(
    fn = function(m) fisher.test(m)$p.value,
    comparisons = c("Low", "High"),
    label = c("P vs L", "P vs H"),
    format = f_str("x.xxx", "p")
  )
  spec <- tplyr_spec(
    cols = "TRT", pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        stat_columns = list("n" = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")),
        assoc_test = at)))
  )
  b <- tplyr_build(spec, dat$adae, pop_data = dat$adsl)
  expect_equal(attr(b$pval1, "label"), "P vs L")
  expect_equal(attr(b$pval2, "label"), "P vs H")
  disp <- as.data.frame(as_display(b))
  hd <- disp[disp$rowlabel1 == "HEADACHE", ]
  expect_equal(trimws(hd$pval1), "0.524")
})

test_that("pairwise assoc_test uses non-distinct counts when distinct_by absent", {
  # With one record per subject, non-distinct n == distinct n, but exercise the
  # n/total path explicitly. The fn captures the 2x2 it receives.
  captured <- new.env()
  at <- assoc_test(
    fn = function(m) { captured$m <- m; fisher.test(m)$p.value },
    reference = "Placebo", comparisons = "Low",
    format = f_str("x.xxx", "p")
  )
  d <- data.frame(
    TRT = c(rep("Placebo", 4), rep("Low", 5)),
    AEDECOD = c("HEADACHE", "HEADACHE", "NAUSEA", "OTHER",
                "HEADACHE", "NAUSEA", "NAUSEA", "NAUSEA", "OTHER"),
    stringsAsFactors = FALSE
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("AEDECOD", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")),
      assoc_test = at))))
  b <- tplyr_build(spec, d)
  expect_true("pval1" %in% names(b))
  # 2x2 dims: rows = arms, cols = event/no-event
  expect_equal(dim(captured$m), c(2L, 2L))
})

test_that("pairwise assoc_test blanks special (total) rows", {
  dat <- .assoc_pairwise_data()
  at <- assoc_test(
    fn = function(m) fisher.test(m)$p.value,
    reference = "Placebo", comparisons = c("Low", "High"),
    format = f_str("x.xxx", "p")
  )
  spec <- tplyr_spec(
    cols = "TRT", pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID", total_row = TRUE,
        stat_columns = list("n" = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")),
        assoc_test = at)))
  )
  b <- tplyr_build(spec, dat$adae, pop_data = dat$adsl)
  disp <- as.data.frame(as_display(b))
  tot <- disp[disp$rowlabel1 == "Total", ]
  expect_equal(trimws(tot$pval1), "")
  expect_equal(trimws(tot$pval2), "")
})

test_that("pairwise assoc_test validation errors surface", {
  # no cols -> error at validate time
  at <- assoc_test(fn = function(m) 1, reference = "A",
                   comparisons = "B")
  spec <- tplyr_spec(cols = character(0), layers = tplyr_layers(
    group_count("V", settings = layer_settings(assoc_test = at))))
  expect_error(tplyr2:::validate_spec(spec), "at least one column variable")

  # reference appearing in comparisons
  at2 <- assoc_test(fn = function(m) 1, reference = "A",
                    comparisons = c("A", "B"))
  spec2 <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(assoc_test = at2))))
  expect_error(tplyr2:::validate_spec(spec2), "must not also appear")
})

test_that("pairwise assoc_test survives JSON serialization", {
  scratch <- file.path(tempdir(), "tplyr2_assoc_pw"); dir.create(scratch, showWarnings = FALSE)
  at <- assoc_test(
    fn = function(m) fisher.test(m)$p.value,
    reference = "Placebo", comparisons = c("Low", "High"),
    format = f_str("x.xxx", "p"), label = c("P vs L", "P vs H")
  )
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("AEDECOD", settings = layer_settings(assoc_test = at))))
  path <- file.path(scratch, "assoc_pw.json")
  tplyr_write_spec(spec, path)
  s <- tplyr_read_spec(path)$layers[[1]]$settings$assoc_test
  expect_s3_class(s, "tplyr_assoc_test")
  expect_true(s$pairwise)
  expect_equal(s$reference, "Placebo")
  expect_equal(s$comparisons, c("Low", "High"))
  expect_equal(s$label, c("P vs L", "P vs H"))
  expect_true(is.function(s$fn))
})
