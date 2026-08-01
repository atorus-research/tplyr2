# --- assoc_test() constructor ---

test_that("assoc_test validates its arguments", {
  expect_error(assoc_test(fn = "notafn"), "must be a function")
  expect_error(assoc_test(fn = function(d) 1, format = "x.xxx"), "f_str")
  # multi-variable formats are accepted (issue #60): fn may return a tuple
  expect_s3_class(
    assoc_test(fn = function(d) c(1, 2), format = f_str("xx (xx)", "a", "b")),
    "tplyr_assoc_test"
  )
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
  # Cells stay blank (the documented contract), but the reason is reported (#75)
  expect_warning(
    b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_count("RESP", settings = layer_settings(
        format_strings = list(n_counts = f_str("xx", "n")), assoc_test = at)))), d),
    "assoc_test fn.*boom")
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
  # omnibus label must be a single string
  expect_error(assoc_test(fn = function(d) 1, label = c("a", "b")),
               "single character string in omnibus")
  # empty comparisons
  expect_error(assoc_test(fn = function(m) 1, comparisons = character(0)),
               "at least one arm level")
  # reference must be scalar in pairwise mode
  expect_error(
    assoc_test(fn = function(m) 1, reference = c("A", "B"),
               comparisons = "Low"),
    "single arm level"
  )
  # comparisons supplied as a list of single levels normalizes to a vector
  at_list <- assoc_test(fn = function(m) 1, reference = "Placebo",
                        comparisons = list("Low", "High"))
  expect_equal(at_list$comparisons, c("Low", "High"))

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

test_that("format_assoc_return renders numeric, character, and blank returns", {
  fmt <- f_str("x.xxx", "p")
  # numeric -> formatted with the f_str
  expect_equal(trimws(tplyr2:::format_assoc_return(0.0312, fmt)), "0.031")
  # numeric NA -> blank
  expect_identical(tplyr2:::format_assoc_return(NA_real_, fmt), "")
  # character -> verbatim, format ignored (issue #47)
  expect_identical(tplyr2:::format_assoc_return("0.031*", fmt), "0.031*")
  expect_identical(tplyr2:::format_assoc_return(">.99", fmt), ">.99")
  expect_identical(tplyr2:::format_assoc_return("NE", fmt), "NE")
  expect_identical(tplyr2:::format_assoc_return("0.524 ", fmt), "0.524 ")
  # character NA / logical NA -> blank
  expect_identical(tplyr2:::format_assoc_return(NA_character_, fmt), "")
  expect_identical(tplyr2:::format_assoc_return(NA, fmt), "")
  # wrong length / empty -> blank
  expect_identical(tplyr2:::format_assoc_return(c(1, 2), fmt), "")
  expect_identical(tplyr2:::format_assoc_return(character(0), fmt), "")
})

test_that("pairwise assoc_test passes a character fn return through verbatim (#47)", {
  dat <- .assoc_pairwise_data()
  # AE-style display keyed on the raw p: '*' flag if p < .15, '>.99' ceiling,
  # trailing space to align with flagged rows, blank when both arms have 0.
  ae_disp <- function(m) {
    if (sum(m[, 1]) == 0) return(NA_character_)
    p <- fisher.test(m)$p.value
    d <- formatC(round(p, 3), format = "f", digits = 3)
    if (p > .99) ">.99" else if (p < .15) paste0(d, "*") else paste0(d, " ")
  }
  at <- assoc_test(
    fn = ae_disp,
    # format is intentionally different from the display to prove it is ignored
    # for character returns
    format = f_str("xxxxx", "p"),
    reference = "Placebo", comparisons = c("Low", "High")
  )
  spec <- tplyr_spec(
    cols = "TRT", pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("AEDECOD",
      settings = layer_settings(
        distinct_by = "USUBJID",
        stat_columns = list("n" = f_str("xx", "distinct_n")),
        assoc_test = at)))
  )
  b <- tplyr_build(spec, dat$adae, pop_data = dat$adsl)
  disp <- as.data.frame(as_display(b))

  hd <- disp[disp$rowlabel1 == "HEADACHE", ]
  na <- disp[disp$rowlabel1 == "NAUSEA", ]
  # p = 0.524 -> not flagged, trailing-space verbatim; p = 1.000 -> ">.99"
  expect_identical(hd$pval1, "0.524 ")
  expect_identical(hd$pval2, ">.99")
  expect_identical(na$pval1, "0.524 ")
  expect_identical(na$pval2, ">.99")
})

test_that("omnibus assoc_test passes a character fn return through verbatim (#47)", {
  dat <- .assoc_pairwise_data()
  at <- assoc_test(
    fn = function(.data) {
      p <- fisher.test(table(.data$TRT, .data$AEDECOD))$p.value
      if (p > .99) ">.99" else paste0(formatC(p, format = "f", digits = 3), "*")
    },
    format = f_str("xxxxx", "p")
  )
  spec <- tplyr_spec(
    cols = "TRT", pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("AEDECOD",
      settings = layer_settings(distinct_by = "USUBJID", assoc_test = at)))
  )
  b <- tplyr_build(spec, dat$adae, pop_data = dat$adsl)
  # the single omnibus value lands on the first row, verbatim
  expect_true("pval1" %in% names(b))
  first_val <- b$pval1[trimws(b$pval1) != ""][1]
  expect_true(grepl("\\*$", first_val) || first_val == ">.99")
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

test_that("pairwise assoc_test defaults reference to first level of a character col", {
  dat <- .assoc_pairwise_data()  # TRT is character: Placebo appears first
  at <- assoc_test(
    fn = function(m) fisher.test(m)$p.value,
    comparisons = c("Low", "High"),  # no reference -> first appearance = Placebo
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
  expect_equal(attr(b$pval1, "label"), "Placebo vs Low")
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

test_that("pairwise assoc_test places p-values per (by, level) with a by variable", {
  set.seed(11)
  d <- data.frame(
    TRT = c(rep("Placebo", 8), rep("Active", 8)),
    SOC = rep(c("CARDIAC", "GI"), each = 4, times = 2),
    AEDECOD = rep(c("A", "B"), times = 8),
    stringsAsFactors = FALSE
  )
  pop <- data.frame(TRT = c(rep("Placebo", 10), rep("Active", 10)),
                    stringsAsFactors = FALSE)
  # single label -> recycled across the (one) comparison
  at <- assoc_test(
    fn = function(m) fisher.test(m)$p.value,
    reference = "Placebo", comparisons = "Active",
    format = f_str("x.xxx", "p"), label = "P vs A"
  )
  spec <- tplyr_spec(
    cols = "TRT", pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("AEDECOD", by = "SOC",
      settings = layer_settings(
        format_strings = list(n_counts = f_str("xx", "n")),
        assoc_test = at)))
  )
  b <- tplyr_build(spec, d, pop_data = pop)
  expect_true("pval1" %in% names(b))
  expect_equal(attr(b$pval1, "label"), "P vs A")
  disp <- as.data.frame(as_display(b))
  # a value on every target-level row (all SOC x PT combinations)
  expect_true(all(trimws(disp$pval1) != ""))
})

.assoc_nested_data <- function() {
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:15),
    TRT = c(rep("Placebo", 4), rep("Low", 5), rep("High", 6)),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S01", "S02", "S03", "S05", "S06", "S07", "S08",
                "S10", "S11", "S12", "S13", "S14"),
    TRT = c(rep("Placebo", 3), rep("Low", 4), rep("High", 5)),
    SOC = c("NERVOUS", "NERVOUS", "GI", "NERVOUS", "GI", "GI", "GI",
            "NERVOUS", "NERVOUS", "NERVOUS", "GI", "GI"),
    PT = c("HEADACHE", "HEADACHE", "NAUSEA", "HEADACHE", "NAUSEA", "NAUSEA",
           "NAUSEA", "HEADACHE", "HEADACHE", "HEADACHE", "NAUSEA", "NAUSEA"),
    stringsAsFactors = FALSE
  )
  list(adsl = adsl, adae = adae)
}

.nested_spec <- function(at, total_row = FALSE) {
  tplyr_spec(
    cols = "TRT", pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count(c("SOC", "PT"),
      settings = layer_settings(
        distinct_by = "USUBJID", total_row = total_row,
        total_row_label = "ANY EVENT",
        stat_columns = list("n" = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")),
        assoc_test = at)))
  )
}

test_that("pairwise assoc_test emits p-values on inner and outer nested rows (#49)", {
  dat <- .assoc_nested_data()
  fish <- function(m) if (sum(m[, 1]) == 0) NA_real_ else fisher.test(m)$p.value
  at <- assoc_test(fn = fish, format = f_str("x.xxx", "p"),
                   reference = "Placebo", comparisons = c("Low", "High"))
  b <- tplyr_build(.nested_spec(at), dat$adae, pop_data = dat$adsl)
  disp <- as.data.frame(as_display(b))

  expect_true(all(c("pval1", "pval2") %in% names(disp)))
  # Every category row (SOC subtotal AND PT) carries a value
  expect_true(all(trimws(disp$pval1) != ""))
  expect_true(all(trimws(disp$pval2) != ""))
  # default labels
  expect_equal(attr(b$pval1, "label"), "Placebo vs Low")
  expect_equal(attr(b$pval2, "label"), "Placebo vs High")

  # Outer (SOC subtotal) rows: rowlabel2 == ""
  soc_rows <- disp[disp$rowlabel2 == "", ]
  expect_equal(nrow(soc_rows), 2L)  # GI + NERVOUS
  expect_true(all(trimws(soc_rows$pval1) != ""))

  # A nested PT row's p-value equals the single-level p for the same PT/arm,
  # since the 2x2 is built from the same distinct counts + denominators.
  single <- as.data.frame(as_display(tplyr_build(
    tplyr_spec(cols = "TRT", pop_data = pop_data(cols = "TRT"),
      layers = tplyr_layers(group_count("PT", settings = layer_settings(
        distinct_by = "USUBJID",
        stat_columns = list("n" = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")),
        assoc_test = at)))),
    dat$adae, pop_data = dat$adsl)))
  nausea_nested <- disp[disp$rowlabel2 == "NAUSEA", ]
  nausea_single <- single[single$rowlabel1 == "NAUSEA", ]
  expect_equal(trimws(nausea_nested$pval1), trimws(nausea_single$pval1))
  expect_equal(trimws(nausea_nested$pval2), trimws(nausea_single$pval2))
})

test_that("nested pairwise total_row toggles the grand-total p-value (#49)", {
  dat <- .assoc_nested_data()
  fish <- function(m) if (sum(m[, 1]) == 0) NA_real_ else fisher.test(m)$p.value

  # default total_row = TRUE -> grand-total row gets a p-value
  at_on <- assoc_test(fn = fish, format = f_str("x.xxx", "p"),
                      reference = "Placebo", comparisons = c("Low", "High"))
  disp_on <- as.data.frame(as_display(
    tplyr_build(.nested_spec(at_on, total_row = TRUE), dat$adae, pop_data = dat$adsl)))
  tot_on <- disp_on[disp_on$rowlabel1 == "ANY EVENT", ]
  expect_equal(nrow(tot_on), 1L)
  expect_true(trimws(tot_on$pval1) != "")
  expect_true(trimws(tot_on$pval2) != "")

  # total_row = FALSE -> grand-total row blank, category rows unaffected
  at_off <- assoc_test(fn = fish, format = f_str("x.xxx", "p"),
                       reference = "Placebo", comparisons = c("Low", "High"),
                       total_row = FALSE)
  disp_off <- as.data.frame(as_display(
    tplyr_build(.nested_spec(at_off, total_row = TRUE), dat$adae, pop_data = dat$adsl)))
  tot_off <- disp_off[disp_off$rowlabel1 == "ANY EVENT", ]
  expect_equal(trimws(tot_off$pval1), "")
  expect_equal(trimws(tot_off$pval2), "")
  cat_off <- disp_off[disp_off$rowlabel1 != "ANY EVENT", ]
  expect_true(all(trimws(cat_off$pval1) != ""))
})

test_that("nested pairwise assoc_test handles a zero-event reference arm (#49)", {
  # Placebo has NO events at all: its 2x2 is 0-vs-k, still a valid test. The
  # reference arm's denominator must come from pop_data, not the (empty)
  # observed counts, so every SOC/PT/total row still gets a p-value.
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:15),
    TRT = factor(c(rep("Placebo", 4), rep("Low", 5), rep("High", 6)),
                 levels = c("Placebo", "Low", "High")),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S05", "S06", "S10", "S11"),
    TRT = factor(c("Low", "Low", "High", "High"),
                 levels = c("Placebo", "Low", "High")),
    SOC = c("GI", "NERVOUS", "GI", "NERVOUS"),
    PT = c("NAUSEA", "SYNCOPE", "NAUSEA", "SYNCOPE"),
    stringsAsFactors = FALSE
  )

  calls <- 0
  fp <- function(m) {
    calls <<- calls + 1
    if (sum(m[, 1]) == 0) NA_real_ else fisher.test(m)$p.value
  }
  at <- assoc_test(fn = fp, format = f_str("x.xxx", "p"),
                   reference = "Placebo", comparisons = c("Low", "High"))
  spec <- tplyr_spec(
    cols = "TRT", pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count(c("SOC", "PT"),
      settings = layer_settings(
        distinct_by = "USUBJID", total_row = TRUE, total_row_label = "ANY",
        stat_columns = list("n" = f_str("xx", "distinct_n")),
        assoc_test = at)))
  )
  disp <- as.data.frame(as_display(tplyr_build(spec, adae, pop_data = adsl)))

  # fn is actually called for the 0-vs-k comparisons (not short-circuited)
  expect_gt(calls, 0)
  # every category + total row carries a p-value for both comparisons
  expect_true(all(trimws(disp$pval1) != ""))
  expect_true(all(trimws(disp$pval2) != ""))

  # a 0-vs-1 category row matches a direct Fisher on the same 2x2
  gi_nausea <- disp[disp$rowlabel1 == "GI" & disp$rowlabel2 == "NAUSEA", ]
  expected <- fisher.test(matrix(c(0, 1, 4, 4), nrow = 2))$p.value
  expect_equal(trimws(gi_nausea$pval1),
               trimws(apply_formats(f_str("x.xxx", "p"), expected)))
})

test_that("nested pairwise assoc_test passes a character fn return verbatim (#47 + #49)", {
  dat <- .assoc_nested_data()
  ae_disp <- function(m) {
    if (sum(m[, 1]) == 0) return(NA_character_)
    p <- fisher.test(m)$p.value
    if (p > .99) ">.99" else paste0(formatC(round(p, 3), format = "f", digits = 3), "*")
  }
  at <- assoc_test(fn = ae_disp, format = f_str("xxxxx", "p"),
                   reference = "Placebo", comparisons = c("Low", "High"))
  disp <- as.data.frame(as_display(
    tplyr_build(.nested_spec(at), dat$adae, pop_data = dat$adsl)))
  # every category cell is a verbatim fn string: a flagged p or the >.99 ceiling
  vals <- c(disp$pval1, disp$pval2)
  vals <- vals[trimws(vals) != ""]
  expect_true(length(vals) > 0)
  expect_true(all(grepl("\\*$", vals) | vals == ">.99"))
})

test_that("assoc_test validates total_row and round-trips it through JSON", {
  expect_error(assoc_test(fn = function(m) 1, comparisons = "Low",
                          total_row = "yes"),
               "single logical")
  expect_error(assoc_test(fn = function(m) 1, comparisons = "Low",
                          total_row = NA),
               "single logical")

  at <- assoc_test(fn = function(m) fisher.test(m)$p.value,
                   reference = "Placebo", comparisons = c("Low", "High"),
                   total_row = FALSE)
  expect_false(at$total_row)

  spec <- tplyr_spec(cols = "TRT",
    layers = tplyr_layers(group_count(c("SOC", "PT"),
      settings = layer_settings(assoc_test = at))))
  tmp <- withr::local_tempfile(fileext = ".json")
  tplyr_write_spec(spec, tmp)
  spec2 <- tplyr_read_spec(tmp)
  expect_false(spec2$layers[[1]]$settings$assoc_test$total_row)
})

test_that("compute_pairwise_assoc handles zero denominators and bad fn returns", {
  counts <- data.table::data.table(
    TRT = c("Placebo", "Low", "Placebo", "Low"),
    AEDECOD = c("A", "A", "B", "B"),
    n = c(2, 1, 1, 0), total = c(4, 0, 4, 5)  # A/Low has total 0 -> NA
  )
  at <- assoc_test(fn = function(m) fisher.test(m)$p.value,
                   reference = "Placebo", comparisons = "Low",
                   format = f_str("x.xxx", "p"))
  res <- tplyr2:::compute_pairwise_assoc(counts, "TRT", "AEDECOD",
                                         character(0), NULL, at, "Placebo")
  # Zero denominator -> blank display; valid cell -> formatted display
  expect_identical(res$.disp[res$AEDECOD == "A"], "")
  expect_true(trimws(res$.disp[res$AEDECOD == "B"]) != "")

  # fn returning a non-scalar collapses to a blank
  at2 <- assoc_test(fn = function(m) c(1, 2),
                    reference = "Placebo", comparisons = "Low",
                    format = f_str("x.xxx", "p"))
  res2 <- tplyr2:::compute_pairwise_assoc(counts, "TRT", "AEDECOD",
                                          character(0), NULL, at2, "Placebo")
  expect_true(all(res2$.disp == ""))

  # resolve_assoc_reference guards against missing cols (no explicit reference)
  at_noref <- assoc_test(fn = function(m) 1, comparisons = "Low",
                         format = f_str("x.xxx", "p"))
  expect_error(
    tplyr2:::resolve_assoc_reference(at_noref, data.frame(x = 1), character(0)),
    "at least one column variable"
  )

  # compute_pairwise_assoc guards against missing cols
  expect_error(
    tplyr2:::compute_pairwise_assoc(counts, character(0), "AEDECOD",
                                    character(0), NULL, at, "Placebo"),
    "at least one column variable"
  )
})

test_that("merge_pairwise_assoc emits blank labelled columns when there is no data", {
  wide <- data.table::data.table(rowlabel1 = c("A", "B"), res1 = c("1", "2"))
  at <- assoc_test(fn = function(m) 1, reference = "P",
                   comparisons = c("Low", "High"), format = f_str("x.xxx", "p"))
  tplyr2:::merge_pairwise_assoc(wide, NULL, at, "AEDECOD",
                                character(0), character(0), "P")
  expect_true(all(c("pval1", "pval2") %in% names(wide)))
  expect_equal(attr(wide$pval1, "label"), "P vs Low")
  expect_equal(attr(wide$pval2, "label"), "P vs High")
  expect_true(all(wide$pval1 == "") && all(wide$pval2 == ""))

  # No rowlabel columns present: returns early without error
  wide2 <- data.table::data.table(res1 = "1")
  ad <- data.table::data.table(.comp_idx = 1L, AEDECOD = "A", p = 0.5)
  at1 <- assoc_test(fn = function(m) 1, reference = "P",
                    comparisons = "Low", format = f_str("x.xxx", "p"))
  out <- tplyr2:::merge_pairwise_assoc(wide2, ad, at1, "AEDECOD",
                                       character(0), character(0), "P")
  expect_identical(names(out), "res1")
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

# --- desc layer (omnibus only, #51) ---

.desc_assoc_data <- function() {
  set.seed(1)
  data.frame(
    TRT = factor(rep(c("Placebo", "Low", "High"), each = 20),
                 levels = c("Placebo", "Low", "High")),
    AGE = c(rnorm(20, 75, 8), rnorm(20, 74, 8), rnorm(20, 76, 8)),
    SEX = sample(c("M", "F"), 60, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("desc assoc_test places one omnibus p-value on the first stat row (#51)", {
  d <- .desc_assoc_data()
  aov_fn <- function(.data) anova(lm(AGE ~ TRT, .data))[["Pr(>F)"]][1]
  at <- assoc_test(fn = aov_fn, format = f_str("x.xxx", "p"), label = "ANOVA p")
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(
      format_strings = list(Mean = f_str("xx.x", "mean"), SD = f_str("xx.xx", "sd")),
      assoc_test = at)))), d)

  expect_true("pval1" %in% names(b))
  expect_equal(attr(b$pval1, "label"), "ANOVA p")
  disp <- as.data.frame(as_display(b))
  # value on the first (Mean) row, blank on the rest
  expect_equal(sum(trimws(disp$pval1) != ""), 1L)
  expect_true(trimws(disp$pval1[disp$rowlabel1 == "Mean"]) != "")
  expect_equal(trimws(disp$pval1[disp$rowlabel1 == "SD"]), "")
  # matches a direct ANOVA on the whole layer
  expected <- anova(lm(AGE ~ TRT, d))[["Pr(>F)"]][1]
  expect_equal(trimws(disp$pval1[disp$rowlabel1 == "Mean"]),
               trimws(apply_formats(f_str("x.xxx", "p"), expected)))
})

test_that("desc assoc_test emits one p-value per by-group (#51)", {
  d <- .desc_assoc_data()
  # long demographics-style frame: two continuous characteristics stacked
  d2 <- rbind(
    data.frame(TRT = d$TRT, VAL = d$AGE, CHAR = "Age"),
    data.frame(TRT = d$TRT, VAL = rnorm(60, 25, 4), CHAR = "BMI")
  )
  at <- assoc_test(fn = function(.data) anova(lm(VAL ~ TRT, .data))[["Pr(>F)"]][1],
                   format = f_str("x.xxx", "p"))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("VAL", by = "CHAR", settings = layer_settings(
      format_strings = list(Mean = f_str("xx.x", "mean"), SD = f_str("xx.xx", "sd")),
      assoc_test = at)))), d2)
  disp <- as.data.frame(as_display(b))

  # one non-blank p-value per characteristic, on that group's first row
  age <- disp[disp$rowlabel1 == "Age", ]
  bmi <- disp[disp$rowlabel1 == "BMI", ]
  expect_equal(sum(trimws(age$pval1) != ""), 1L)
  expect_equal(sum(trimws(bmi$pval1) != ""), 1L)
  expect_true(trimws(age$pval1[1]) != "")
  # each group's value matches its own ANOVA
  exp_age <- anova(lm(VAL ~ TRT, d2[d2$CHAR == "Age", ]))[["Pr(>F)"]][1]
  expect_equal(trimws(age$pval1[trimws(age$pval1) != ""]),
               trimws(apply_formats(f_str("x.xxx", "p"), exp_age)))
})

test_that("desc assoc_test passes a character fn return through verbatim (#51)", {
  d <- .desc_assoc_data()
  at <- assoc_test(
    fn = function(.data) {
      p <- anova(lm(AGE ~ TRT, .data))[["Pr(>F)"]][1]
      if (p < 0.05) sprintf("%.3f*", p) else sprintf("%.3f ", p)
    },
    format = f_str("x.xxx", "p"))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(
      format_strings = list(Mean = f_str("xx.x", "mean")),
      assoc_test = at)))), d)
  val <- b$pval1[trimws(b$pval1) != ""][1]
  expect_true(grepl("\\*$", val) || grepl("\\d $", val))
})

test_that("desc assoc_test renders NA as blank (#51)", {
  d <- .desc_assoc_data()
  at <- assoc_test(fn = function(.data) NA_real_)
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(
      format_strings = list(Mean = f_str("xx.x", "mean")),
      assoc_test = at)))), d)
  expect_true(all(trimws(b$pval1) == ""))
})

test_that("pairwise assoc_test is rejected on a desc layer (#51)", {
  d <- .desc_assoc_data()
  at <- assoc_test(fn = function(m) 1, reference = "Placebo",
                   comparisons = c("Low", "High"))
  expect_error(
    tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_desc("AGE", settings = layer_settings(assoc_test = at)))), d),
    "only supported on count layers"
  )
})

test_that("count and desc assoc p-values share one pval column in a spec (#51)", {
  d <- .desc_assoc_data()
  count_at <- assoc_test(
    fn = function(.data) chisq.test(table(.data$TRT, .data$SEX))$p.value,
    format = f_str("x.xxx", "p"))
  desc_at <- assoc_test(
    fn = function(.data) anova(lm(AGE ~ TRT, .data))[["Pr(>F)"]][1],
    format = f_str("x.xxx", "p"))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(
      format_strings = list(Mean = f_str("xx.x", "mean")), assoc_test = desc_at)),
    group_count("SEX", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")), assoc_test = count_at)))), d)

  expect_true("pval1" %in% names(b))
  # both layers contribute a value into the shared column
  expect_gte(sum(trimws(b$pval1) != ""), 2L)
})

# --- #53: total_group / custom_group rows must not leak into fn ---

test_that("omnibus assoc_test excludes total_group duplicate rows from fn (#53)", {
  adsl <- data.frame(
    USUBJID = 1:40,
    TRT01P = factor(rep(c("Placebo", "Low", "High", "Xtra"), each = 10),
                    levels = c("Placebo", "Low", "High", "Xtra")),
    AGEGR1 = c(rep("<65", 6), rep(">=65", 4), rep("<65", 3), rep(">=65", 7),
               rep("<65", 8), rep(">=65", 2), rep("<65", 5), rep(">=65", 5)),
    stringsAsFactors = FALSE)

  seen <- NULL
  chi_fn <- function(.data) {
    seen <<- .data
    sprintf("%.4f", suppressWarnings(
      chisq.test(table(.data$TRT01P, .data$AGEGR1))$p.value))
  }
  b <- tplyr_build(tplyr_spec(cols = "TRT01P",
    total_groups = list(total_group("TRT01P")),
    layers = tplyr_layers(group_count("AGEGR1", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")),
      assoc_test = assoc_test(fn = chi_fn, format = f_str("x.xxxx", "p")))))), adsl)

  # fn saw only the 40 real rows, no synthetic "Total" arm, no marker column
  expect_equal(nrow(seen), 40L)
  expect_false("Total" %in% as.character(seen$TRT01P))
  expect_false("Total" %in% levels(seen$TRT01P))   # phantom level dropped too
  expect_false(".tplyr_synthetic" %in% names(seen))
  # and the reported p equals a direct test on the real data
  expected <- sprintf("%.4f", suppressWarnings(
    chisq.test(table(adsl$TRT01P, adsl$AGEGR1))$p.value))
  expect_equal(trimws(b$pval1[b$pval1 != ""][1]), expected)
})

test_that("omnibus assoc_test excludes custom_group duplicate rows from fn (#53)", {
  d <- data.frame(
    TRT = factor(rep(c("Placebo", "Low", "High"), each = 10),
                 levels = c("Placebo", "Low", "High")),
    RESP = factor(rep(c("N", "Y"), 15), levels = c("N", "Y")),
    stringsAsFactors = FALSE)
  seen_n <- NULL
  b <- tplyr_build(tplyr_spec(cols = "TRT",
    custom_groups = list(custom_group("TRT", "Active" = c("Low", "High"))),
    layers = tplyr_layers(group_count("RESP", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")),
      assoc_test = assoc_test(
        fn = function(.data) { seen_n <<- nrow(.data); 0.5 },
        format = f_str("x.xxx", "p")))))), d)
  # the "Active" duplicates (20 rows) are excluded -> fn sees only the 30 real
  expect_equal(seen_n, 30L)
})

# --- #54: omnibus value lands on the first DISPLAY row, not the pre-sort row ---

test_that("omnibus assoc_test places the value on the first display row (#54)", {
  d2 <- data.frame(
    TRT = factor(rep(c("A", "B"), each = 15), levels = c("A", "B")),
    AGEGR1 = factor(c(rep("<65", 8), rep("65-80", 4), rep(">80", 3),
                      rep("<65", 3), rep("65-80", 9), rep(">80", 3)),
                    levels = c("<65", "65-80", ">80")),
    stringsAsFactors = FALSE)
  at <- assoc_test(
    fn = function(.data) suppressWarnings(
      chisq.test(table(.data$TRT, .data$AGEGR1))$p.value),
    format = f_str("x.xxx", "p"))
  disp <- as.data.frame(as_display(tplyr_build(tplyr_spec(cols = "TRT",
    layers = tplyr_layers(group_count("AGEGR1", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")),
      order_count_method = "byfactor", assoc_test = at)))), d2)))

  # value on the first factor-order row (<65), blank on the rest
  expect_true(trimws(disp$pval1[disp$rowlabel1 == "<65"]) != "")
  expect_equal(trimws(disp$pval1[disp$rowlabel1 == "65-80"]), "")
  expect_equal(trimws(disp$pval1[disp$rowlabel1 == ">80"]), "")
})

test_that("omnibus assoc_test lands on each by-group's first display row (#54)", {
  set.seed(9)
  d <- data.frame(
    TRT = factor(rep(c("A", "B"), each = 30), levels = c("A", "B")),
    PARAM = factor(rep(c("ALT", "AST"), 30), levels = c("ALT", "AST")),
    GR = factor(sample(c("Hi", "Lo", "Mid"), 60, replace = TRUE),
                levels = c("Lo", "Mid", "Hi")),
    stringsAsFactors = FALSE)
  at <- assoc_test(
    fn = function(.data) suppressWarnings(
      chisq.test(table(.data$TRT, .data$GR))$p.value),
    format = f_str("x.xxx", "p"))
  disp <- as.data.frame(as_display(tplyr_build(tplyr_spec(cols = "TRT",
    layers = tplyr_layers(group_count("GR", by = "PARAM", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")),
      order_count_method = "byfactor", assoc_test = at)))), d)))

  # within each PARAM, exactly one value and it is on the first factor row (Lo)
  for (p in c("ALT", "AST")) {
    grp <- disp[disp$rowlabel1 == p, ]
    grp <- grp[order(match(grp$rowlabel2, c("Lo", "Mid", "Hi"))), ]
    expect_equal(sum(trimws(grp$pval1) != ""), 1L)
    expect_true(trimws(grp$pval1[grp$rowlabel2 == "Lo"]) != "")
  }
})

# --- Multiple values in one cell (#60) ---

test_that("format_assoc_return maps a numeric vector onto a multi-variable f_str (#60)", {
  fmt <- f_str("xx.xx (xx.xx, xx.xx)", "or", "lo", "hi")
  # three values, three variables -> one formatted cell (fields are width-padded)
  cell <- tplyr2:::format_assoc_return(c(1.85, 1.10, 3.02), fmt)
  expect_match(cell, "^\\s*1\\.85 \\(\\s*1\\.10,\\s*3\\.02\\)$")
  # arity mismatch (too few / too many) -> blank
  expect_identical(tplyr2:::format_assoc_return(c(1.85, 1.10), fmt), "")
  expect_identical(tplyr2:::format_assoc_return(c(1, 2, 3, 4), fmt), "")
  # all-NA -> blank; a single NA field blanks just that field
  expect_identical(tplyr2:::format_assoc_return(c(NA_real_, NA_real_, NA_real_), fmt), "")
  partial <- tplyr2:::format_assoc_return(c(1.85, NA_real_, 3.02), fmt)
  expect_true(grepl("1.85", partial))
  # character escape hatch still wins, even with a multi-variable format
  expect_identical(tplyr2:::format_assoc_return("NE", fmt), "NE")
  # scalar + one-variable format is unchanged
  expect_equal(trimws(tplyr2:::format_assoc_return(0.0312, f_str("x.xxx", "p"))), "0.031")
})

test_that("assoc_test constructor accepts a multi-variable format (#60)", {
  at <- assoc_test(fn = function(m) c(1, 2, 3),
                   format = f_str("xx (xx, xx)", "or", "lo", "hi"),
                   reference = "Placebo", comparisons = "Low")
  expect_s3_class(at, "tplyr_assoc_test")
  expect_equal(length(at$format$vars), 3L)
})

test_that("pairwise assoc_test renders an odds ratio with CI in one cell (#60)", {
  set.seed(4)
  d <- data.frame(
    TRT = factor(rep(c("Placebo", "Low", "High"), each = 40),
                 levels = c("Placebo", "Low", "High")),
    RESP = factor(sample(c("Y", "N"), 120, replace = TRUE), levels = c("N", "Y")))
  at <- assoc_test(
    fn = function(m) {
      ft <- suppressWarnings(fisher.test(m))
      c(ft$estimate, ft$conf.int[1], ft$conf.int[2])
    },
    reference = "Placebo", comparisons = c("Low", "High"),
    format = f_str("xx.xx (xx.xx, xx.xx)", "or", "lo", "hi"),
    label = "OR (95% CI)")
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("RESP", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")), assoc_test = at)))), d)

  expect_true(all(c("pval1", "pval2") %in% names(b)))
  expect_equal(attr(b$pval1, "label"), "OR (95% CI)")
  # cells look like "or (lo, hi)"
  vals <- b$pval1[trimws(b$pval1) != ""]
  expect_true(all(grepl("^\\s*[0-9.]+ \\([0-9. ]+, [0-9. ]+\\)$", vals)))
})

test_that("omnibus assoc_test renders multiple statistics in one cell (#60)", {
  set.seed(5)
  d <- data.frame(
    TRT = factor(rep(c("A", "B"), each = 30)),
    RESP = factor(sample(c("Y", "N"), 60, replace = TRUE)))
  at <- assoc_test(
    fn = function(.data) {
      ch <- suppressWarnings(chisq.test(table(.data$TRT, .data$RESP)))
      c(unname(ch$statistic), ch$p.value)
    },
    format = f_str("xx.x (p=x.xxx)", "stat", "p"), label = "chi-square")
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("RESP", settings = layer_settings(
      format_strings = list(n_counts = f_str("xx", "n")), assoc_test = at)))), d)
  val <- b$pval1[trimws(b$pval1) != ""][1]
  expect_true(grepl("p=", val))
})

# --- by leading with a string label (#72) ---

test_that("pairwise p-values populate when `by` leads with a label (#72)", {
  mk <- function(by) tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count("AGEGR1", by = by, settings = layer_settings(
        assoc_test = assoc_test(
          fn = function(d) suppressWarnings(stats::chisq.test(d)$p.value),
          format = f_str("x.xxx", "pval"),
          reference = "Placebo",
          comparisons = c("Xanomeline High Dose"))))
    )
  )
  plain   <- tplyr_build(mk("SEX"), tplyr_adsl)
  labeled <- tplyr_build(mk(c("Age Group", "SEX")), tplyr_adsl)

  # Same head(all_label_cols, ...) offset bug as merge_risk_diff_columns().
  expect_true(all(labeled$pval1 != ""))
  expect_equal(as.vector(labeled$pval1), as.vector(plain$pval1))
})
