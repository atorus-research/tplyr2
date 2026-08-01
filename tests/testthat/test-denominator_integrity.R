# --- Denominator integrity guards (#76) ---

test_that("a column level absent from pop_data warns (#76)", {
  d <- data.frame(TRT = c(rep("Placebo", 3), rep("High Dose", 2)),
                  VAL = c("X", "Y", "X", "X", "Y"), stringsAsFactors = FALSE)
  pop <- data.frame(TRT = rep("Placebo", 6), stringsAsFactors = FALSE)
  spec <- tplyr_spec(cols = "TRT", pop_data = pop_data("TRT"),
                     layers = tplyr_layers(group_count("VAL")))

  # Both the coverage warning and the downstream integrity warning fire.
  warns <- testthat::capture_warnings(tplyr_build(spec, d, pop_data = pop))
  expect_true(any(str_detect(warns,
                             "pop_data has no rows for 'TRT' level: High Dose")))
  expect_true(any(str_detect(warns, "pop_data levels: Placebo")))
})

test_that("n > 0 against a missing denominator warns and renders blank (#76)", {
  d <- data.frame(TRT = c(rep("Placebo", 3), rep("High Dose", 2)),
                  VAL = c("X", "Y", "X", "X", "Y"), stringsAsFactors = FALSE)
  pop <- data.frame(TRT = rep("Placebo", 6), stringsAsFactors = FALSE)
  spec <- tplyr_spec(cols = "TRT", pop_data = pop_data("TRT"),
                     layers = tplyr_layers(group_count("VAL")))

  warns <- testthat::capture_warnings(out <- tplyr_build(spec, d, pop_data = pop))
  expect_true(any(str_detect(warns, "n > 0 with a missing or zero total")))
  expect_true(any(str_detect(warns, "TRT = High Dose")))

  # The covered arm still renders a percent; the uncovered one is blank
  expect_match(out$res2[out$rowlabel1 == "X"], "33\\.3%")
  expect_match(out$res1[out$rowlabel1 == "X"], "\\(\\s+%\\)")
})

test_that("n > 0 with a zero denominator is blank, not 0.0% (#76)", {
  # Previously displayed 0.0% — an affirmatively wrong number.
  d <- data.frame(TRT = c("A", "A", "B"), VAL = c("X", "Y", "X"),
                  KEEP = "n", stringsAsFactors = FALSE)
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("VAL", settings = layer_settings(
      denom_where = quote(KEEP == "y")))))

  expect_warning(out <- tplyr_build(spec, d), "missing or zero total")
  expect_false(any(str_detect(out$res1, "0\\.0%")))
  expect_match(out$res1[out$rowlabel1 == "X"], "\\(\\s+%\\)")
})

test_that("a genuine zero count still renders 0.0% (#76)", {
  # The NA convention must not swallow legitimate zeros: grid completion gives
  # n = 0 against a valid denominator.
  d <- data.frame(TRT = c("A", "A", "B"), VAL = c("X", "Y", "X"),
                  stringsAsFactors = FALSE)
  out <- tplyr_build(
    tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL"))), d)
  expect_match(out$res2[out$rowlabel1 == "Y"], "0 \\(\\s*0\\.0%\\)")
  expect_silent(tplyr_build(
    tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL"))), d))
})

test_that("count, shift, and desc share the NA-percent convention (#76)", {
  # Count layers used to render 0 where desc rendered NA.
  d <- data.frame(TRT = c("A", "A", "B"), VAL = c("X", "Y", "X"),
                  AVAL = c(1, 2, 3), KEEP = "n",
                  B = factor(c("L", "N", "L")), A2 = factor(c("N", "L", "N")),
                  stringsAsFactors = FALSE)
  dw <- quote(KEEP == "y")

  cnt <- suppressWarnings(tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("VAL", settings = layer_settings(denom_where = dw)))), d))
  sft <- suppressWarnings(tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_shift(c(row = "B", column = "A2"),
                settings = layer_settings(denom_where = dw)))), d))
  dsc <- suppressWarnings(tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("AVAL", settings = layer_settings(
      denom_where = dw,
      format_strings = list("pct" = f_str("xx.x", "pct")))))), d))

  expect_false(any(str_detect(cnt$res1, "0\\.0%")))
  expect_false(any(str_detect(sft$res1, "0\\.0%")))
  expect_true(all(trimws(dsc$res1) == ""))
})

test_that("safe_pct is NA without a usable denominator (#76)", {
  expect_equal(safe_pct(c(5, 5, 5, 0), c(10, 0, NA, 10)),
               c(50, NA, NA, 0))
})

# --- risk_diff robustness (#76) ---

test_that("compute_risk_diff warns when a count exceeds its denominator (#76)", {
  counts <- data.table::data.table(
    TRT = c("A", "B"), VAL = c("X", "X"),
    n = c(20, 3), total = c(10, 30)
  )
  expect_warning(
    rd <- compute_risk_diff(counts, "TRT", "VAL", character(0),
                            list(comparisons = list(c("A", "B")))),
    "count exceeds denominator")
  expect_true(is.na(rd$rdiff[1]))
})

test_that("the plain difference survives a CI failure (#76)", {
  # p1 - p2 needs no test, so a prop.test failure must not blank it.
  counts <- data.table::data.table(
    TRT = c("A", "B"), VAL = c("X", "X"),
    n = c(5, 3), total = c(10, 30)
  )
  rd <- compute_risk_diff(counts, "TRT", "VAL", character(0),
                          list(comparisons = list(c("A", "B"))))
  expect_equal(rd$rdiff[1], (5 / 10 - 3 / 30) * 100)
  expect_false(is.na(rd$lower[1]))
})
