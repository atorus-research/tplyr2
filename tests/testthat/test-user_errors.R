# --- User-code failures report their reason (#75) ---
#
# The contract stays: user code cannot abort the build, and NA renders blank.
# What is asserted here is that the discarded condition message now reaches the
# user, deduplicated, as one warning per build.

test_that("a custom summary typo names the summary and the cause (#75)", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(
      format_strings = list("GeoMean" = f_str("xx.x", "gm")),
      custom_summaries = list(gm = quote(meann(.var)))))))

  expect_warning(out <- tplyr_build(spec, tplyr_adsl),
                 "custom summary 'gm'")
  expect_warning(tplyr_build(spec, tplyr_adsl), "could not find function")
  # Cell rendering is unchanged
  expect_equal(nrow(out), 1)
  expect_true(all(trimws(out$res1) == ""))
})

test_that("a partial custom-summary failure names the group (#75)", {
  # The dangerous case: real numbers everywhere, one blank cell.
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_desc("AGE", by = "SEX", settings = layer_settings(
      format_strings = list("Odd" = f_str("xx.x", "odd")),
      custom_summaries = list(
        odd = quote(if (.var[1] > 50) stop("model did not converge") else mean(.var)))))))

  expect_warning(tplyr_build(spec, tplyr_adsl), "model did not converge")
  expect_warning(tplyr_build(spec, tplyr_adsl), "TRT01P = |SEX = ")
})

test_that("repeated identical failures collapse into one warning (#75)", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_desc("AGE", by = "SEX", settings = layer_settings(
      format_strings = list("Bad" = f_str("xx.x", "bad")),
      custom_summaries = list(bad = quote(stop("always fails")))))))

  warns <- testthat::capture_warnings(tplyr_build(spec, tplyr_adsl))
  # Six groups fail with the same message; one warning, with a count
  expect_length(warns, 1)
  expect_match(warns, "always fails")
  expect_match(warns, "more group")
})

test_that("an assoc_test fn error is reported (#75)", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("AGEGR1", settings = layer_settings(
      assoc_test = assoc_test(fn = function(d) stop("package not installed"),
                              format = f_str("x.xxx", "pval"))))))

  expect_warning(out <- tplyr_build(spec, tplyr_adsl),
                 "assoc_test fn.*package not installed")
  expect_true(all(trimws(out$pval1) == ""))
})

test_that("an assoc_test return-shape mismatch is reported (#75)", {
  # A caller bug, not a statistical outcome — it used to blank silently.
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("AGEGR1", settings = layer_settings(
      assoc_test = assoc_test(fn = function(d) c(1.2, 3.4),
                              format = f_str("x.xxx", "pval"))))))

  expect_warning(tplyr_build(spec, tplyr_adsl),
                 "returned 2 values but the format declares 1")
})

test_that("a pairwise assoc_test failure is reported once (#75)", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("AGEGR1", settings = layer_settings(
      assoc_test = assoc_test(fn = function(m) stop("singular matrix"),
                              format = f_str("x.xxx", "pval"),
                              reference = "Placebo",
                              comparisons = c("Xanomeline High Dose"))))))

  # fn runs once per level per comparison; the user sees one warning.
  warns <- testthat::capture_warnings(tplyr_build(spec, tplyr_adsl))
  expect_length(warns, 1)
  expect_match(warns, "pairwise assoc_test fn.*singular matrix")
})

test_that("a clean build emits no user-code warning (#75)", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_desc("AGE")))
  expect_silent(tplyr_build(spec, tplyr_adsl))
})

test_that("record_user_fn_error is inert outside a build (#75)", {
  expect_silent(record_user_fn_error("x", simpleError("y")))
  expect_false(user_fn_errors$active)
})
