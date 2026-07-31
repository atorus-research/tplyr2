# --- Validation tests ---

# --- validate_spec ---

test_that("validate_spec rejects non-spec objects", {
  expect_error(
    tplyr2:::validate_spec(list(cols = "TRT")),
    "must be a tplyr_spec object"
  )
  expect_error(
    tplyr2:::validate_spec("not a spec"),
    "must be a tplyr_spec object"
  )
})

test_that("validate_spec rejects non-character cols", {
  bad_spec <- structure(
    list(cols = 123, layers = list()),
    class = "tplyr_spec"
  )
  expect_error(
    tplyr2:::validate_spec(bad_spec),
    "cols.*must be a character vector"
  )
})

test_that("validate_spec rejects non-layer in layers list", {
  bad_spec <- structure(
    list(cols = "TRT", layers = list("not a layer")),
    class = "tplyr_spec"
  )
  expect_error(
    tplyr2:::validate_spec(bad_spec),
    "Layer 1 is not a tplyr_layer"
  )
})

test_that("validate_spec rejects non-list layers", {
  bad_spec <- structure(
    list(cols = "TRT", layers = "wrong"),
    class = "tplyr_spec"
  )
  expect_error(
    tplyr2:::validate_spec(bad_spec),
    "layers.*must be a list"
  )
})

test_that("validate_spec accepts valid spec", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  expect_true(tplyr2:::validate_spec(spec))
})

test_that("validate_spec accepts spec with NULL cols", {
  spec <- structure(
    list(cols = NULL, layers = list()),
    class = "tplyr_spec"
  )
  expect_true(tplyr2:::validate_spec(spec))
})

# --- validate_layer ---

test_that("validate_layer rejects empty target_var", {
  bad_layer <- structure(
    list(target_var = character(0), by = NULL, where = NULL,
         settings = layer_settings(), layer_type = "count"),
    class = c("tplyr_count_layer", "tplyr_layer")
  )
  expect_error(
    tplyr2:::validate_layer(bad_layer, 1),
    "Layer 1: target_var must be a non-empty character"
  )
})

test_that("validate_layer rejects non-character target_var", {
  bad_layer <- structure(
    list(target_var = 42, by = NULL, where = NULL,
         settings = layer_settings(), layer_type = "count"),
    class = c("tplyr_count_layer", "tplyr_layer")
  )
  expect_error(
    tplyr2:::validate_layer(bad_layer, 2),
    "Layer 2: target_var must be a non-empty character"
  )
})

test_that("validate_layer rejects invalid format_strings", {
  bad_layer <- structure(
    list(target_var = "VAL", by = NULL, where = NULL,
         settings = layer_settings(format_strings = list(n_counts = "not an f_str")),
         layer_type = "count"),
    class = c("tplyr_count_layer", "tplyr_layer")
  )
  expect_error(
    tplyr2:::validate_layer(bad_layer, 1),
    "format_strings.*must be an f_str object"
  )
})

test_that("validate_layer checks shift target_var names", {
  bad_layer <- structure(
    list(target_var = c("A", "B"), by = NULL, where = NULL,
         settings = layer_settings(), layer_type = "shift"),
    class = c("tplyr_shift_layer", "tplyr_layer")
  )
  expect_error(
    tplyr2:::validate_layer(bad_layer, 1),
    "shift layer target_var must have names"
  )
})

test_that("validate_layer accepts valid count layer", {
  layer <- group_count("VAL")
  expect_true(tplyr2:::validate_layer(layer, 1))
})

test_that("validate_layer accepts valid shift layer", {
  layer <- group_shift(c(row = "BNRIND", column = "ANRIND"))
  expect_true(tplyr2:::validate_layer(layer, 1))
})

# --- validate_build_data ---

test_that("validate_build_data catches missing column variable", {
  data <- data.frame(TRT = "A", VAL = "X")
  spec <- tplyr_spec(
    cols = "MISSING_COL",
    layers = tplyr_layers(group_count("VAL"))
  )
  expect_error(
    tplyr_build(spec, data),
    "Column variable 'MISSING_COL' not found in data"
  )
})

test_that("validate_build_data catches missing target variable", {
  data <- data.frame(TRT = "A", VAL = "X")
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("MISSING_VAR"))
  )
  expect_error(
    tplyr_build(spec, data),
    "Layer 1: target variable 'MISSING_VAR' not found"
  )
})

test_that("validate_build_data treats unknown by strings as labels", {
  # classify_by treats strings not in column names as labels, not data vars
  # so "MISSING_BY" becomes a row label, not an error
  data <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL", by = "MISSING_BY"))
  )
  result <- tplyr_build(spec, data)
  # "MISSING_BY" should appear as a row label
  expect_true("rowlabel1" %in% names(result))
  expect_true(all(result$rowlabel1 == "MISSING_BY"))
})

test_that("validate_build_data passes for valid data", {
  data <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  # Should not error
  result <- tplyr_build(spec, data)
  expect_true(is.data.frame(result))
})

# --- validate_layer_stats ---

test_that("validate_layer_stats warns on unknown count stat", {
  layer <- group_count("VAL",
    settings = layer_settings(
      format_strings = list(
        unknown = f_str("xx", "bogus_stat")
      )
    )
  )
  expect_warning(
    tplyr2:::validate_layer_stats(layer, 1),
    "not a recognized statistic"
  )
})

test_that("validate_layer_stats allows known count stats", {
  layer <- group_count("VAL",
    settings = layer_settings(
      format_strings = list(
        n_counts = f_str("xx (xx.x%)", "n", "pct")
      )
    )
  )
  expect_silent(tplyr2:::validate_layer_stats(layer, 1))
})

test_that("validate_layer_stats allows custom summary names in desc", {
  layer <- group_desc("AGE",
    settings = layer_settings(
      format_strings = list(
        cv = f_str("xx.xx", "cv")
      ),
      custom_summaries = list(cv = quote(sd(.var) / mean(.var)))
    )
  )
  expect_silent(tplyr2:::validate_layer_stats(layer, 1))
})

test_that("tplyr_build gives informative error for non-spec", {
  expect_error(
    tplyr_build(list(cols = "TRT"), data.frame(TRT = "A")),
    "must be a tplyr_spec object"
  )
})

# Issue #14: pct threshold and zero-count display validation
test_that("validate_layer rejects invalid pct_lt / pct_gt / zero_count_display", {
  bad_lt <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("X", settings = layer_settings(pct_lt = -1))))
  expect_error(tplyr2:::validate_spec(bad_lt), "pct_lt")

  bad_gt <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("X", settings = layer_settings(pct_gt = 150))))
  expect_error(tplyr2:::validate_spec(bad_gt), "pct_gt")
})

test_that("layer_settings rejects an unknown zero_count_display value", {
  expect_error(layer_settings(zero_count_display = "nope"))
})

test_that("valid pct thresholds and zero_count_display pass validation", {
  ok <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("X", settings = layer_settings(
      pct_lt = 1, pct_gt = 99, zero_count_display = "count_only"))))
  expect_true(tplyr2:::validate_spec(ok))
})

# --- Single-proportion CI settings (#44) ---

test_that("layer_settings rejects an unknown ci_method", {
  expect_error(
    layer_settings(ci_method = "bootstrap"),
    "should be one of"
  )
})

test_that("validate_layer rejects an out-of-range ci_level", {
  bad_settings <- layer_settings()
  bad_settings$ci_level <- 1.5
  bad_layer <- structure(
    list(target_var = "VAL", by = NULL, where = NULL,
         settings = bad_settings, layer_type = "count"),
    class = c("tplyr_count_layer", "tplyr_layer")
  )
  expect_error(
    tplyr2:::validate_layer(bad_layer, 1),
    "ci_level must be a single number in \\(0, 1\\)"
  )
})

test_that("validate_layer rejects an unknown ci_method set post hoc", {
  bad_settings <- layer_settings()
  bad_settings$ci_method <- "not_a_method"
  bad_layer <- structure(
    list(target_var = "VAL", by = NULL, where = NULL,
         settings = bad_settings, layer_type = "count"),
    class = c("tplyr_count_layer", "tplyr_layer")
  )
  expect_error(
    tplyr2:::validate_layer(bad_layer, 3),
    "ci_method must be one of"
  )
})

test_that("validate_layer accepts a valid CI count layer", {
  layer <- group_count("VAL", settings = layer_settings(
    ci_method = "wilson", ci_level = 0.90))
  expect_true(tplyr2:::validate_layer(layer, 1))
})

# ---------------------------------------------------------------------------
# Wide result-column layouts cannot be mixed with standard layers
# ---------------------------------------------------------------------------

.shape_data <- function() {
  set.seed(5)
  d <- as.data.frame(tplyr_adsl)
  d$BN <- factor(sample(c("LOW", "NORM"), nrow(d), TRUE))
  d$AN <- factor(sample(c("LOW", "NORM"), nrow(d), TRUE))
  d
}

test_that("a shift layer alone and several shift layers together are allowed", {
  d <- .shape_data()
  expect_s3_class(tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_shift(c(row = "BN", column = "AN")))), d), "data.frame")
  expect_s3_class(tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_shift(c(row = "BN", column = "AN")),
    group_shift(c(row = "AN", column = "BN")))), d), "data.frame")
})

test_that("a shift layer mixed with a standard layer is rejected", {
  d <- .shape_data()
  expect_error(
    tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
      group_count("SEX"), group_shift(c(row = "BN", column = "AN")))), d),
    "wide result-column layout"
  )
})

test_that("a stats_as_columns layer mixed with a standard layer is rejected", {
  d <- .shape_data()
  expect_error(
    tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
      group_count("SEX"),
      group_desc("AGE", by = "AGEGR1", settings = layer_settings(
        stats_as_columns = TRUE)))), d),
    "wide result-column layout"
  )
})

test_that("two different wide layouts cannot be combined either", {
  d <- .shape_data()
  expect_error(
    tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
      group_shift(c(row = "BN", column = "AN")),
      group_desc("AGE", by = "AGEGR1", settings = layer_settings(
        stats_as_columns = TRUE)))), d),
    "wide result-column layout"
  )
})

test_that("standard layers of different types still combine freely", {
  d <- .shape_data()
  expect_s3_class(tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("SEX"), group_desc("AGE"),
    group_count(c("RACE", "SEX")))), d), "data.frame")
})
