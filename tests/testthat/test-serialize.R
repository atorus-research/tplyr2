# --- Serialization tests ---

scratch_dir <- file.path(tempdir(), "tplyr2_serialize_tests")
dir.create(scratch_dir, showWarnings = FALSE, recursive = TRUE)

# --- JSON roundtrip tests ---

test_that("JSON roundtrip for simple count spec", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  path <- file.path(scratch_dir, "count.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_true(inherits(spec2, "tplyr_spec"))
  expect_equal(spec2$cols, "TRT")
  expect_equal(length(spec2$layers), 1)
  expect_true(inherits(spec2$layers[[1]], "tplyr_count_layer"))
  expect_equal(spec2$layers[[1]]$target_var, "VAL")
})

test_that("JSON roundtrip for desc spec with format_strings", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list(
            "Mean (SD)" = f_str("xx.x (xx.xx)", "mean", "sd"),
            "n" = f_str("xx", "n")
          )
        )
      )
    )
  )
  path <- file.path(scratch_dir, "desc.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_true(inherits(spec2$layers[[1]], "tplyr_desc_layer"))
  expect_equal(spec2$layers[[1]]$target_var, "AGE")
  fs <- spec2$layers[[1]]$settings$format_strings
  expect_equal(length(fs), 2)
  expect_true(inherits(fs[["Mean (SD)"]], "tplyr_f_str"))
  expect_equal(fs[["Mean (SD)"]]$format_string, "xx.x (xx.xx)")
  expect_equal(fs[["Mean (SD)"]]$vars, c("mean", "sd"))
})

test_that("JSON roundtrip for shift spec", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"))
    )
  )
  path <- file.path(scratch_dir, "shift.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_true(inherits(spec2$layers[[1]], "tplyr_shift_layer"))
  tv <- spec2$layers[[1]]$target_var
  expect_equal(length(tv), 2)
  expect_equal(tv[["row"]], "BNRIND")
  expect_equal(tv[["column"]], "ANRIND")
})

test_that("JSON roundtrip with where expression", {
  spec <- tplyr_spec(
    cols = "TRT",
    where = SAFFL == "Y",
    layers = tplyr_layers(group_count("VAL"))
  )
  path <- file.path(scratch_dir, "where.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_false(is.null(spec2$where))
  expect_equal(deparse(spec2$where), deparse(quote(SAFFL == "Y")))
})

test_that("JSON roundtrip with layer where expression", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL", where = PARAM == "Height")
    )
  )
  path <- file.path(scratch_dir, "layer_where.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_false(is.null(spec2$layers[[1]]$where))
  expect_equal(deparse(spec2$layers[[1]]$where), deparse(quote(PARAM == "Height")))
})

test_that("JSON roundtrip with custom_summaries expressions", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list(cv = f_str("xx.xx", "cv")),
          custom_summaries = list(
            cv = rlang::expr(sd(.var) / mean(.var) * 100)
          )
        )
      )
    )
  )
  path <- file.path(scratch_dir, "custom_sum.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  cs <- spec2$layers[[1]]$settings$custom_summaries
  expect_true(!is.null(cs))
  expect_true("cv" %in% names(cs))
  expect_true(is.language(cs$cv))
})

test_that("JSON roundtrip with f_str empty values", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          format_strings = list(
            n_counts = f_str("xx (xx.x%)", "n", "pct",
                             empty = c(.overall = "---"))
          )
        )
      )
    )
  )
  path <- file.path(scratch_dir, "empty.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  fs <- spec2$layers[[1]]$settings$format_strings$n_counts
  expect_equal(fs$empty, c(.overall = "---"))
})

test_that("JSON roundtrip with pop_data config", {
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(group_count("VAL"))
  )
  path <- file.path(scratch_dir, "pop.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_true(!is.null(spec2$pop_data))
  expect_true(inherits(spec2$pop_data, "tplyr_pop_data"))
  expect_equal(spec2$pop_data$cols, "TRT")
})

test_that("JSON roundtrip with total_groups", {
  spec <- tplyr_spec(
    cols = "TRT",
    total_groups = list(total_group("TRT", label = "Total")),
    layers = tplyr_layers(group_count("VAL"))
  )
  path <- file.path(scratch_dir, "totals.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_equal(length(spec2$total_groups), 1)
  expect_true(inherits(spec2$total_groups[[1]], "tplyr_total_group"))
  expect_equal(spec2$total_groups[[1]]$label, "Total")
})

test_that("JSON roundtrip with custom_groups", {
  spec <- tplyr_spec(
    cols = "TRT",
    custom_groups = list(
      custom_group("TRT", "Active" = c("High", "Low"))
    ),
    layers = tplyr_layers(group_count("VAL"))
  )
  path <- file.path(scratch_dir, "custom_grps.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_equal(length(spec2$custom_groups), 1)
  expect_true(inherits(spec2$custom_groups[[1]], "tplyr_custom_group"))
  expect_equal(spec2$custom_groups[[1]]$groups$Active, c("High", "Low"))
})

test_that("JSON roundtrip with risk_diff config", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          risk_diff = list(
            comparisons = list(c("A", "B")),
            ci = 0.95,
            format = f_str("xx.x (xx.x, xx.x)", "rdiff", "lower", "upper")
          )
        )
      )
    )
  )
  path <- file.path(scratch_dir, "riskdiff.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  rd <- spec2$layers[[1]]$settings$risk_diff
  expect_true(!is.null(rd))
  expect_equal(rd$ci, 0.95)
  expect_true(inherits(rd$format, "tplyr_f_str"))
  expect_equal(rd$format$vars, c("rdiff", "lower", "upper"))
})

test_that("JSON roundtrip with label() in by parameter", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL", by = label("Demographics"))
    )
  )
  path <- file.path(scratch_dir, "label_by.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_true(is_label(spec2$layers[[1]]$by))
  expect_equal(as.character(spec2$layers[[1]]$by), "Demographics")
})

test_that("JSON roundtrip with by data variable", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL", by = "GRP")
    )
  )
  path <- file.path(scratch_dir, "by_var.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_equal(spec2$layers[[1]]$by, "GRP")
})

# --- Build from file ---

test_that("tplyr_build accepts file path as spec", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
        )
      )
    )
  )
  path <- file.path(scratch_dir, "build_from_file.json")
  tplyr_write_spec(spec, path)

  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    VAL = c(rep("X", 3), rep("Y", 2), rep("X", 2), rep("Y", 3))
  )

  result_from_spec <- tplyr_build(spec, data)
  result_from_file <- tplyr_build(path, data)

  expect_equal(result_from_spec$rowlabel1, result_from_file$rowlabel1)
  expect_equal(result_from_spec$res1, result_from_file$res1)
  expect_equal(result_from_spec$res2, result_from_file$res2)
})

# --- Build roundtrip produces identical output ---

test_that("build from JSON produces identical output to build from spec", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    VAL = c(rep("X", 7), rep("Y", 3), rep("X", 4), rep("Y", 6))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct")),
          total_row = TRUE
        )
      )
    )
  )

  path <- file.path(scratch_dir, "identical_output.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  result1 <- tplyr_build(spec, data)
  result2 <- tplyr_build(spec2, data)

  # Results should be identical (excluding attributes that may differ)
  expect_equal(result1$rowlabel1, result2$rowlabel1)
  expect_equal(result1$res1, result2$res1)
  expect_equal(result1$res2, result2$res2)
})

# --- Error handling ---

test_that("tplyr_write_spec errors on non-spec", {
  expect_error(
    tplyr_write_spec(list(a = 1), file.path(scratch_dir, "bad.json")),
    "must be a tplyr_spec object"
  )
})

test_that("tplyr_read_spec errors on missing file", {
  expect_error(
    tplyr_read_spec(file.path(scratch_dir, "nonexistent.json")),
    "File not found"
  )
})

test_that("tplyr_write_spec errors on bad extension", {
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("VAL")))
  expect_error(
    tplyr_write_spec(spec, file.path(scratch_dir, "bad.txt")),
    "Unsupported file extension"
  )
})

# --- NULL and edge case handling ---

test_that("serialization handles NULL fields correctly", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  path <- file.path(scratch_dir, "nulls.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_null(spec2$where)
  expect_null(spec2$pop_data)
  expect_null(spec2$total_groups)
  expect_null(spec2$custom_groups)
})

test_that("serialization handles empty layers list", {
  spec <- tplyr_spec(cols = "TRT", layers = list())
  path <- file.path(scratch_dir, "empty_layers.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_equal(length(spec2$layers), 0)
})

# --- YAML roundtrip ---

test_that("YAML roundtrip works for simple spec", {
  skip_if_not_installed("yaml")

  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))
        )
      )
    )
  )
  path <- file.path(scratch_dir, "simple.yaml")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_true(inherits(spec2, "tplyr_spec"))
  expect_equal(spec2$cols, "TRT")
  expect_true(inherits(spec2$layers[[1]]$settings$format_strings$n_counts, "tplyr_f_str"))
})

# --- Shift target_var names preservation ---

test_that("shift target_var names preserved through JSON roundtrip", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"))
    )
  )
  path <- file.path(scratch_dir, "shift_names.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  tv <- spec2$layers[[1]]$target_var
  expect_true(!is.null(names(tv)))
  expect_equal(names(tv), c("row", "column"))
})

# --- Multi-layer spec roundtrip ---

test_that("multi-layer spec roundtrip works", {
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          format_strings = list(n_counts = f_str("xx", "n"))
        )
      ),
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("Mean" = f_str("xx.x", "mean"))
        )
      )
    )
  )
  path <- file.path(scratch_dir, "multi.json")
  tplyr_write_spec(spec, path)
  spec2 <- tplyr_read_spec(path)

  expect_equal(length(spec2$layers), 2)
  expect_true(inherits(spec2$layers[[1]], "tplyr_count_layer"))
  expect_true(inherits(spec2$layers[[2]], "tplyr_desc_layer"))
})

# Cleanup
withr::defer(unlink(scratch_dir, recursive = TRUE), teardown_env())
