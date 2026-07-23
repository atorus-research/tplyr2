# Tests for stat_columns: one result column per statistic per column group
# (issue #10 — multi-column AE layouts)

stat_cols_ae <- function() {
  list(
    "n (%)" = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct"),
    "E"     = f_str("xxx", "n")
  )
}

# --- Structure and labels ---

test_that("stat_columns produces one res column per stat per column group", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae()
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  n_arms <- length(unique(tplyr_adae$TRTA))
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_length(res_cols, n_arms * 2)
})

test_that("stat_columns labels follow '<group> (N=n) | <stat>' arm-major", {
  data(tplyr_adae, package = "tplyr2")
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    pop_data = pop_data(cols = c("TRTA" = "TRT01A")),
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae()
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adae, pop_data = tplyr_adsl)

  expect_match(attr(result$res1, "label"), "^Placebo \\(N=\\d+\\) \\| n \\(%\\)$")
  expect_match(attr(result$res2, "label"), "^Placebo \\(N=\\d+\\) \\| E$")
  expect_match(attr(result$res3, "label"), "^Xanomeline High Dose \\(N=\\d+\\) \\| n \\(%\\)$")
  expect_match(attr(result$res4, "label"), "^Xanomeline High Dose \\(N=\\d+\\) \\| E$")

  # The same N is attached to both sub-columns of an arm
  n1 <- sub(".*\\(N=(\\d+)\\).*", "\\1", attr(result$res1, "label"))
  n2 <- sub(".*\\(N=(\\d+)\\).*", "\\1", attr(result$res2, "label"))
  expect_equal(n1, n2)
})

test_that("stat_columns values match an equivalent packed single-cell build", {
  data(tplyr_adae, package = "tplyr2")
  base_settings <- function(...) {
    layer_settings(distinct_by = "USUBJID", ...)
  }
  spec_split <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD", settings = base_settings(stat_columns = stat_cols_ae()))
    )
  )
  spec_packed <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD", settings = base_settings(
        format_strings = list(
          n_counts = f_str("xxx (xx.x%) [xxx]", "distinct_n", "distinct_pct", "n")
        )
      ))
    )
  )
  split <- tplyr_build(spec_split, tplyr_adae)
  packed <- tplyr_build(spec_packed, tplyr_adae)

  expect_equal(split$rowlabel1, packed$rowlabel1)
  expect_equal(paste0(split$res1, " [", split$res2, "]"), packed$res1,
               ignore_attr = TRUE)
  expect_equal(paste0(split$res3, " [", split$res4, "]"), packed$res2,
               ignore_attr = TRUE)
  expect_equal(paste0(split$res5, " [", split$res6, "]"), packed$res3,
               ignore_attr = TRUE)
})

test_that("without stat_columns output is unchanged (legacy parity)", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(
          distinct_by = "USUBJID",
          format_strings = list(
            n_counts = f_str("xxx (xx.x%)", "distinct_n", "distinct_pct")
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_length(res_cols, length(unique(tplyr_adae$TRTA)))
  expect_false(grepl(" \\| ", attr(result$res1, "label")))
})

# --- Nested counts ---

test_that("stat_columns works with nested count layers", {
  data(tplyr_adae, package = "tplyr2")
  spec_split <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count(c("AEBODSYS", "AEDECOD"),
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae(),
          total_row = TRUE
        )
      )
    )
  )
  spec_packed <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count(c("AEBODSYS", "AEDECOD"),
        settings = layer_settings(
          distinct_by = "USUBJID",
          format_strings = list(
            n_counts = f_str("xxx (xx.x%) [xxx]", "distinct_n", "distinct_pct", "n")
          ),
          total_row = TRUE
        )
      )
    )
  )
  split <- tplyr_build(spec_split, tplyr_adae)
  packed <- tplyr_build(spec_packed, tplyr_adae)

  # Same rows, same interleaving/ordering, decomposed values
  expect_equal(split$rowlabel1, packed$rowlabel1)
  expect_equal(split$rowlabel2, packed$rowlabel2)
  expect_equal(split$ord_layer_1, packed$ord_layer_1)
  expect_equal(split$ord_layer_2, packed$ord_layer_2)
  expect_equal(paste0(split$res1, " [", split$res2, "]"), packed$res1,
               ignore_attr = TRUE)

  # Total row present and populated in every stat sub-column
  total_idx <- which(split$rowlabel1 == "Total")
  expect_length(total_idx, 1)
  expect_true(nzchar(split$res1[total_idx]))
  expect_true(nzchar(split$res2[total_idx]))
})

# --- Special rows ---

test_that("total and missing rows populate every stat sub-column", {
  data(tplyr_adsl, package = "tplyr2")
  adsl_miss <- tplyr_adsl
  adsl_miss$SEX[1:5] <- NA

  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count("SEX",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae(),
          total_row = TRUE,
          missing_count = list(label = "Missing")
        )
      )
    )
  )
  result <- tplyr_build(spec, adsl_miss)

  for (lbl in c("Total", "Missing")) {
    idx <- which(result$rowlabel1 == lbl)
    expect_length(idx, 1)
    res_cols <- grep("^res\\d+$", names(result), value = TRUE)
    for (rc in res_cols) {
      expect_true(nzchar(result[[rc]][idx]),
                  label = paste0(lbl, " row has empty ", rc))
    }
  }
})

# --- No column variables ---

test_that("stat_columns with no cols keeps row order and labels stats", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = character(0),
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae(),
          total_row = TRUE
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  expect_equal(grep("^res\\d+$", names(result), value = TRUE), c("res1", "res2"))
  expect_equal(attr(result$res1, "label"), "n (%)")
  expect_equal(attr(result$res2, "label"), "E")
  # Total row must remain last (no dcast row re-sort)
  expect_equal(result$rowlabel1[nrow(result)], "Total")
})

# --- Multiple column variables ---

test_that("stat_columns works with multiple cols", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = c("TRT01P", "SEX"),
    layers = tplyr_layers(
      group_count("RACE",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae()
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  n_combos <- nrow(unique(tplyr_adsl[, c("TRT01P", "SEX")]))
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_length(res_cols, n_combos * 2)

  # Labels: "<col1> | <col2> (N=n) | <stat>" with adjacent stat pairs
  lbl1 <- attr(result$res1, "label")
  lbl2 <- attr(result$res2, "label")
  expect_match(lbl1, "\\| n \\(%\\)$")
  expect_match(lbl2, "\\| E$")
  expect_equal(sub(" \\| n \\(%\\)$", "", lbl1), sub(" \\| E$", "", lbl2))
})

# --- by variables ---

test_that("stat_columns works with by variables", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AESEV", by = "AEBODSYS",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae()
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  expect_true(all(c("rowlabel1", "rowlabel2") %in% names(result)))
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_length(res_cols, length(unique(tplyr_adae$TRTA)) * 2)
})

# --- Risk difference coexistence ---

test_that("stat_columns coexists with risk_diff", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae(),
          risk_diff = list(
            comparisons = list(c("Xanomeline High Dose", "Placebo"))
          )
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  expect_true("rdiff1" %in% names(result))
  expect_equal(attr(result$rdiff1, "label"), "Xanomeline High Dose vs Placebo")
  # rdiff columns come after all res columns
  expect_gt(match("rdiff1", names(result)),
            max(match(grep("^res\\d+$", names(result), value = TRUE), names(result))))
})

# --- More than 9 res columns (numeric-suffix ordering) ---

test_that("res columns stay in build order past res9", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = c("TRT01P", "SEX"),
    layers = tplyr_layers(
      group_count("RACE",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae()
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_gt(length(res_cols), 9)
  expect_equal(res_cols, paste0("res", seq_along(res_cols)))
})

# --- Stacked layers ---

test_that("multiple count layers with matching stat_columns stack", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AESEV",
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae())),
      group_count("AEDECOD",
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae()))
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  expect_equal(sort(unique(result$ord_layer_index)), c(1, 2))
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_length(res_cols, length(unique(tplyr_adae$TRTA)) * 2)
})

# --- Validation ---

test_that("mixing stat_columns and non-stat_columns layers errors", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AESEV",
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae())),
      group_count("AEDECOD",
        settings = layer_settings(distinct_by = "USUBJID"))
    )
  )
  expect_error(tplyr_build(spec, tplyr_adae), "stat_columns")
})

test_that("layers with different stat_columns names error", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AESEV",
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae())),
      group_count("AEDECOD",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = list("subjects" = f_str("xxx", "distinct_n"),
                              "events" = f_str("xxx", "n"))))
    )
  )
  expect_error(tplyr_build(spec, tplyr_adae), "same statistic names")
})

test_that("stat_columns validation rejects bad configurations", {
  data(tplyr_adae, package = "tplyr2")
  build_with <- function(sc) {
    spec <- tplyr_spec(
      cols = "TRTA",
      layers = tplyr_layers(
        group_count("AESEV", settings = layer_settings(stat_columns = sc))
      )
    )
    tplyr_build(spec, tplyr_adae)
  }

  expect_error(build_with(list(f_str("xx", "n"))), "must be named")
  expect_error(build_with(list("a" = f_str("xx", "n"), "a" = f_str("xx", "pct"))),
               "unique")
  expect_error(build_with(list("a | b" = f_str("xx", "n"))), "reserved")
  expect_error(build_with(list("a (N=1)" = f_str("xx", "n"))), "reserved")
  expect_error(build_with(list("a" = "not an f_str")), "f_str object")
})

test_that("both stat_columns and format_strings warns, stat_columns wins", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AESEV",
        settings = layer_settings(
          distinct_by = "USUBJID",
          stat_columns = stat_cols_ae(),
          format_strings = list(n_counts = f_str("xx", "n"))
        )
      )
    )
  )
  expect_warning(result <- tplyr_build(spec, tplyr_adae), "takes precedence")
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_length(res_cols, length(unique(tplyr_adae$TRTA)) * 2)
})

test_that("stat_columns on a desc layer is silently ignored", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          stat_columns = list("x" = f_str("xx.x", "mean"))
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_length(res_cols, length(unique(tplyr_adsl$TRT01P)))
})

# --- Metadata ---

test_that("stat_columns metadata shares filters across stat sub-columns", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae()))
    )
  )
  result <- tplyr_build(spec, tplyr_adae, metadata = TRUE)
  meta <- attr(result, "tplyr_meta")

  rid <- result$row_id[1]
  m1 <- meta[[paste0(rid, "||res1")]]
  m2 <- meta[[paste0(rid, "||res2")]]
  m3 <- meta[[paste0(rid, "||res3")]]

  # Sub-columns of the same arm: identical filters, different statistic
  expect_equal(lapply(m1$filters, deparse1), lapply(m2$filters, deparse1))
  expect_equal(m1$statistic, "n (%)")
  expect_equal(m2$statistic, "E")

  # Next arm resolves to a different column filter
  expect_true(any(grepl("Xanomeline High Dose",
                        vapply(m3$filters, deparse1, character(1)))))
})

test_that("stat_columns metadata subsets reproduce cell values", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae()))
    )
  )
  result <- tplyr_build(spec, tplyr_adae, metadata = TRUE)

  # Pick a row with events in the Placebo arm
  idx <- which(as.integer(trimws(result$res2)) > 0)[1]
  rid <- result$row_id[idx]

  sub_distinct <- tplyr_meta_subset(result, rid, "res1", tplyr_adae)
  sub_events <- tplyr_meta_subset(result, rid, "res2", tplyr_adae)

  n_distinct_cell <- as.integer(sub(" *\\(.*$", "", trimws(result$res1[idx])))
  n_events_cell <- as.integer(trimws(result$res2[idx]))

  expect_equal(length(unique(sub_distinct$USUBJID)), n_distinct_cell)
  expect_equal(nrow(sub_events), n_events_cell)
})

# --- Serialization ---

test_that("stat_columns round-trips through JSON and YAML", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae()))
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  for (ext in c(".json", ".yaml")) {
    path <- tempfile(fileext = ext)
    tplyr_write_spec(spec, path)
    spec_back <- tplyr_read_spec(path)

    sc <- spec_back$layers[[1]]$settings$stat_columns
    expect_equal(names(sc), c("n (%)", "E"))
    expect_s3_class(sc[[1]], "tplyr_f_str")

    expect_identical(tplyr_build(spec_back, tplyr_adae), result)
    unlink(path)
  }
})

# --- ARD ---

test_that("stat_columns round-trips through ARD", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count("AEDECOD",
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae()))
    )
  )
  result <- tplyr_build(spec, tplyr_adae)

  ard <- tplyr_to_ard(result)
  expect_false(any(grepl("^formatted", names(ard))))

  back <- tplyr_from_ard(ard, spec)
  res_cols <- grep("^res\\d+$", names(back), value = TRUE)
  expect_length(res_cols, length(unique(tplyr_adae$TRTA)) * 2)
  expect_match(attr(back$res1, "label"), " \\| n \\(%\\)$")
  expect_setequal(back$res1, result$res1)
  expect_setequal(back$res2, result$res2)
})

test_that("nested stat_columns numeric data excludes formatted columns in ARD", {
  data(tplyr_adae, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRTA",
    layers = tplyr_layers(
      group_count(c("AEBODSYS", "AEDECOD"),
        settings = layer_settings(distinct_by = "USUBJID",
                                  stat_columns = stat_cols_ae()))
    )
  )
  result <- tplyr_build(spec, tplyr_adae)
  ard <- tplyr_to_ard(result)

  expect_false(any(grepl("^formatted", names(ard))))
  expect_true(all(c("n", "distinct_n", "pct", "distinct_pct") %in%
                    unique(ard$stat_name)))
})