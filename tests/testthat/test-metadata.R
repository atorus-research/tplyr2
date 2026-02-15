# --- Metadata & traceability tests ---

test_that("metadata = TRUE adds row_id column", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    SEX = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  expect_true("row_id" %in% names(result))
  expect_equal(length(result$row_id), nrow(result))
})

test_that("metadata = FALSE does not add row_id", {
  data <- data.frame(TRT = c("A", "B"), SEX = c("M", "F"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = FALSE)
  expect_false("row_id" %in% names(result))
})

test_that("tplyr_meta attribute is attached when metadata = TRUE", {
  data <- data.frame(TRT = c("A", "B"), SEX = c("M", "F"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  expect_false(is.null(attr(result, "tplyr_meta")))
})

test_that("tplyr_meta_result returns tplyr_meta object with filters for count cell", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    SEX = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  # First row
  rid <- result$row_id[1]
  meta <- tplyr_meta_result(result, rid, "res1")
  expect_s3_class(meta, "tplyr_meta")
  expect_true(is.list(meta$filters))
  expect_true(length(meta$filters) > 0)
  expect_true(is.character(meta$names))
  expect_true(length(meta$names) > 0)
})

test_that("tplyr_meta_result errors without metadata", {
  data <- data.frame(TRT = c("A", "B"), SEX = c("M", "F"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data)
  expect_error(tplyr_meta_result(result, "fake_id", "res1"),
               "metadata = TRUE")
})

test_that("tplyr_meta_result returns NULL for nonexistent cell", {
  data <- data.frame(TRT = c("A", "B"), SEX = c("M", "F"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  meta <- tplyr_meta_result(result, "nonexistent_row", "res1")
  expect_null(meta)
})

test_that("tplyr_meta_subset returns correct source rows for count layer", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = c(rep("M", 6), rep("F", 4), rep("M", 3), rep("F", 7))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)

  # Find the "F" row
  f_row <- result[result$rowlabel1 == "F", ]
  rid <- f_row$row_id

  # Get subset for TRT=A (res1, which should be the first column level)
  subset_a <- tplyr_meta_subset(result, rid, "res1", data)
  expect_true(is.data.frame(subset_a))

  if (nrow(subset_a) > 0) {
    # All returned rows should have SEX == "F"
    expect_true(all(subset_a$SEX == "F"))
    # All should be from TRT column level matching res1
    expect_true(length(unique(subset_a$TRT)) == 1)
  }
})

test_that("row count in metadata matches cell count", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = c(rep("M", 6), rep("F", 4), rep("M", 3), rep("F", 7))
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)

  # Get numeric data to verify
  nd <- tplyr_numeric_data(result, layer = 1)

  for (i in seq_len(nrow(result))) {
    rid <- result$row_id[i]
    # Evaluate the filter and check count
    subset_data <- tplyr_meta_subset(result, rid, "res1", data)
    if (!is.null(subset_data)) {
      sex_val <- result$rowlabel1[i]
      res1_label <- attr(result$res1, "label")
      col_level <- sub("\\s*\\(N=\\d+\\)$", "", res1_label)

      matching_nd <- nd[nd$SEX == sex_val & nd$TRT == col_level, ]
      if (nrow(matching_nd) > 0) {
        expect_equal(nrow(subset_data), matching_nd$n[1])
      }
    }
  }
})

test_that("metadata works with desc layer", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    AGE = c(25:29, 35:39)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  expect_true("row_id" %in% names(result))

  rid <- result$row_id[1]
  meta <- tplyr_meta_result(result, rid, "res1")
  expect_s3_class(meta, "tplyr_meta")
  expect_true(length(meta$filters) > 0)
  # Desc layer: target_var should be in names
  expect_true("AGE" %in% meta$names)
  # Functional check: subsetting should return rows
  subset_data <- tplyr_meta_subset(result, rid, "res1", data)
  expect_true(nrow(subset_data) > 0)
})

test_that("metadata works with by variables", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    GRP = rep(c("G1", "G2"), times = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX", by = "GRP"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  expect_true("row_id" %in% names(result))

  # Check a specific cell
  g1_m_row <- result[result$rowlabel1 == "G1" & result$rowlabel2 == "M", ]
  if (nrow(g1_m_row) > 0) {
    rid <- g1_m_row$row_id[1]
    subset <- tplyr_meta_subset(result, rid, "res1", data)
    if (!is.null(subset) && nrow(subset) > 0) {
      expect_true(all(subset$GRP == "G1"))
      expect_true(all(subset$SEX == "M"))
    }
  }
})

test_that("metadata works with where filter", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    FLAG = rep(c("Y", "N"), 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    where = FLAG == "Y",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  expect_true("row_id" %in% names(result))

  rid <- result$row_id[1]
  subset <- tplyr_meta_subset(result, rid, "res1", data)
  if (!is.null(subset) && nrow(subset) > 0) {
    # Source rows should have FLAG == "Y"
    expect_true(all(subset$FLAG == "Y"))
  }

  # Verify the where filter appears in metadata
  meta <- tplyr_meta_result(result, rid, "res1")
  expect_true("FLAG" %in% meta$names)
})

test_that("metadata works with analyze layer", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    VAL = 1:10
  )
  fn <- function(.data, .target_var) {
    data.frame(row_label = "n", formatted = as.character(nrow(.data)))
  }
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_analyze("VAL", analyze_fn = fn))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  expect_true("row_id" %in% names(result))

  rid <- result$row_id[1]
  meta <- tplyr_meta_result(result, rid, "res1")
  expect_s3_class(meta, "tplyr_meta")
})

test_that("row_id values are unique", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10),
    AGE = c(25:34, 35:44)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("SEX"),
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  expect_equal(length(result$row_id), length(unique(result$row_id)))
})

test_that("row_id is stable across identical builds", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    SEX = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result1 <- tplyr_build(spec, data, metadata = TRUE)
  result2 <- tplyr_build(spec, data, metadata = TRUE)
  expect_equal(result1$row_id, result2$row_id)
})

test_that("generate_row_ids works standalone", {
  result <- data.frame(
    rowlabel1 = c("M", "F"),
    res1 = c("5", "3"),
    ord_layer_index = c(1, 1)
  )
  ids <- generate_row_ids(result)
  expect_equal(length(ids), 2)
  expect_true(all(grepl("^1_", ids)))
})

test_that("tplyr_meta_subset returns NULL for nonexistent cell", {
  data <- data.frame(TRT = c("A", "B"), SEX = c("M", "F"))
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  subset <- tplyr_meta_subset(result, "nonexistent_id", "res1", data)
  expect_null(subset)
})

test_that("metadata for Total row has proper filters", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    SEX = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("SEX",
        settings = layer_settings(total_row = TRUE)
      )
    )
  )
  result <- tplyr_build(spec, data, metadata = TRUE)

  total_row <- result[result$rowlabel1 == "Total", ]
  if (nrow(total_row) > 0) {
    rid <- total_row$row_id[1]
    meta <- tplyr_meta_result(result, rid, "res1")
    expect_s3_class(meta, "tplyr_meta")
    # Total row should have filters (at least the column variable filter)
    expect_true(length(meta$filters) > 0)
    # Subset should return all rows for that treatment group
    subset_data <- tplyr_meta_subset(result, rid, "res1", data)
    expect_equal(nrow(subset_data), 5)
  }
})

test_that("metadata with shift layer", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 6),
    BNRIND = rep(c("L", "N", "H"), 4),
    ANRIND = rep(c("L", "N", "H"), 4)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"))
    )
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  expect_true("row_id" %in% names(result))

  rid <- result$row_id[1]
  meta <- tplyr_meta_result(result, rid, "res1")
  expect_s3_class(meta, "tplyr_meta")
  # Should reference both shift variables
  expect_true("BNRIND" %in% meta$names)
  expect_true("ANRIND" %in% meta$names)
})

test_that("metadata multi-layer integration", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10),
    AGE = c(25:34, 35:44)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("SEX"),
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data, metadata = TRUE)

  # Check count layer cell
  count_row <- result[result$ord_layer_index == 1, ][1, ]
  meta1 <- tplyr_meta_result(result, count_row$row_id, "res1")
  expect_s3_class(meta1, "tplyr_meta")
  expect_equal(meta1$layer_index, 1L)

  # Check desc layer cell
  desc_row <- result[result$ord_layer_index == 2, ][1, ]
  meta2 <- tplyr_meta_result(result, desc_row$row_id, "res1")
  expect_s3_class(meta2, "tplyr_meta")
  expect_equal(meta2$layer_index, 2L)
})

# --- New filter-expression-specific tests ---

test_that("filter expressions reference correct variables", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    SEX = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  f_row <- result[result$rowlabel1 == "F", ]
  meta <- tplyr_meta_result(result, f_row$row_id, "res1")
  expect_true("TRT" %in% meta$names)
  expect_true("SEX" %in% meta$names)
  # Verify filter expressions contain these variable references
  filter_vars <- unique(unlist(lapply(meta$filters, all.vars)))
  expect_true("TRT" %in% filter_vars)
  expect_true("SEX" %in% filter_vars)
})

test_that("total group metadata translates to correct filter", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    SEX = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    total_groups = list(total_group("TRT", "Total")),
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)

  # Find the res column for "Total"
  total_col <- NULL
  for (rc in grep("^res", names(result), value = TRUE)) {
    lbl <- attr(result[[rc]], "label")
    if (!is.null(lbl) && grepl("Total", lbl)) {
      total_col <- rc
      break
    }
  }
  expect_false(is.null(total_col))

  rid <- result$row_id[1]
  meta <- tplyr_meta_result(result, rid, total_col)
  # Total group: should NOT have a TRT == "Total" filter
  filter_strs <- vapply(meta$filters, deparse1, character(1))
  expect_false(any(grepl('"Total"', filter_strs)))
  # Subset should return rows for both A and B
  subset_data <- tplyr_meta_subset(result, rid, total_col, data)
  expect_true(all(c("A", "B") %in% subset_data$TRT))
})

test_that("custom group metadata translates to %in% filter", {
  data <- data.frame(
    TRT = rep(c("A", "B", "C"), each = 4),
    SEX = rep(c("M", "F"), 6)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    custom_groups = list(custom_group("TRT", "AB" = c("A", "B"))),
    layers = tplyr_layers(group_count("SEX"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)

  # Find the res column for "AB"
  ab_col <- NULL
  for (rc in grep("^res", names(result), value = TRUE)) {
    lbl <- attr(result[[rc]], "label")
    if (!is.null(lbl) && grepl("AB", lbl)) {
      ab_col <- rc
      break
    }
  }
  expect_false(is.null(ab_col))

  rid <- result$row_id[1]
  meta <- tplyr_meta_result(result, rid, ab_col)
  # Should have a %in% filter with the component values
  filter_strs <- vapply(meta$filters, deparse1, character(1))
  expect_true(any(grepl('%in%', filter_strs)))
  # Subset should return rows for A and B only
  subset_data <- tplyr_meta_subset(result, rid, ab_col, data)
  expect_true(all(subset_data$TRT %in% c("A", "B")))
  expect_false("C" %in% subset_data$TRT)
})

test_that("missing count metadata uses is.na filter", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 5),
    SEX = c("M", "F", NA, "M", "F", "M", NA, "F", "M", "F")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("SEX",
        settings = layer_settings(
          missing_count = list(label = "Missing")
        )
      )
    )
  )
  result <- tplyr_build(spec, data, metadata = TRUE)

  missing_row <- result[result$rowlabel1 == "Missing", ]
  if (nrow(missing_row) > 0) {
    rid <- missing_row$row_id[1]
    meta <- tplyr_meta_result(result, rid, "res1")
    # Should have an is.na filter
    filter_strs <- vapply(meta$filters, deparse1, character(1))
    expect_true(any(grepl("is\\.na", filter_strs)))
    # Subset should return rows with NA SEX
    subset_data <- tplyr_meta_subset(result, rid, "res1", data)
    expect_true(all(is.na(subset_data$SEX)))
  }
})

test_that("print.tplyr_meta produces readable output", {
  meta <- tplyr_meta(
    names = c("TRT", "SEX"),
    filters = list(str2lang('TRT == "A"'), str2lang('SEX == "M"')),
    layer_index = 1L
  )
  out <- capture.output(print(meta))
  expect_true(any(grepl("tplyr_meta", out)))
  expect_true(any(grepl("TRT", out)))
  expect_true(any(grepl("SEX", out)))
})

test_that("tplyr_meta_subset roundtrip matches manual filter", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    GRP = rep(c("G1", "G2"), times = 10),
    SEX = rep(c("M", "F"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("SEX", by = "GRP"))
  )
  result <- tplyr_build(spec, data, metadata = TRUE)

  # Get a specific cell and its metadata
  g1_f_row <- result[result$rowlabel1 == "G1" & result$rowlabel2 == "F", ]
  if (nrow(g1_f_row) > 0) {
    rid <- g1_f_row$row_id[1]
    res1_label <- attr(result$res1, "label")
    col_val <- sub("\\s*\\(N=\\d+\\)$", "", res1_label)

    # Get subset via metadata
    meta_subset <- tplyr_meta_subset(result, rid, "res1", data)

    # Manual filter
    manual_subset <- data[data$TRT == col_val & data$GRP == "G1" & data$SEX == "F", ]

    expect_equal(nrow(meta_subset), nrow(manual_subset))
  }
})

test_that("metadata with layer-level where filter", {
  data <- data.frame(
    TRT = rep(c("A", "B"), each = 10),
    SEX = rep(c("M", "F"), 10),
    SEVERITY = rep(c("MILD", "MODERATE"), 10)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(
      group_count("SEX",
        where = SEVERITY == "MODERATE"
      )
    )
  )
  result <- tplyr_build(spec, data, metadata = TRUE)
  rid <- result$row_id[1]
  subset <- tplyr_meta_subset(result, rid, "res1", data)
  if (!is.null(subset) && nrow(subset) > 0) {
    expect_true(all(subset$SEVERITY == "MODERATE"))
  }

  meta <- tplyr_meta_result(result, rid, "res1")
  expect_true("SEVERITY" %in% meta$names)
})

# --- Anti-join / missing subjects metadata tests ---

test_that("missing_subjects metadata has anti_join with correct structure", {
  target <- data.frame(
    TRT = c("A", "A", "B"),
    USUBJID = c("S1", "S2", "S3"),
    VAL = c("X", "Y", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "B", "B"),
    USUBJID = c("S1", "S2", "S4", "S3", "S5")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = TRUE,
          missing_subjects_label = "Not in Pop"
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop, metadata = TRUE)

  ms_row <- result[result$rowlabel1 == "Not in Pop", ]
  expect_equal(nrow(ms_row), 1)

  rid <- ms_row$row_id[1]
  meta <- tplyr_meta_result(result, rid, "res1")
  expect_s3_class(meta, "tplyr_meta")

  # Should have anti_join
  expect_false(is.null(meta$anti_join))
  expect_s3_class(meta$anti_join, "tplyr_meta_anti_join")

  # on field should be the distinct_by variable
  expect_equal(meta$anti_join$on, "USUBJID")

  # join_meta should have column filters
  expect_true(length(meta$anti_join$join_meta$filters) > 0)
  pop_filter_vars <- unique(unlist(lapply(meta$anti_join$join_meta$filters, all.vars)))
  expect_true("TRT" %in% pop_filter_vars)
})

test_that("missing_subjects metadata subset returns correct subjects", {
  target <- data.frame(
    TRT = c("A", "A", "B"),
    USUBJID = c("S1", "S2", "S3"),
    VAL = c("X", "Y", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "B", "B"),
    USUBJID = c("S1", "S2", "S4", "S3", "S5")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = TRUE,
          missing_subjects_label = "Not in Pop"
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop, metadata = TRUE)

  ms_row <- result[result$rowlabel1 == "Not in Pop", ]
  rid <- ms_row$row_id[1]

  # Find column for TRT=A
  a_col <- NULL
  for (rc in grep("^res\\d+$", names(result), value = TRUE)) {
    lbl <- attr(result[[rc]], "label")
    if (!is.null(lbl) && grepl("^A", lbl)) {
      a_col <- rc
      break
    }
  }
  expect_false(is.null(a_col))

  # Subset for TRT=A missing subjects: should return S4
  subset_a <- tplyr_meta_subset(result, rid, a_col, target, pop_data = pop)
  expect_true(is.data.frame(subset_a))
  expect_equal(nrow(subset_a), 1)
  expect_equal(subset_a$USUBJID, "S4")
  expect_equal(subset_a$TRT, "A")

  # Find column for TRT=B
  b_col <- NULL
  for (rc in grep("^res\\d+$", names(result), value = TRUE)) {
    lbl <- attr(result[[rc]], "label")
    if (!is.null(lbl) && grepl("^B", lbl)) {
      b_col <- rc
      break
    }
  }
  expect_false(is.null(b_col))

  # Subset for TRT=B missing subjects: should return S5
  subset_b <- tplyr_meta_subset(result, rid, b_col, target, pop_data = pop)
  expect_equal(nrow(subset_b), 1)
  expect_equal(subset_b$USUBJID, "S5")
  expect_equal(subset_b$TRT, "B")
})

test_that("missing_subjects metadata warns when pop_data not provided to subset", {
  target <- data.frame(
    TRT = c("A", "A"),
    USUBJID = c("S1", "S2"),
    VAL = c("X", "Y")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A"),
    USUBJID = c("S1", "S2", "S3")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = TRUE,
          missing_subjects_label = "Not in Pop"
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop, metadata = TRUE)
  ms_row <- result[result$rowlabel1 == "Not in Pop", ]
  rid <- ms_row$row_id[1]

  # Calling without pop_data should warn
  expect_warning(
    tplyr_meta_subset(result, rid, "res1", target),
    "pop_data is required"
  )
})

test_that("missing_subjects metadata with by-variables filters correctly", {
  target <- data.frame(
    TRT = c("A", "A", "A", "B"),
    USUBJID = c("S1", "S2", "S3", "S4"),
    GRP = c("G1", "G1", "G2", "G1"),
    VAL = c("X", "Y", "X", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B"),
    USUBJID = c("S1", "S2", "S3", "S5", "S4", "S6"),
    GRP = c("G1", "G1", "G2", "G1", "G1", "G1")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        by = "GRP",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = TRUE,
          missing_subjects_label = "Not in Pop"
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop, metadata = TRUE)

  # Missing subjects row is aggregated (not broken out by GRP)
  ms_row <- result[result$rowlabel2 == "Not in Pop", ]
  expect_equal(nrow(ms_row), 1)

  rid <- ms_row$row_id[1]

  # Metadata for TRT=A (res1): anti-join should exist

  meta_a <- tplyr_meta_result(result, rid, "res1")
  expect_false(is.null(meta_a$anti_join))
  expect_equal(meta_a$anti_join$on, "USUBJID")

  # Main filters should include TRT (col var)
  filter_vars_a <- unique(unlist(lapply(meta_a$filters, all.vars)))
  expect_true("TRT" %in% filter_vars_a)

  # Pop-side filters should include TRT
  pop_vars_a <- unique(unlist(lapply(meta_a$anti_join$join_meta$filters, all.vars)))
  expect_true("TRT" %in% pop_vars_a)

  # Functional check: S5 is in pop TRT=A but not in target
  subset_a <- tplyr_meta_subset(result, rid, "res1", target, pop_data = pop)
  expect_equal(nrow(subset_a), 1)
  expect_equal(subset_a$USUBJID, "S5")

  # Metadata for TRT=B (res2): S6 is missing
  subset_b <- tplyr_meta_subset(result, rid, "res2", target, pop_data = pop)
  expect_equal(nrow(subset_b), 1)
  expect_equal(subset_b$USUBJID, "S6")
})

test_that("missing_subjects without distinct_by has no anti_join in metadata", {
  target <- data.frame(
    TRT = c("A", "A", "B"),
    VAL = c("X", "Y", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "A", "B", "B", "B"),
    VAL = c("X", "Y", "Z", "W", "X", "Y", "Z")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          missing_subjects = TRUE,
          missing_subjects_label = "Not in Pop"
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop, metadata = TRUE)
  ms_row <- result[result$rowlabel1 == "Not in Pop", ]
  if (nrow(ms_row) > 0) {
    rid <- ms_row$row_id[1]
    meta <- tplyr_meta_result(result, rid, "res1")
    # Without distinct_by, no anti_join (row-level counting is not expressible)
    expect_null(meta$anti_join)
  }
})

test_that("missing_subjects metadata with pop_data where filter", {
  target <- data.frame(
    TRT = c("A", "A"),
    USUBJID = c("S1", "S2"),
    VAL = c("X", "Y")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "A"),
    USUBJID = c("S1", "S2", "S3", "S4"),
    SAFFL = c("Y", "Y", "Y", "N")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT", where = SAFFL == "Y"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = TRUE,
          missing_subjects_label = "Not in Pop"
        )
      )
    )
  )
  result <- tplyr_build(spec, target, pop_data = pop, metadata = TRUE)
  ms_row <- result[result$rowlabel1 == "Not in Pop", ]
  if (nrow(ms_row) > 0) {
    rid <- ms_row$row_id[1]
    meta <- tplyr_meta_result(result, rid, "res1")

    # Anti-join pop filters should include the pop where filter
    expect_false(is.null(meta$anti_join))
    pop_filter_vars <- unique(unlist(lapply(meta$anti_join$join_meta$filters, all.vars)))
    expect_true("SAFFL" %in% pop_filter_vars)

    # Functional check: S4 has SAFFL=N so should be excluded from pop
    # Only S3 is the truly missing subject (in pop, not in target, SAFFL=Y)
    subset <- tplyr_meta_subset(result, rid, "res1", target, pop_data = pop)
    expect_equal(nrow(subset), 1)
    expect_equal(subset$USUBJID, "S3")
  }
})

test_that("missing_subjects with zero missing produces correct metadata", {
  data <- data.frame(
    TRT = c("A", "B"),
    USUBJID = c("S1", "S2"),
    VAL = c("X", "X")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    layers = tplyr_layers(
      group_count("VAL",
        settings = layer_settings(
          distinct_by = "USUBJID",
          missing_subjects = TRUE,
          missing_subjects_label = "Not in Pop"
        )
      )
    )
  )
  result <- tplyr_build(spec, data, pop_data = data, metadata = TRUE)
  ms_row <- result[result$rowlabel1 == "Not in Pop", ]
  if (nrow(ms_row) > 0) {
    rid <- ms_row$row_id[1]
    # Subset should return 0 rows (everyone in pop is also in target)
    subset <- tplyr_meta_subset(result, rid, "res1", data, pop_data = data)
    expect_equal(nrow(subset), 0)
  }
})

test_that("print.tplyr_meta displays anti-join info", {
  meta <- tplyr_meta(
    names = c("TRT", "USUBJID"),
    filters = list(str2lang('TRT == "A"')),
    layer_index = 1L,
    anti_join = tplyr_meta_anti_join(
      join_meta = tplyr_meta(
        names = "TRT",
        filters = list(str2lang('TRT == "A"')),
        layer_index = 1L
      ),
      on = "USUBJID"
    )
  )
  out <- capture.output(print(meta))
  expect_true(any(grepl("Anti-join", out)))
  expect_true(any(grepl("USUBJID", out)))
})
