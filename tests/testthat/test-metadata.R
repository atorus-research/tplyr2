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
  # Without distinct_by there is no subject key, so row-level missing-subjects
  # counting is not expressible as a filter set: the build warns and emits no
  # metadata for that row rather than the complement of what the cell counts.
  expect_warning(
    result <- tplyr_build(spec, target, pop_data = pop, metadata = TRUE),
    "no subject key to anti-join on"
  )
  ms_row <- result[result$rowlabel1 == "Not in Pop", ]
  if (nrow(ms_row) > 0) {
    rid <- ms_row$row_id[1]
    expect_null(tplyr_meta_result(result, rid, "res1"))
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

# Coverage: print.tplyr_meta
test_that("print.tplyr_meta renders names, filters and statistic", {
  d <- data.frame(TRT = rep(c("A","B"), each = 6), V = rep(c("X","Y"), 6))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(group_count("V"))),
                   d, metadata = TRUE)
  rid <- b$row_id[1]
  m <- tplyr_meta_result(b, rid, "res1")
  expect_output(print(m), "tplyr_meta")
})

# ---------------------------------------------------------------------------
# Round-trip consistency: a cell's metadata must reproduce what it displays
# ---------------------------------------------------------------------------

meta_rt_data <- function() {
  data.frame(
    TRT  = rep(c("A", "B"), each = 10),
    V    = c("X", "X", "Y", "UNK", NA, "Y", "X", "Z", "UNK", "X",
             "Y", "Y", "X", NA, "UNK", "Z", "Z", "X", "Y", "X"),
    SUBJ = sprintf("S%02d", 1:20),
    stringsAsFactors = FALSE
  )
}

# Recompute the leading statistic of every cell and compare to the display
expect_meta_roundtrip <- function(b, data, stat = "n", distinct_by = NULL,
                                  pop_data = NULL, res_cols = NULL) {
  if (is.null(res_cols)) res_cols <- grep("^res\\d+$", names(b), value = TRUE)
  for (i in seq_len(nrow(b))) {
    for (cc in res_cols) {
      cell <- b[[cc]][i]
      if (is.na(cell) || !nzchar(trimws(cell))) next
      shown <- str_extract_num(cell, 1)
      expect_false(is.null(tplyr_meta_result(b, b$row_id[i], cc)),
                   info = paste("no metadata for row", i, cc))
      sub <- tplyr_meta_subset(b, b$row_id[i], cc, data, pop_data = pop_data)
      got <- if (stat == "distinct_n") length(unique(sub[[distinct_by]])) else nrow(sub)
      expect_equal(as.numeric(got), as.numeric(shown),
                   info = paste("row", i, b$rowlabel1[i], cc))
    }
  }
}

test_that("count metadata round-trips with missing_values folded into Missing", {
  d <- meta_rt_data()
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      missing_count = list(label = "Missing", missing_values = "UNK"))))),
    d, metadata = TRUE)
  expect_meta_roundtrip(b, d)
})

test_that("total row metadata round-trips under both count_missings settings", {
  d <- meta_rt_data()
  for (tcm in c(TRUE, FALSE)) {
    b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_count("V", settings = layer_settings(
        missing_count = list(label = "Missing", missing_values = "UNK"),
        total_row = TRUE, total_row_count_missings = tcm)))),
      d, metadata = TRUE)
    expect_meta_roundtrip(b, d)
  }
})

test_that("total row n and distinct_n agree with each other", {
  d <- meta_rt_data()
  for (tcm in c(TRUE, FALSE)) {
    b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_count("V", settings = layer_settings(
        distinct_by = "SUBJ",
        missing_count = list(label = "Missing", missing_values = "UNK"),
        total_row = TRUE, total_row_count_missings = tcm,
        format_strings = list(n_counts = f_str("xx / xx", "n", "distinct_n")))))),
      d, metadata = TRUE)
    tr <- b[b$rowlabel1 == "Total", ]
    # one row per subject in this fixture, so n and distinct_n must match
    expect_equal(str_extract_num(tr$res1, 1), str_extract_num(tr$res1, 2))
    expect_equal(str_extract_num(tr$res2, 1), str_extract_num(tr$res2, 2))
  }
})

test_that("total_row_count_missings = TRUE includes missing records in the total", {
  d <- meta_rt_data()
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      missing_count = list(label = "Missing", missing_values = "UNK"),
      total_row = TRUE, total_row_count_missings = TRUE)))),
    d, metadata = TRUE)
  # arm A has 10 records in total, including 2 UNK and 1 NA
  expect_equal(str_extract_num(b$res1[b$rowlabel1 == "Total"], 1), 10)
})

test_that("nested count metadata round-trips with missing_values", {
  d <- data.frame(
    TRT  = rep(c("A", "B"), each = 6),
    SUBJ = sprintf("S%02d", 1:12),
    SOC  = c("CARD", "CARD", "GI", "GI", "UNK", "CARD",
             "GI", "GI", "CARD", "UNK", "GI", "CARD"),
    PT   = c("AF", "MI", "NAUSEA", "VOM", "UNKPT", "AF",
             "NAUSEA", "VOM", "MI", "UNKPT", "NAUSEA", "AF"),
    stringsAsFactors = FALSE
  )
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count(c("SOC", "PT"), settings = layer_settings(
      distinct_by = "SUBJ",
      missing_count = list(label = "Missing", missing_values = "UNK"),
      format_strings = list(
        n_counts = f_str("xx (xx.x%)", "distinct_n", "distinct_pct")))))),
    d, metadata = TRUE)
  expect_meta_roundtrip(b, d, stat = "distinct_n", distinct_by = "SUBJ")
})

test_that("shift layer metadata round-trips, including with CI keywords", {
  set.seed(11); n <- 40
  d <- data.frame(
    SUBJ = sprintf("S%02d", seq_len(n)),
    TRT  = rep(c("A", "B"), each = n / 2),
    BASE = factor(sample(c("LOW", "NORM"), n, TRUE), levels = c("LOW", "NORM")),
    POST = factor(sample(c("LOW", "NORM"), n, TRUE), levels = c("LOW", "NORM")),
    stringsAsFactors = FALSE
  )
  for (fs in list(f_str("xx (xx.x%)", "n", "pct"),
                  f_str("xx (xx.x%) [xx.x, xx.x]", "n", "pct", "ci_lower", "ci_upper"))) {
    b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
      group_shift(c(row = "BASE", column = "POST"), settings = layer_settings(
        format_strings = list(n_counts = fs))))), d, metadata = TRUE)
    expect_meta_roundtrip(b, d)
  }
})

test_that("stat_columns metadata round-trips on both statistic columns", {
  d <- meta_rt_data()
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count("V", settings = layer_settings(
      distinct_by = "SUBJ",
      stat_columns = list(
        "n (%)" = f_str("xx (xx.x%)", "distinct_n", "distinct_pct"),
        "E"     = f_str("xx", "n")))))), d, metadata = TRUE)
  rc <- grep("^res\\d+$", names(b), value = TRUE)
  expect_meta_roundtrip(b, d, stat = "distinct_n", distinct_by = "SUBJ",
                        res_cols = rc[c(TRUE, FALSE)])
  expect_meta_roundtrip(b, d, stat = "n", res_cols = rc[c(FALSE, TRUE)])
})

test_that("stats_as_columns with a by variable resolves column filters correctly", {
  d <- data.frame(TRT = rep(c("A", "B"), each = 8), GRP = rep(c("G1", "G2"), 8),
                  VAL = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5,
                          2, 3, 4, 5, 6, 7, 8, 9))
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("VAL", by = "GRP", settings = layer_settings(
      stats_as_columns = TRUE,
      format_strings = list("n" = f_str("xx", "n"),
                            "Mean" = f_str("xx.x", "mean")))))), d, metadata = TRUE)

  for (i in seq_len(nrow(b))) {
    for (cc in grep("^res\\d+$", names(b), value = TRUE)) {
      lbl <- attr(b[[cc]], "label")
      sub <- tplyr_meta_subset(b, b$row_id[i], cc, d)
      # the column filter must resolve to a real treatment level, not the
      # composite "A | n" label
      expect_gt(nrow(sub), 0)
      shown <- str_extract_num(b[[cc]][i], 1)
      got <- if (grepl("\\| n$", lbl)) nrow(sub) else round(mean(sub$VAL), 1)
      expect_equal(as.numeric(got), as.numeric(shown), info = paste(i, cc, lbl))
    }
  }
})

test_that("stats_as_columns without a by variable warns that metadata is unavailable", {
  d <- data.frame(TRT = rep(c("A", "B"), each = 8), VAL = 1:16)
  spec <- tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("VAL", settings = layer_settings(
      stats_as_columns = TRUE,
      format_strings = list("n" = f_str("xx", "n"))))))
  expect_warning(tplyr_build(spec, d, metadata = TRUE),
                 "cell metadata is not available for a stats_as_columns layer")
  # and no warning when metadata was not requested
  expect_silent(tplyr_build(spec, d))
})

# ---------------------------------------------------------------------------
# generate_row_ids() is only meaningful on an unmodified build
# ---------------------------------------------------------------------------

test_that("generate_row_ids matches the attached row_id column on a fresh build", {
  b <- tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("DCDECOD", by = "SEX"))), tplyr_adsl, metadata = TRUE)
  expect_equal(generate_row_ids(b), b$row_id)
  expect_false(anyDuplicated(b$row_id) > 0)
})

test_that("row IDs stay unique on a nested layer that repeats an inner level", {
  d <- data.frame(
    TRT = rep("A", 6), SUBJ = sprintf("S%d", 1:6),
    SOC = c("CARD", "CARD", "GI", "GI", "RESP", "RESP"),
    PT  = c("PAIN", "AF", "PAIN", "NAUSEA", "PAIN", "COUGH"),
    stringsAsFactors = FALSE
  )
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_count(c("SOC", "PT"), settings = layer_settings(
      distinct_by = "SUBJ",
      format_strings = list(n_counts = f_str("xx", "distinct_n")))))), d,
    metadata = TRUE)
  # "PAIN" appears under three system organ classes
  expect_equal(sum(b$rowlabel2 == "PAIN"), 3L)
  expect_equal(anyDuplicated(b$row_id), 0L)
})

test_that("generate_row_ids warns when labels have been blanked by row masks", {
  b <- tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("DCDECOD", by = "SEX"))), tplyr_adsl, metadata = TRUE)
  masked <- apply_row_masks(b)
  expect_warning(generate_row_ids(masked), "duplicate ID")
  # the attached column is unaffected and still keys the metadata
  expect_equal(masked$row_id, b$row_id)
  expect_false(is.null(tplyr_meta_result(b, masked$row_id[2], "res1")))
})

# ---------------------------------------------------------------------------
# An empty filter set means "unrestricted", not "no rows"
# ---------------------------------------------------------------------------

test_that("a total_group column with no row filter yields the whole dataset", {
  d <- as.data.frame(tplyr_adsl)
  b <- tplyr_build(tplyr_spec(
    cols = "TRT01P",
    total_groups = list(total_group("TRT01P", label = "Total")),
    layers = tplyr_layers(group_desc("AGE", settings = layer_settings(
      format_strings = list(n = f_str("xxx", "n"),
                            Mean = f_str("xx.xx", "mean")))))), d, metadata = TRUE)

  tot <- grep("^res", names(b), value = TRUE)[
    vapply(grep("^res", names(b), value = TRUE),
           function(cc) grepl("^Total", attr(b[[cc]], "label")), logical(1))]

  m <- tplyr_meta_result(b, b$row_id[1], tot)
  expect_length(m$filters, 0)              # the cell genuinely has no filters
  sub <- tplyr_meta_subset(b, b$row_id[1], tot, d)
  expect_equal(nrow(sub), nrow(d))

  n_row <- which(trimws(b$rowlabel1) == "n")
  expect_equal(str_extract_num(b[[tot]][n_row], 1), nrow(d))
  mean_row <- which(trimws(b$rowlabel1) == "Mean")
  expect_equal(round(mean(sub$AGE), 2),
               str_extract_num(b[[tot]][mean_row], 1))
})

test_that("a count total row in a total_group column round-trips", {
  d <- as.data.frame(tplyr_adsl)
  b <- tplyr_build(tplyr_spec(
    cols = "TRT01P",
    total_groups = list(total_group("TRT01P", label = "Total")),
    layers = tplyr_layers(group_count("SEX", settings = layer_settings(
      total_row = TRUE)))), d, metadata = TRUE)
  tot <- grep("^res", names(b), value = TRUE)[
    vapply(grep("^res", names(b), value = TRUE),
           function(cc) grepl("^Total", attr(b[[cc]], "label")), logical(1))]
  i <- which(trimws(b$rowlabel1) == "Total")
  sub <- tplyr_meta_subset(b, b$row_id[i], tot, d)
  expect_equal(nrow(sub), str_extract_num(b[[tot]][i], 1))
})

test_that("total and custom group columns both round-trip together", {
  d <- as.data.frame(tplyr_adsl)
  b <- tplyr_build(tplyr_spec(
    cols = "TRT01P",
    total_groups  = list(total_group("TRT01P", label = "Total")),
    custom_groups = list(custom_group("TRT01P",
      "Treated" = c("Xanomeline High Dose", "Xanomeline Low Dose"))),
    layers = tplyr_layers(group_count("SEX"))), d, metadata = TRUE)
  expect_meta_roundtrip(b, d)
})

# ---------------------------------------------------------------------------
# by-variable values that are blank, NA, or padded
# ---------------------------------------------------------------------------

test_that("a by level that is an empty string is still filtered on", {
  lb <- as.data.frame(tplyr_adlb)
  expect_gt(sum(lb$AVISIT == ""), 0)   # the shipped data really has these
  b <- tplyr_build(tplyr_spec(cols = "TRTA", layers = tplyr_layers(
    group_desc("AVAL", by = "AVISIT", settings = layer_settings(
      format_strings = list("n" = f_str("xxx", "n")))))), lb, metadata = TRUE)
  expect_meta_roundtrip(b, lb)
})

test_that("a by level that is NA builds metadata and filters with is.na()", {
  d <- as.data.frame(tplyr_adsl)
  d$GRP <- ifelse(seq_len(nrow(d)) %% 3 == 0, NA, "G1")
  b <- tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_desc("AGE", by = "GRP", settings = layer_settings(
      format_strings = list("n" = f_str("xxx", "n")))))), d, metadata = TRUE)
  expect_meta_roundtrip(b, d)
  na_row <- which(is.na(b$rowlabel1) | b$rowlabel1 == "NA")
  expect_gt(length(na_row), 0)
  m <- tplyr_meta_result(b, b$row_id[na_row[1]], "res1")
  expect_true(any(grepl("is.na", vapply(m$filters, deparse1, character(1)))))
})

test_that("by values with surrounding whitespace are matched untrimmed", {
  d <- as.data.frame(tplyr_adsl)
  d$GRP2 <- ifelse(d$SEX == "F", "F ", " M")
  b <- tplyr_build(tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_desc("AGE", by = "GRP2", settings = layer_settings(
      format_strings = list("n" = f_str("xxx", "n")))))), d, metadata = TRUE)
  expect_meta_roundtrip(b, d)
})

test_that("a nested layer's empty inner label still contributes no filter", {
  ae <- as.data.frame(tplyr_adae)
  b <- tplyr_build(tplyr_spec(cols = "TRTA", layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"), settings = layer_settings(
      distinct_by = "USUBJID",
      format_strings = list(n_counts = f_str("xx", "n")))))), ae, metadata = TRUE)
  # an outer-level row filters on AEBODSYS only
  outer <- which(!nzchar(trimws(b$rowlabel2)))[1]
  m <- tplyr_meta_result(b, b$row_id[outer], "res1")
  expect_false(any(grepl("AEDECOD", vapply(m$filters, deparse1, character(1)))))
  expect_meta_roundtrip(b, ae)
})

test_that("missing_subjects without distinct_by yields no metadata, with a warning", {
  pop <- data.frame(TRT = rep(c("A", "B"), each = 12),
                    SUBJ = sprintf("P%02d", 1:24), stringsAsFactors = FALSE)
  d <- data.frame(TRT = rep(c("A", "B"), each = 4),
                  SUBJ = c("P01", "P01", "P02", "P03", "P13", "P14", "P14", "P15"),
                  AE = c("H", "N", "H", "N", "H", "H", "N", "N"),
                  stringsAsFactors = FALSE)
  spec <- tplyr_spec(cols = "TRT", pop_data = pop_data(cols = c("TRT" = "TRT")),
    layers = tplyr_layers(group_count("AE", settings = layer_settings(
      missing_subjects = TRUE,
      format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct"))))))

  expect_warning(b <- tplyr_build(spec, d, pop_data = pop, metadata = TRUE),
                 "no subject key to anti-join on")
  i <- which(b$rowlabel1 == "Missing")
  expect_null(tplyr_meta_result(b, b$row_id[i], "res1"))
  # ordinary rows are unaffected
  expect_false(is.null(tplyr_meta_result(b, b$row_id[1], "res1")))
})

test_that("missing_subjects with distinct_by anti-joins to the counted subjects", {
  pop <- data.frame(TRT = rep(c("A", "B"), each = 12),
                    SUBJ = sprintf("P%02d", 1:24), stringsAsFactors = FALSE)
  d <- data.frame(TRT = rep(c("A", "B"), each = 4),
                  SUBJ = c("P01", "P01", "P02", "P03", "P13", "P14", "P14", "P15"),
                  AE = c("H", "N", "H", "N", "H", "H", "N", "N"),
                  stringsAsFactors = FALSE)
  b <- tplyr_build(tplyr_spec(cols = "TRT", pop_data = pop_data(cols = c("TRT" = "TRT")),
    layers = tplyr_layers(group_count("AE", settings = layer_settings(
      missing_subjects = TRUE, distinct_by = "SUBJ",
      format_strings = list(n_counts = f_str("xx (xx.x%)", "n", "pct")))))),
    d, pop_data = pop, metadata = TRUE)
  i <- which(b$rowlabel1 == "Missing")
  for (cc in c("res1", "res2")) {
    sub <- tplyr_meta_subset(b, b$row_id[i], cc, d, pop_data = pop)
    expect_equal(length(unique(sub$SUBJ)), str_extract_num(b[[cc]][i], 1))
  }
})
