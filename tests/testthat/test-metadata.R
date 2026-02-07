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

test_that("tplyr_meta_result returns tplyr_meta object for count cell", {
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
  expect_true(is.integer(meta$row_indices))
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
    meta1 <- tplyr_meta_result(result, rid, "res1")
    if (!is.null(meta1)) {
      # Number of source rows should match the count for that cell
      sex_val <- result$rowlabel1[i]
      res1_label <- attr(result$res1, "label")
      col_level <- sub("\\s*\\(N=\\d+\\)$", "", res1_label)

      matching_nd <- nd[nd$SEX == sex_val & nd$TRT == col_level, ]
      if (nrow(matching_nd) > 0) {
        expect_equal(length(meta1$row_indices), matching_nd$n[1])
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
  # Desc layer: all rows in the group contribute
  expect_true(length(meta$row_indices) > 0)
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

test_that("metadata for special rows (Total) has empty indices", {
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
    # Total row: "Total" doesn't match any actual SEX value in data
    # so row_indices should be empty
    expect_s3_class(meta, "tplyr_meta")
    expect_equal(length(meta$row_indices), 0)
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
