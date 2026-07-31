test_that("tplyr_build returns a data.frame", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX")
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)
  expect_s3_class(result, "data.frame")
})

test_that("tplyr_build applies global where filter", {
  test_data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    SEX = c("M", "F", "M", "F"),
    FLAG = c("Y", "N", "Y", "Y")
  )

  spec <- tplyr_spec(
    cols = "TRT",
    where = FLAG == "Y",
    layers = tplyr_layers(
      group_count(target_var = "SEX")
    )
  )
  result <- tplyr_build(spec, test_data)

  # Only 3 rows pass the filter (FLAG == "Y")
  # So total for A = 1 (only M), total for B = 2 (M and F)
  m_row <- result[result$rowlabel1 == "M", ]
  expect_true(grepl("1", m_row$res1))
})

test_that("tplyr_build sorts layers correctly", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX"),
      group_desc(
        target_var = "AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xxx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Count layer (index 1) should come before desc layer (index 2)
  layer_indices <- result$ord_layer_index
  is_sorted <- all(diff(layer_indices) >= 0)
  expect_true(is_sorted)

  # First rows should have ordindx = 1
  expect_equal(result$ord_layer_index[1], 1)
  # Last row should have ordindx = 2
  expect_equal(result$ord_layer_index[nrow(result)], 2)
})

test_that("tplyr_build handles multiple layers", {
  data(tplyr_adsl, package = "tplyr2")
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = tplyr_layers(
      group_count(target_var = "SEX", by = "Sex n (%)"),
      group_count(target_var = "RACE", by = "Race n (%)"),
      group_desc(
        target_var = "AGE",
        by = "Age (Years)",
        settings = layer_settings(
          format_strings = list("n" = f_str("xxx", "n"), "Mean" = f_str("xx.x", "mean"))
        )
      )
    )
  )
  result <- tplyr_build(spec, tplyr_adsl)

  # Should have all three layers
  expect_true(1 %in% result$ord_layer_index)
  expect_true(2 %in% result$ord_layer_index)
  expect_true(3 %in% result$ord_layer_index)
})

test_that("apply_overrides works", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("SEX")))
  updated <- tplyr2:::apply_overrides(spec, list(cols = "TRT01A"))
  expect_equal(updated$cols, "TRT01A")
})

test_that("apply_overrides is no-op with no overrides", {
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(group_count("SEX")))
  updated <- tplyr2:::apply_overrides(spec, list())
  expect_equal(updated$cols, "TRT01P")
})

test_that("harmonize_and_bind handles empty list", {
  result <- tplyr2:::harmonize_and_bind(list())
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("tplyr_build errors on unknown layer type", {
  spec <- tplyr_spec(
    cols = "TRT01P",
    layers = list(structure(list(), class = "tplyr_unknown"))
  )
  expect_error(tplyr_build(spec, data.frame(TRT01P = "A")), "not a tplyr_layer")
})

# Coverage: dispatch guard for an unknown layer type (reachable e.g. from a
# malformed deserialized spec whose layer has no recognized subclass)
test_that("tplyr_build errors on a layer with an unknown type", {
  bogus_layer <- structure(
    list(target_var = "VAL", by = NULL, where = NULL, settings = layer_settings()),
    class = "tplyr_layer"
  )
  spec <- tplyr_spec(cols = "TRT", layers = list(bogus_layer))
  d <- data.frame(TRT = c("A", "B"), VAL = c("X", "Y"))
  expect_error(tplyr_build(spec, d), "Unknown layer type")
})

# ---------------------------------------------------------------------------
# A layer `where` that empties a column group must not shift the columns
# ---------------------------------------------------------------------------

test_that("a layer whose where empties a column group keeps its values in the right column", {
  ae <- as.data.frame(tplyr_adae)
  # only Xanomeline Low Dose has SEVERE records
  expect_equal(sort(unique(ae$TRTA[ae$AESEV == "SEVERE"])), "Xanomeline Low Dose")

  b <- tplyr_build(tplyr_spec(cols = "TRTA", layers = tplyr_layers(
    group_count("AEDECOD"),
    group_count("AEDECOD", where = AESEV == "SEVERE"))), ae)

  res <- grep("^res\\d+$", names(b), value = TRUE)
  expect_length(res, 3L)
  labs <- vapply(res, function(cc) attr(b[[cc]], "label"), character(1))
  expect_true(any(grepl("^Xanomeline Low Dose", labs)))
  low <- res[grepl("^Xanomeline Low Dose", labs)]
  other <- setdiff(res, low)

  sev <- b[b$ord_layer_index == 2 & b$rowlabel1 == "AGITATION", ]
  expect_equal(str_extract_num(sev[[low]], 1), 1)
  for (cc in other) expect_equal(str_extract_num(sev[[cc]], 1), 0)
})

test_that("the where-emptied column group round-trips through metadata", {
  ae <- as.data.frame(tplyr_adae)
  b <- tplyr_build(tplyr_spec(cols = "TRTA", layers = tplyr_layers(
    group_count("AEDECOD"),
    group_count("AEDECOD", where = AESEV == "SEVERE"))), ae, metadata = TRUE)

  for (i in which(b$ord_layer_index == 2)) {
    for (cc in grep("^res\\d+$", names(b), value = TRUE)) {
      v <- trimws(b[[cc]][i])
      if (!nzchar(v)) next
      sub <- tplyr_meta_subset(b, b$row_id[i], cc, ae)
      expect_equal(as.numeric(nrow(sub)), as.numeric(str_extract_num(v, 1)),
                   info = paste(b$rowlabel1[i], cc))
    }
  }
})
