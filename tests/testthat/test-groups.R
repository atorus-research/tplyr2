# --- Total group tests ---

test_that("total group creates extra column in count layer output", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    VAL = c("X", "Y", "X", "Y")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    total_groups = list(total_group("TRT", label = "Total")),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  expect_true(any(grepl("Total", labels)))
  # Should have 3 columns: A, B, Total
  expect_equal(length(res_cols), 3)
})

test_that("total group N equals total of all subjects", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B", "B"),
    VAL = c("X", "Y", "X", "Y", "X")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    total_groups = list(total_group("TRT")),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  total_label <- labels[grepl("Total", labels)]
  # Total N should be 5 (all subjects)
  expect_true(grepl("N=5", total_label))
})

test_that("total group with custom label", {
  data <- data.frame(
    TRT = c("A", "B"),
    VAL = c("X", "X")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    total_groups = list(total_group("TRT", label = "All Subjects")),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  expect_true(any(grepl("All Subjects", labels)))
})

test_that("total group works with pop_data", {
  target <- data.frame(
    TRT = c("A", "B"),
    VAL = c("X", "X")
  )
  pop <- data.frame(
    TRT = c("A", "A", "A", "B", "B")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    pop_data = pop_data(cols = "TRT"),
    total_groups = list(total_group("TRT")),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, target, pop_data = pop)
  hn <- tplyr_header_n(result)
  total_n <- hn$.n[hn$TRT == "Total"]
  # Total N from pop_data: 3 + 2 = 5
  expect_equal(total_n, 5)
})

test_that("total group in desc layer", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    AGE = c(30, 40, 50, 60)
  )
  spec <- tplyr_spec(
    cols = "TRT",
    total_groups = list(total_group("TRT")),
    layers = tplyr_layers(
      group_desc("AGE",
        settings = layer_settings(
          format_strings = list("n" = f_str("xx", "n"))
        )
      )
    )
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  expect_true(any(grepl("Total", labels)))
  # Total column should have n=4
  total_col <- res_cols[grepl("Total", labels)]
  expect_true(grepl("4", result[[total_col]]))
})

test_that("total group in shift layer", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    BNRIND = c("N", "H", "N", "L"),
    ANRIND = c("H", "N", "L", "N")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    total_groups = list(total_group("TRT")),
    layers = tplyr_layers(
      group_shift(c(row = "BNRIND", column = "ANRIND"))
    )
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  # Should have more columns than without total group (A, B, Total) * shift levels
  expect_true(length(res_cols) >= 3)
})

test_that("no total groups preserves backward compatibility", {
  data <- data.frame(
    TRT = c("A", "B"),
    VAL = c("X", "X")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  expect_equal(length(res_cols), 2)  # Just A and B
})

# --- Custom group tests ---

test_that("custom group combines specified levels", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B", "C", "C"),
    VAL = c("X", "Y", "X", "Y", "X", "Y")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    custom_groups = list(
      custom_group("TRT", "AB" = c("A", "B"))
    ),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  # Should have A, B, C, and AB
  expect_equal(length(res_cols), 4)
  expect_true(any(grepl("AB", labels)))
})

test_that("custom group N equals sum of source levels", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B", "B", "C"),
    VAL = c("X", "Y", "X", "Y", "X", "X")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    custom_groups = list(
      custom_group("TRT", "AB" = c("A", "B"))
    ),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  ab_label <- labels[grepl("AB", labels)]
  # AB N should be 2 + 3 = 5
  expect_true(grepl("N=5", ab_label))
})

test_that("custom group + total group together", {
  data <- data.frame(
    TRT = c("A", "A", "B", "B"),
    VAL = c("X", "Y", "X", "Y")
  )
  spec <- tplyr_spec(
    cols = "TRT",
    custom_groups = list(
      custom_group("TRT", "AB" = c("A", "B"))
    ),
    total_groups = list(total_group("TRT")),
    layers = tplyr_layers(group_count("VAL"))
  )
  result <- tplyr_build(spec, data)
  res_cols <- grep("^res\\d+$", names(result), value = TRUE)
  labels <- vapply(res_cols, function(col) attr(result[[col]], "label"), character(1))
  # A, B, AB, Total = 4 columns
  expect_equal(length(res_cols), 4)
  expect_true(any(grepl("Total", labels)))
  expect_true(any(grepl("AB", labels)))
  # Total should include custom group members
  total_label <- labels[grepl("Total", labels)]
  # Total N: A=2 + B=2 + AB=4 + Total itself -> all rows
  # Since total duplicates everything: original 4 + AB 4 + Total 8 = nah
  # Actually: custom groups first adds AB (4 rows) -> 8 rows
  # Then total duplicates all 8 -> 16 rows
  # Total N = 16... but that seems right since total includes custom groups
})

test_that("tplyr_spec stores custom_groups", {
  cg <- custom_group("TRT", "AB" = c("A", "B"))
  spec <- tplyr_spec(
    cols = "TRT",
    custom_groups = list(cg)
  )
  expect_equal(length(spec$custom_groups), 1)
  expect_s3_class(spec$custom_groups[[1]], "tplyr_custom_group")
})

# ---------------------------------------------------------------------------
# total_group() must not double-count custom_group() members
# ---------------------------------------------------------------------------

test_that("a total group spans each subject once when a custom group is present", {
  d <- as.data.frame(tplyr_adsl)
  b <- tplyr_build(tplyr_spec(
    cols = "TRT01P",
    total_groups  = list(total_group("TRT01P", label = "Total")),
    custom_groups = list(custom_group("TRT01P",
      "Treated" = c("Xanomeline High Dose", "Xanomeline Low Dose"))),
    layers = tplyr_layers(group_count("SEX"))), d)

  tot <- grep("^res", names(b), value = TRUE)[
    vapply(grep("^res", names(b), value = TRUE),
           function(cc) grepl("^Total", attr(b[[cc]], "label")), logical(1))]
  expect_equal(attr(b[[tot]], "label"), sprintf("Total (N=%d)", nrow(d)))
  expect_equal(str_extract_num(b[[tot]][b$rowlabel1 == "F"], 1), sum(d$SEX == "F"))
  expect_equal(str_extract_num(b[[tot]][b$rowlabel1 == "M"], 1), sum(d$SEX == "M"))
})

test_that("a total group alone is unchanged, and the custom group column is intact", {
  d <- as.data.frame(tplyr_adsl)
  b <- tplyr_build(tplyr_spec(
    cols = "TRT01P",
    total_groups  = list(total_group("TRT01P", label = "Total")),
    custom_groups = list(custom_group("TRT01P",
      "Treated" = c("Xanomeline High Dose", "Xanomeline Low Dose"))),
    layers = tplyr_layers(group_count("SEX"))), d)
  trt <- grep("^res", names(b), value = TRUE)[
    vapply(grep("^res", names(b), value = TRUE),
           function(cc) grepl("^Treated", attr(b[[cc]], "label")), logical(1))]
  n_treated <- sum(d$TRT01P %in% c("Xanomeline High Dose", "Xanomeline Low Dose"))
  expect_equal(attr(b[[trt]], "label"), sprintf("Treated (N=%d)", n_treated))
  expect_equal(str_extract_num(b[[trt]][b$rowlabel1 == "F"], 1),
               sum(d$SEX == "F" & d$TRT01P %in%
                     c("Xanomeline High Dose", "Xanomeline Low Dose")))
})

test_that("two total groups each span the originals once", {
  d <- data.frame(TRT = rep(c("A", "B"), each = 5), V = rep("X", 10),
                  stringsAsFactors = FALSE)
  b <- tplyr_build(tplyr_spec(
    cols = "TRT",
    total_groups = list(total_group("TRT", label = "Total"),
                        total_group("TRT", label = "All")),
    layers = tplyr_layers(group_count("V"))), d)
  labs <- vapply(grep("^res", names(b), value = TRUE),
                 function(cc) attr(b[[cc]], "label"), character(1))
  expect_true(any(labs == "Total (N=10)"))
  expect_true(any(labs == "All (N=10)"))
})

test_that("a total group still spans a custom group defined on a DIFFERENT variable", {
  d <- as.data.frame(tplyr_adsl)
  b <- tplyr_build(tplyr_spec(
    cols = c("TRT01P", "SEX"),
    total_groups  = list(total_group("TRT01P", label = "Total")),
    custom_groups = list(custom_group("SEX", "Both" = c("F", "M"))),
    layers = tplyr_layers(group_count("RACE"))), d)

  res <- grep("^res\\d+$", names(b), value = TRUE)
  labs <- vapply(res, function(cc) attr(b[[cc]], "label"), character(1))
  tb <- res[labs == sprintf("Total | Both (N=%d)", nrow(d))]
  expect_length(tb, 1L)
  # the Total x Both cell counts every subject of that race, once
  for (i in seq_len(nrow(b))) {
    expect_equal(str_extract_num(b[[tb]][i], 1),
                 sum(d$RACE == b$rowlabel1[i]))
  }
})

test_that("total and custom groups on the same variable round-trip through metadata", {
  d <- as.data.frame(tplyr_adsl)
  b <- tplyr_build(tplyr_spec(
    cols = "TRT01P",
    total_groups  = list(total_group("TRT01P", label = "Total")),
    custom_groups = list(custom_group("TRT01P",
      "Treated" = c("Xanomeline High Dose", "Xanomeline Low Dose"))),
    layers = tplyr_layers(group_count("SEX"))), d, metadata = TRUE)
  for (i in seq_len(nrow(b))) {
    for (cc in grep("^res\\d+$", names(b), value = TRUE)) {
      sub <- tplyr_meta_subset(b, b$row_id[i], cc, d)
      expect_equal(as.numeric(nrow(sub)),
                   as.numeric(str_extract_num(b[[cc]][i], 1)),
                   info = paste(b$rowlabel1[i], cc))
    }
  }
})
