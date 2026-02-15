# --- apply_row_masks tests ---

test_that("apply_row_masks blanks repeated labels", {
  df <- data.frame(
    rowlabel1 = c("A", "A", "B", "B"),
    res1 = c("1", "2", "3", "4"),
    ord_layer_index = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  result <- apply_row_masks(df)
  expect_equal(result$rowlabel1, c("A", "", "B", ""))
})

test_that("apply_row_masks respects layer boundaries", {
  df <- data.frame(
    rowlabel1 = c("A", "A", "A", "A"),
    res1 = c("1", "2", "3", "4"),
    ord_layer_index = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
  result <- apply_row_masks(df)
  # Layer 2 starts fresh, "A" should not be blanked at row 3

  expect_equal(result$rowlabel1, c("A", "", "A", ""))
})

test_that("apply_row_masks with row_breaks inserts blank rows", {
  df <- data.frame(
    rowlabel1 = c("X", "Y"),
    res1 = c("1", "2"),
    ord_layer_index = c(1, 2),
    stringsAsFactors = FALSE
  )
  result <- apply_row_masks(df, row_breaks = TRUE)
  expect_equal(nrow(result), 3)
  expect_equal(result$rowlabel1[2], "")
})

test_that("apply_row_masks handles empty data", {
  df <- data.frame(rowlabel1 = character(0), res1 = character(0), stringsAsFactors = FALSE)
  result <- apply_row_masks(df)
  expect_equal(nrow(result), 0)
})

test_that("apply_row_masks handles multiple label columns", {
  df <- data.frame(
    rowlabel1 = c("A", "A", "A", "B"),
    rowlabel2 = c("X", "X", "Y", "X"),
    res1 = c("1", "2", "3", "4"),
    ord_layer_index = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  result <- apply_row_masks(df)
  expect_equal(result$rowlabel1, c("A", "", "", "B"))
  expect_equal(result$rowlabel2, c("X", "", "Y", "X"))
})

# --- collapse_row_labels tests ---

test_that("collapse_row_labels validates inputs", {
  expect_error(collapse_row_labels(1, "a", "b"), "must be a data frame")
  df <- data.frame(a = 1, b = 2)
  expect_error(collapse_row_labels(df, "a", "b", indent = 1), "must be a character string")
  expect_error(collapse_row_labels(df, "a"), "Must have two or more columns")
  expect_error(collapse_row_labels(df, "a", "z"), "missing from x")
})

test_that("collapse_row_labels collapses two columns with stub rows", {
  df <- data.frame(
    row_label1 = c("A", "A", "B", "B"),
    row_label2 = c("C", "D", "E", "F"),
    var1 = 1:4,
    stringsAsFactors = FALSE
  )
  result <- collapse_row_labels(df, "row_label1", "row_label2")

  # Should have stub rows inserted: A, C, D, B, E, F = 6 rows
  expect_equal(nrow(result), 6)
  expect_true("row_label" %in% names(result))
  expect_false("row_label1" %in% names(result))
  expect_false("row_label2" %in% names(result))

  # Check indentation: stubs at level 1 (no indent), data at level 2 (indented)
  expect_equal(result$row_label[1], "A")
  expect_equal(result$row_label[2], "  C")
  expect_equal(result$row_label[3], "  D")
  expect_equal(result$row_label[4], "B")
  expect_equal(result$row_label[5], "  E")
  expect_equal(result$row_label[6], "  F")
})

test_that("collapse_row_labels handles deeper nesting", {
  df <- data.frame(
    rl1 = c("A", "A", "A", "B", "B"),
    rl2 = c("C", "C", "D", "E", "E"),
    rl3 = c("G", "H", "I", "J", "K"),
    var1 = 1:5,
    stringsAsFactors = FALSE
  )
  result <- collapse_row_labels(df, "rl1", "rl2", "rl3")

  # First group (A, C): stubs A + C, then data G, H
  expect_equal(result$row_label[1], "A")
  expect_equal(result$row_label[2], "  C")
  expect_equal(result$row_label[3], "    G")
  expect_equal(result$row_label[4], "    H")
})

test_that("collapse_row_labels custom indent and target_col", {
  df <- data.frame(
    row_label1 = c("A", "A", "B"),
    row_label2 = c("X", "Y", "Z"),
    var1 = 1:3,
    stringsAsFactors = FALSE
  )
  result <- collapse_row_labels(df, "row_label1", "row_label2",
                                indent = "    ", target_col = "rl")

  expect_true("rl" %in% names(result))
  expect_equal(result$rl[1], "A")
  expect_equal(result$rl[2], "    X")
  expect_equal(result$rl[3], "    Y")
  expect_equal(result$rl[4], "B")
  expect_equal(result$rl[5], "    Z")
})

test_that("collapse_row_labels preserves data columns", {
  df <- data.frame(
    row_label1 = c("A", "A"),
    row_label2 = c("X", "Y"),
    val = c(10, 20),
    stringsAsFactors = FALSE
  )
  result <- collapse_row_labels(df, "row_label1", "row_label2")

  # Stub row should have NA for numeric columns
  expect_true(is.na(result$val[1]))
  # Data rows should retain values
  expect_equal(result$val[2], 10)
  expect_equal(result$val[3], 20)
})

# --- str_indent_wrap tests ---

test_that("str_indent_wrap errors properly", {
  expect_error(str_indent_wrap(1), regexp = "x must be a character vector")
})

test_that("str_indent_wrap wraps text with hyphenation and indentation", {
  text1 <- c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS")
  text2 <- c("RENAL AND URINARY DISORDERS", "\tNEPHROLITHIASIS")
  text3 <- c("RENAL AND URINARY DISORDERS", "\t\tNEPHROLITHIASIS")

  expect_equal(
    str_indent_wrap(text1, width = 8),
    c("RENAL\nAND\nURINARY\nDISORDE-\nRS", "   NEPHROL-\n   ITHIASI-\n   S")
  )

  expect_equal(
    str_indent_wrap(text2, width = 9, tab_width = 4),
    c("RENAL AND\nURINARY\nDISORDER-\nS", "    NEPHROLI-\n    THIASIS")
  )

  expect_equal(
    str_indent_wrap(text3, width = 9, tab_width = 2),
    c("RENAL AND\nURINARY\nDISORDER-\nS", "    NEPHROLI-\n    THIASIS")
  )
})

test_that("str_indent_wrap handles short text", {
  result <- str_indent_wrap("Short", width = 80)
  expect_equal(result, "Short")
})

test_that("str_indent_wrap handles vector input", {
  result <- str_indent_wrap(c("A", "B"), width = 80)
  expect_equal(result, c("A", "B"))
})

# --- apply_conditional_format tests ---

test_that("apply_conditional_format validates inputs", {
  expect_error(apply_conditional_format(1, 1, x == 0, "a"), "must be a character")
  expect_error(apply_conditional_format("a", 1.5, x == 0, "b"), "must be an integer")
  expect_error(apply_conditional_format("a", 1, x == 0, 1), "must be a string")
  expect_error(apply_conditional_format("a", 1, y == 0, "b"), "variable name `x`")
  expect_error(apply_conditional_format("a", 1, x == 0, "b", full_string = "yes"), "must be bool")
})

test_that("apply_conditional_format replaces full string conditionally", {
  string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")
  result <- apply_conditional_format(string, 2, x == 0, " 0        ", full_string = TRUE)
  expect_equal(result, c(" 0        ", " 8  (9.3%)", "78 (90.7%)"))
})

test_that("apply_conditional_format replaces within format group", {
  string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")
  result <- apply_conditional_format(string, 2, x < 1, "(<1%)")
  expect_equal(result[1], " 0   (<1%)")
  expect_equal(result[2], " 8  (9.3%)")
  expect_equal(result[3], "78 (90.7%)")
})

test_that("apply_conditional_format handles compound conditions", {
  string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")
  result <- apply_conditional_format(string, 1, x > 7 & x < 13, "**")
  # Only the second element should be replaced (x = 8)
  expect_equal(result[1], " 0  (0.0%)")
  expect_equal(result[3], "78 (90.7%)")
  expect_true(grepl("\\*\\*", result[2]))
})

test_that("apply_conditional_format warns on long replacement", {
  string <- c(" 0  (0.0%)")
  expect_warning(
    apply_conditional_format(string, 1, x == 0, "very long replacement"),
    "longer"
  )
})

test_that("apply_conditional_format handles missing format groups gracefully", {
  string <- c(" 5 (10.0%)", "text only")
  # format_group 2 doesn't exist in "text only" — should be treated as NA (no replacement)
  result <- apply_conditional_format(string, 2, x == 10, "XX", full_string = TRUE)
  expect_equal(result[2], "text only")
})

# --- str_extract_num tests ---

test_that("str_extract_num extracts first number", {
  expect_equal(str_extract_num(" 5 (10.3%)", 1), 5)
  expect_equal(str_extract_num(" 5 (10.3%)", 2), 10.3)
})

test_that("str_extract_num returns NA for missing index", {
  expect_true(is.na(str_extract_num("5", 2)))
})

test_that("str_extract_num handles NA input", {
  expect_true(is.na(str_extract_num(NA_character_)))
})

test_that("str_extract_num handles vector input", {
  result <- str_extract_num(c(" 5 (10.0%)", "10 (20.0%)"), 1)
  expect_equal(result, c(5, 10))
})

test_that("str_extract_num handles negative numbers", {
  expect_equal(str_extract_num("-3.5 (1.2, 5.8)", 1), -3.5)
})

# --- replace_leading_whitespace tests ---

test_that("replace_leading_whitespace replaces spaces", {
  result <- replace_leading_whitespace("   hello")
  expect_equal(result, "\u00a0\u00a0\u00a0hello")
})

test_that("replace_leading_whitespace no leading spaces", {
  result <- replace_leading_whitespace("hello")
  expect_equal(result, "hello")
})

test_that("replace_leading_whitespace handles NA", {
  result <- replace_leading_whitespace(NA_character_)
  expect_true(is.na(result))
})

test_that("replace_leading_whitespace custom replacement", {
  result <- replace_leading_whitespace("  hi", replace_with = "&nbsp;")
  expect_equal(result, "&nbsp;&nbsp;hi")
})

test_that("replace_leading_whitespace vector input", {
  result <- replace_leading_whitespace(c("  a", " b", "c"))
  expect_equal(result, c("\u00a0\u00a0a", "\u00a0b", "c"))
})
