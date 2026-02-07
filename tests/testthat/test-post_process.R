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

test_that("collapse_row_labels merges columns with indentation", {
  df <- data.frame(
    rowlabel1 = c("A", "", "B", ""),
    rowlabel2 = c("", "X", "", "Y"),
    res1 = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
  result <- collapse_row_labels(df)
  expect_true("row_label" %in% names(result))
  expect_false("rowlabel1" %in% names(result))
  expect_equal(result$row_label, c("A", "  X", "B", "  Y"))
})

test_that("collapse_row_labels custom indent", {
  df <- data.frame(
    rowlabel1 = c("A", ""),
    rowlabel2 = c("", "X"),
    res1 = c("1", "2"),
    stringsAsFactors = FALSE
  )
  result <- collapse_row_labels(df, indent = "    ")
  expect_equal(result$row_label, c("A", "    X"))
})

test_that("collapse_row_labels single column", {
  df <- data.frame(
    rowlabel1 = c("A", "B"),
    res1 = c("1", "2"),
    stringsAsFactors = FALSE
  )
  result <- collapse_row_labels(df)
  expect_equal(result$row_label, c("A", "B"))
})

test_that("collapse_row_labels empty data", {
  df <- data.frame(rowlabel1 = character(0), res1 = character(0), stringsAsFactors = FALSE)
  result <- collapse_row_labels(df)
  expect_equal(nrow(result), 0)
})

# --- add_column_headers tests ---

test_that("add_column_headers adds header row from labels", {
  df <- data.frame(
    rowlabel1 = c("X", "Y"),
    res1 = c("1", "2"),
    res2 = c("3", "4"),
    stringsAsFactors = FALSE
  )
  attr(df$res1, "label") <- "Placebo"
  attr(df$res2, "label") <- "Treatment"
  result <- add_column_headers(df)
  expect_equal(nrow(result), 3)
  expect_equal(result$res1[1], "Placebo")
  expect_equal(result$res2[1], "Treatment")
})

test_that("add_column_headers with header_format string", {
  df <- data.frame(
    rowlabel1 = c("X"),
    res1 = c("1"),
    res2 = c("2"),
    stringsAsFactors = FALSE
  )
  result <- add_column_headers(df, header_format = "Variable | Col A | Col B")
  expect_equal(nrow(result), 2)
  expect_equal(result$rowlabel1[1], "Variable")
  expect_equal(result$res1[1], "Col A")
  expect_equal(result$res2[1], "Col B")
})

test_that("add_column_headers substitutes **level** placeholders", {
  df <- data.frame(
    rowlabel1 = c("X"),
    res1 = c("1"),
    stringsAsFactors = FALSE
  )
  attr(df, "header_n") <- data.frame(TRT = "A", .n = 50, stringsAsFactors = FALSE)
  result <- add_column_headers(df, header_format = "Var | **A**")
  expect_equal(result$res1[1], "(N=50)")
})

# --- str_indent_wrap tests ---

test_that("str_indent_wrap wraps text", {
  text <- "This is a long string that should be wrapped at a reasonable width for display"
  result <- str_indent_wrap(text, width = 30)
  expect_true(grepl("\n", result))
})

test_that("str_indent_wrap handles short text", {
  result <- str_indent_wrap("Short", width = 80)
  expect_equal(result, "Short")
})

test_that("str_indent_wrap handles NA", {
  result <- str_indent_wrap(NA_character_)
  expect_true(is.na(result))
})

test_that("str_indent_wrap handles vector input", {
  result <- str_indent_wrap(c("A", "B"), width = 80)
  expect_equal(result, c("A", "B"))
})

# --- apply_conditional_format tests ---

test_that("apply_conditional_format applies function to matching cells", {
  df <- data.frame(
    res1 = c(" 5 (10.0%)", "10 (20.0%)", " 0 ( 0.0%)"),
    stringsAsFactors = FALSE
  )
  result <- apply_conditional_format(
    df, "res1",
    condition_fn = function(x) grepl("^ *0 ", x),
    format_fn = function(x) gsub("[0-9]", "-", x)
  )
  expect_equal(result$res1[1], " 5 (10.0%)")
  expect_true(grepl("-", result$res1[3]))
})

test_that("apply_conditional_format errors on missing column", {
  df <- data.frame(res1 = "a", stringsAsFactors = FALSE)
  expect_error(apply_conditional_format(df, "nonexistent", identity, identity))
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
