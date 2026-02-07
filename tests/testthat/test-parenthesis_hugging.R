# --- Parenthesis hugging tests ---

test_that("uppercase X enables hugging", {
  fmt <- f_str("xx (XX.X%)", "n", "pct")
  # With small pct value, leading space should shift to trailing
  result <- apply_formats(fmt, 5, 9.3)
  # Without hug: " 5 ( 9.3%)"
  # With hug:    " 5 (9.3% )"
  expect_equal(result, " 5 (9.3% )")
})

test_that("lowercase x does not hug", {
  fmt <- f_str("xx (xx.x%)", "n", "pct")
  result <- apply_formats(fmt, 5, 9.3)
  # Standard: " 5 ( 9.3%)"
  expect_equal(result, " 5 ( 9.3%)")
})

test_that("hugging preserves total string width", {
  fmt_hug <- f_str("xx (XX.X%)", "n", "pct")
  fmt_no  <- f_str("xx (xx.x%)", "n", "pct")
  result_hug <- apply_formats(fmt_hug, 5, 9.3)
  result_no  <- apply_formats(fmt_no, 5, 9.3)
  expect_equal(nchar(result_hug), nchar(result_no))
})

test_that("hugging with no leading spaces is a no-op", {
  fmt <- f_str("xx (XX.X%)", "n", "pct")
  # Large values that fill the width — no leading space to shift
  result <- apply_formats(fmt, 99, 99.9)
  expect_equal(result, "99 (99.9%)")
})

test_that("hugging with integer-only format", {
  fmt <- f_str("(XX)", "n")
  result <- apply_formats(fmt, 5)
  # Without hug: "( 5)"
  # With hug:    "(5 )"
  expect_equal(result, "(5 )")
})

test_that("hugging with uppercase A (auto-precision placeholder)", {
  fmt <- f_str("xx (AA.A%)", "n", "pct")
  result <- apply_formats(fmt, 5, 9.3)
  # A is both auto and hug; in non-precision context, works like X
  expect_equal(result, " 5 (9.3% )")
})

test_that("mixed hug and non-hug groups", {
  fmt <- f_str("xx (XX.X)", "n", "pct")
  result <- apply_formats(fmt, 5, 9.3)
  # First group (xx) = no hug, second group (XX.X) = hug
  # " 5 ( 9.3)" without hug on 2nd
  # " 5 (9.3 )" with hug on 2nd
  expect_equal(result, " 5 (9.3 )")
})

test_that("hugging works with vector input", {
  fmt <- f_str("xx (XX.X%)", "n", "pct")
  result <- apply_formats(fmt, c(5, 10, 99), c(9.3, 50.0, 99.9))
  expect_equal(result[1], " 5 (9.3% )")
  expect_equal(result[2], "10 (50.0%)")
  expect_equal(result[3], "99 (99.9%)")
})

test_that("hugging with NA values produces spaces", {
  fmt <- f_str("xx (XX.X%)", "n", "pct")
  result <- apply_formats(fmt, 5, NA)
  # NA formatted as spaces, hugging should still work with all-space number
  expect_equal(nchar(result), nchar(apply_formats(fmt, 5, 9.3)))
})

test_that("hugging with trailing literal containing multiple chars", {
  fmt <- f_str("[XX.X]", "val")
  result <- apply_formats(fmt, 1.5)
  # Without hug: "[ 1.5]"
  # With hug:    "[1.5 ]"
  expect_equal(result, "[1.5 ]")
})

test_that("hug flag is correctly parsed for X", {
  parsed <- tplyr2:::parse_format_part("XX")
  expect_true(parsed$hug)
  expect_false(parsed$auto)
})

test_that("hug flag is correctly parsed for A", {
  parsed <- tplyr2:::parse_format_part("AA")
  expect_true(parsed$hug)
  expect_true(parsed$auto)
})
