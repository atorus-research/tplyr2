test_that("f_str creates format string objects", {
  fmt <- f_str("xxx", "n")
  expect_s3_class(fmt, "tplyr_f_str")
  expect_equal(fmt$vars, "n")
  expect_equal(fmt$format_string, "xxx")
})

test_that("f_str validates variable count matches format groups", {
  expect_error(
    f_str("xxx", "n", "pct"),
    "1 format group.*2 variable"
  )
  expect_error(
    f_str("xx.x (xx.xx)", "mean"),
    "2 format group.*1 variable"
  )
})

test_that("f_str parses multi-group format strings", {
  fmt <- f_str("xx (xx.x%)", "n", "pct")
  expect_equal(fmt$vars, c("n", "pct"))
  expect_length(fmt$parsed$groups, 2)
})

test_that("apply_formats formats integers correctly", {
  fmt <- f_str("xxx", "n")
  result <- apply_formats(fmt, c(86, 5, 123))
  expect_equal(trimws(result), c("86", "5", "123"))
  # Check width padding
  expect_equal(nchar(result), c(3, 3, 3))
})

test_that("apply_formats formats decimals correctly", {
  fmt <- f_str("xx.x", "mean")
  result <- apply_formats(fmt, c(75.209, 3.1415))
  expect_equal(trimws(result), c("75.2", "3.1"))
})

test_that("apply_formats handles multi-variable formatting", {
  fmt <- f_str("xx (xx.x%)", "n", "pct")
  result <- apply_formats(fmt, c(53, 8), c(61.63, 9.30))
  expect_equal(trimws(result[1]), "53 (61.6%)")
  expect_equal(trimws(result[2]), "8 ( 9.3%)")
})

test_that("apply_formats handles NA values", {
  fmt <- f_str("xx.xx", "sd")
  result <- apply_formats(fmt, c(8.59, NA_real_))
  expect_equal(nchar(result[2]), nchar(result[1]))
  expect_match(result[2], "^\\s+$")
})

test_that("apply_formats na= renders NA as an empty string", {
  # na = "" -> truly empty cell (nchar 0), not blank-width fill
  res <- apply_formats(f_str("xx.x", "x"), NA_real_, na = "")
  expect_identical(res, "")
  expect_equal(nchar(res), 0L)
})

test_that("apply_formats na= substitutes only all-NA cells in a vector", {
  res <- apply_formats(f_str("xx.x", "x"), c(2.3, NA, 12.7), na = "")
  expect_identical(res, c(" 2.3", "", "12.7"))
})

test_that("apply_formats na= accepts a non-empty replacement string", {
  res <- apply_formats(f_str("xx.x", "x"), c(NA_real_, 2.3), na = "NE")
  expect_identical(res, c("NE", " 2.3"))
})

test_that("apply_formats width= pads the token to a fixed total width", {
  res <- apply_formats(f_str("xxxx.xxxx", "x"), 2.3291, width = 12)
  expect_identical(res, "   2.3291   ")
  expect_equal(nchar(res), 12L)
})

test_that("apply_formats width= respects pad side", {
  res <- apply_formats(f_str("xxxx.xxxx", "x"), 2.3291, width = 12, pad = "left")
  expect_identical(res, "      2.3291")
  expect_equal(nchar(res), 12L)
})

test_that("apply_formats na= wins over width= (empty cell is not padded)", {
  res <- apply_formats(f_str("xx.x", "x"), NA_real_, na = "", width = 12)
  expect_identical(res, "")
  expect_equal(nchar(res), 0L)
})

test_that("apply_formats na= only fires when all format-group inputs are NA", {
  fmt <- f_str("xx (xx.x%)", "n", "pct")
  res <- apply_formats(fmt, c(NA_real_, 5), c(NA_real_, 50), na = "")
  expect_identical(res[1], "")            # both inputs NA -> na
  expect_identical(res[2], " 5 (50.0%)") # neither NA -> formatted normally
})

test_that("apply_formats NULL na/width defaults preserve blank-width behavior", {
  fmt <- f_str("xx.xx", "sd")
  expect_identical(
    apply_formats(fmt, c(8.59, NA_real_)),
    apply_formats(fmt, c(8.59, NA_real_), na = NULL, width = NULL)
  )
})

test_that("format_number_vec handles zero decimal width", {
  group <- list(int = list(width = 3L, auto = FALSE, offset = 0L, hug = FALSE),
                dec = list(width = 0L, auto = FALSE, offset = 0L, hug = FALSE),
                has_decimal = FALSE)
  result <- tplyr2:::format_number_vec(c(5, 86, 123), group)
  expect_equal(trimws(result), c("5", "86", "123"))
})

test_that("parse_format_string identifies groups and literals", {
  parsed <- tplyr2:::parse_format_string("xx (xx.x%)")
  expect_length(parsed$groups, 2)
  expect_equal(parsed$literals[2], " (")
  expect_equal(parsed$literals[3], "%)")
})

test_that("print.tplyr_f_str works", {
  fmt <- f_str("xx.x", "mean")
  expect_output(print(fmt), "tplyr format string")
  expect_output(print(fmt), "mean")
})

# --- IBM Rounding Tests ---

test_that("tplyr_round uses banker's rounding by default", {
  # R's default: round half to even
  expect_equal(tplyr2:::tplyr_round(2.5, 0), round(2.5, 0))
  expect_equal(tplyr2:::tplyr_round(3.5, 0), round(3.5, 0))
  expect_equal(tplyr2:::tplyr_round(1.225, 2), round(1.225, 2))
})

test_that("IBM rounding rounds half away from zero", {
  withr::with_options(list(tplyr2.IBMRounding = TRUE), {
    expect_equal(tplyr2:::tplyr_round(2.5, 0), 3)
    expect_equal(tplyr2:::tplyr_round(3.5, 0), 4)
    expect_equal(tplyr2:::tplyr_round(0.5, 0), 1)
  })
})

test_that("IBM rounding handles negative numbers", {
  withr::with_options(list(tplyr2.IBMRounding = TRUE), {
    expect_equal(tplyr2:::tplyr_round(-2.5, 0), -3)
    expect_equal(tplyr2:::tplyr_round(-3.5, 0), -4)
    expect_equal(tplyr2:::tplyr_round(-0.5, 0), -1)
  })
})

test_that("IBM rounding works with decimal places", {
  withr::with_options(list(tplyr2.IBMRounding = TRUE), {
    expect_equal(tplyr2:::tplyr_round(1.225, 2), 1.23)
    expect_equal(tplyr2:::tplyr_round(1.235, 2), 1.24)
    expect_equal(tplyr2:::tplyr_round(1.245, 2), 1.25)
  })
})

test_that("IBM rounding integrates with format_number_vec", {
  group <- list(
    int = list(width = 2L, auto = FALSE, offset = 0L, hug = FALSE),
    dec = list(width = 0L, auto = FALSE, offset = 0L, hug = FALSE),
    has_decimal = FALSE
  )
  # 2.5 rounds to 3 with IBM, 2 with banker's
  withr::with_options(list(tplyr2.IBMRounding = TRUE), {
    result <- tplyr2:::format_number_vec(c(2.5, 3.5), group)
    expect_equal(trimws(result), c("3", "4"))
  })
  # Default banker's rounding: 2.5 -> 2, 3.5 -> 4
  result_default <- tplyr2:::format_number_vec(c(2.5, 3.5), group)
  expect_equal(trimws(result_default), c("2", "4"))
})

# Issue #29: a value rounding to negative zero displays without the sign
test_that("negative zero is normalized to zero in formatted output", {
  fmt <- f_str("xx.x", "mean")
  expect_equal(trimws(apply_formats(fmt, -0.005)), "0.0")
  expect_equal(trimws(apply_formats(fmt, -0.04)), "0.0")   # rounds to -0.0
  expect_equal(trimws(apply_formats(fmt, 0.0)), "0.0")
  # integer format
  fmt_i <- f_str("xx", "n")
  expect_equal(trimws(apply_formats(fmt_i, -0.3)), "0")
})

test_that("group_desc mean of a tiny negative value shows 0.0 not -0.0", {
  d <- data.frame(TRT = "A", CHG = c(-0.04, 0.02, -0.01, 0.01))  # mean = -0.005
  b <- tplyr_build(tplyr_spec(cols = "TRT", layers = tplyr_layers(
    group_desc("CHG", settings = layer_settings(
      format_strings = list(Mean = f_str("xx.x", "mean")))))), d)
  expect_equal(trimws(b$res1[b$rowlabel1 == "Mean"]), "0.0")
})
