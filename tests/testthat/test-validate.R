
# --- denoms_by must name the layer's grouping variables (#77) ---

test_that("denoms_by naming a non-grouping variable errors (#77)", {
  # The denominator merges join on intersect(denom_group, names(x)); an unknown
  # variable silently shrank the key set, so the merge either multiplied rows
  # or attached another group's denominator.
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("AGEGR1", by = "SEX",
                settings = layer_settings(denoms_by = c("TRT01P", "RACE")))))
  expect_error(tplyr_build(spec, tplyr_adsl),
               "denoms_by names variable\\(s\\) the layer does not group by: RACE")
  expect_error(tplyr_build(spec, tplyr_adsl), "Valid values.*TRT01P, SEX, AGEGR1")
})

test_that("denoms_by accepts cols, by vars, and the count target (#77)", {
  ok <- function(db) tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_count("AGEGR1", by = "SEX", settings = layer_settings(denoms_by = db))))
  expect_s3_class(tplyr_build(ok("TRT01P"), tplyr_adsl), "data.frame")
  expect_s3_class(tplyr_build(ok(c("TRT01P", "SEX")), tplyr_adsl), "data.frame")
  expect_s3_class(tplyr_build(ok(c("TRT01P", "AGEGR1")), tplyr_adsl), "data.frame")
})

test_that("a desc layer rejects its target variable in denoms_by (#77)", {
  # Desc summarizes the target rather than grouping by it, so naming it would
  # be narrowed away and attach the wrong denominator.
  spec <- tplyr_spec(cols = "TRT01P", layers = tplyr_layers(
    group_desc("AGE", settings = layer_settings(denoms_by = c("TRT01P", "AGE")))))
  expect_error(tplyr_build(spec, tplyr_adsl), "does not group by: AGE")
})

test_that("a shift layer accepts its row and column variables (#77)", {
  spec <- tplyr_spec(cols = "TRTA", layers = tplyr_layers(
    group_shift(c(row = "AESEV", column = "AESER"),
                settings = layer_settings(denoms_by = c("TRTA", "AESEV")))))
  expect_s3_class(tplyr_build(spec, tplyr_adae), "data.frame")
})

test_that("nested denoms_by validates every level of the list form (#77)", {
  good <- tplyr_spec(cols = "TRTA", layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
                settings = layer_settings(denoms_by = list("TRTA", "TRTA")))))
  expect_s3_class(tplyr_build(good, tplyr_adae), "data.frame")

  bad <- tplyr_spec(cols = "TRTA", layers = tplyr_layers(
    group_count(c("AEBODSYS", "AEDECOD"),
                settings = layer_settings(denoms_by = list("TRTA", "NOPE")))))
  expect_error(tplyr_build(bad, tplyr_adae), "does not group by: NOPE")
})

test_that("a NULL denoms_by is unaffected (#77)", {
  spec <- tplyr_spec(cols = "TRT01P",
                     layers = tplyr_layers(group_count("AGEGR1", by = "SEX")))
  expect_s3_class(tplyr_build(spec, tplyr_adsl), "data.frame")
})
