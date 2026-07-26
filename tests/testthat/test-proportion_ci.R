# --- proportion_ci() helper tests ---

test_that("clopper_pearson matches stats::binom.test", {
  for (xn in list(c(12, 40), c(0, 40), c(40, 40), c(3, 17), c(1, 100),
                  c(7, 23))) {
    x <- xn[1]; n <- xn[2]
    ours <- proportion_ci(x, n, "clopper_pearson", 0.95)
    bt <- binom.test(x, n, conf.level = 0.95)$conf.int
    expect_equal(c(ours$lower, ours$upper), as.numeric(bt), tolerance = 1e-8,
                 info = sprintf("x=%d n=%d", x, n))
  }
})

test_that("clopper_pearson matches binom.test at ci_level = 0.90", {
  ours <- proportion_ci(12, 40, "clopper_pearson", 0.90)
  bt <- binom.test(12, 40, conf.level = 0.90)$conf.int
  expect_equal(c(ours$lower, ours$upper), as.numeric(bt), tolerance = 1e-8)
})

test_that("wilson matches stats::prop.test(correct = FALSE)", {
  for (xn in list(c(12, 40), c(3, 17), c(1, 100), c(50, 50), c(0, 20))) {
    x <- xn[1]; n <- xn[2]
    ours <- proportion_ci(x, n, "wilson", 0.95)
    pt <- prop.test(x, n, correct = FALSE, conf.level = 0.95)$conf.int
    expect_equal(c(ours$lower, ours$upper), as.numeric(pt), tolerance = 1e-8,
                 info = sprintf("x=%d n=%d", x, n))
  }
})

test_that("wald matches the closed-form normal approximation", {
  x <- 12; n <- 40
  p <- x / n
  z <- qnorm(0.975)
  se <- sqrt(p * (1 - p) / n)
  ours <- proportion_ci(x, n, "wald", 0.95)
  expect_equal(ours$lower, p - z * se, tolerance = 1e-10)
  expect_equal(ours$upper, p + z * se, tolerance = 1e-10)
})

test_that("agresti_coull matches Wald on the adjusted counts", {
  x <- 12; n <- 40
  z <- qnorm(0.975)
  nt <- n + z^2
  pt <- (x + z^2 / 2) / nt
  se <- sqrt(pt * (1 - pt) / nt)
  ours <- proportion_ci(x, n, "agresti_coull", 0.95)
  expect_equal(ours$lower, pt - z * se, tolerance = 1e-10)
  expect_equal(ours$upper, pt + z * se, tolerance = 1e-10)
})

test_that("jeffreys matches qbeta with the 0.5 shift", {
  x <- 12; n <- 40
  ours <- proportion_ci(x, n, "jeffreys", 0.95)
  expect_equal(ours$lower, qbeta(0.025, x + 0.5, n - x + 0.5), tolerance = 1e-10)
  expect_equal(ours$upper, qbeta(0.975, x + 0.5, n - x + 0.5), tolerance = 1e-10)
})

test_that("jeffreys applies boundary adjustments at x==0 and x==n", {
  lo <- proportion_ci(0, 40, "jeffreys", 0.95)
  expect_equal(lo$lower, 0)
  hi <- proportion_ci(40, 40, "jeffreys", 0.95)
  expect_equal(hi$upper, 1)
})

test_that("edge cases: x==0 lower is 0, x==n upper is 1, n==0 is NA", {
  res <- proportion_ci(c(0, 40, 5), c(0, 40, 40), "clopper_pearson", 0.95)
  # n == 0 -> NA both bounds
  expect_true(is.na(res$lower[1]))
  expect_true(is.na(res$upper[1]))
  # x == n -> upper 1
  expect_equal(res$upper[2], 1)
  # x == 0 (first cell is n==0; test a real x==0 case separately)
  zero <- proportion_ci(0, 40, "clopper_pearson", 0.95)
  expect_equal(zero$lower, 0)
})

test_that("all methods clamp bounds to [0, 1]", {
  for (m in c("clopper_pearson", "wilson", "wald", "agresti_coull",
              "jeffreys")) {
    res <- proportion_ci(c(0, 1, 39, 40), c(40, 40, 40, 40), m, 0.95)
    expect_true(all(res$lower >= 0 & res$lower <= 1))
    expect_true(all(res$upper >= 0 & res$upper <= 1))
  }
})

test_that("proportion_ci is vectorized and length-stable", {
  res <- proportion_ci(c(1, 2, 3, 4), c(10, 20, 30, 40), "wilson", 0.95)
  expect_equal(nrow(res), 4)
  # Compare against element-wise computation
  for (i in 1:4) {
    single <- proportion_ci(c(1, 2, 3, 4)[i], c(10, 20, 30, 40)[i], "wilson",
                            0.95)
    expect_equal(res$lower[i], single$lower)
    expect_equal(res$upper[i], single$upper)
  }
})

test_that("proportion_ci rejects an unknown method", {
  expect_error(proportion_ci(1, 10, "bootstrap"), "should be one of")
})
