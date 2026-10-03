test_that("entropy has expected values for simple distributions", {
  expect_equal(
    entropy(rep("a", 10)),
    0
  )

  expect_equal(
    entropy(c("a", "b"), base = 2),
    1
  )

  expect_equal(
    entropy(c("a", "a", "b", "b"), base = 2),
    1
  )
})


test_that("richness counts distinct observed values", {
  expect_equal(
    richness(c("a", "a", "b", "c", "c")),
    3
  )
})


test_that("move summaries operate column-wise", {
  moves <- matrix(
    c(
      "a", "a", "b", "b",
      "x", "y", "x", "y"
    ),
    nrow = 4,
    ncol = 2
  )

  expect_equal(
    move_richness(moves),
    c(2, 2)
  )

  expect_equal(
    move_entropy(moves, base = 2),
    c(1, 1)
  )
})


test_that("Jensen-Shannon divergence is zero for identical samples", {
  x <- c("a", "a", "b", "c", "c")

  expect_equal(
    as.numeric(calc_js_divergence(x, x)),
    0,
    tolerance = 1e-12
  )
})
