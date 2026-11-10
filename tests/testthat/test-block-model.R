# Tests for topology classification

test_that("classify_topology detects Consensus", {
  B <- matrix(c(0.8, 0.5, 0.5, 0.8), 2, 2)
  expect_equal(classify_topology(B), "Consensus")
})

test_that("classify_topology detects Bipolar Opposition", {
  B <- matrix(c(0.8, -0.5, -0.5, 0.8), 2, 2)
  expect_equal(classify_topology(B), "Bipolar Opposition")
})

test_that("classify_topology detects Triadic Impasse", {
  B <- matrix(c(0.8, -0.5, -0.4, -0.5, 0.8, -0.3, -0.4, -0.3, 0.8), 3, 3)
  expect_equal(classify_topology(B), "Triadic Impasse")
})

test_that("classify_topology detects Hub-and-Spoke", {
  B <- matrix(c(0.9, -0.6, -0.6, -0.6, 0.8, 0.0, -0.6, 0.0, 0.8), 3, 3)
  expect_equal(classify_topology(B), "Hub-and-Spoke")
})
