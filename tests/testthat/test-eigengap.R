# Tests for eigengap_select_k()

test_that("eigengap selects k=2 for bipolar spectrum", {
  # 1 near-zero eigenvalue, then bulk at ~1
  evals <- c(0.001, 1.0, 1.01, 1.02, 1.03)
  result <- eigengap_select_k(evals)
  expect_equal(result$k, 2)
  expect_equal(result$gap_position, 1)
})

test_that("eigengap selects k=3 for triadic spectrum", {
  # 2 small eigenvalues, then bulk at ~1
  evals <- c(0.25, 0.26, 1.0, 1.01, 1.02)
  result <- eigengap_select_k(evals)
  expect_equal(result$k, 3)
  expect_equal(result$gap_position, 2)
})

test_that("min_k is respected", {
  evals <- c(0.001, 1.0, 1.01, 1.02, 1.03)
  result <- eigengap_select_k(evals, min_k = 3)
  expect_gte(result$k, 3)
})

test_that("max_k is respected", {
  evals <- c(0.01, 0.02, 0.03, 0.04, 1.0, 1.01)
  result <- eigengap_select_k(evals, max_k = 3)
  expect_lte(result$k, 3)
})
