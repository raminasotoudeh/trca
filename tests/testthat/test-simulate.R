# Tests for simulation data generators

test_that("sim_bipolar returns correct structure", {
  sim <- sim_bipolar(n_per_faction = 20, p = 10, seed = 1)
  expect_equal(nrow(sim$data), 40)
  expect_equal(ncol(sim$data), 10)
  expect_equal(length(sim$truth), 40)
  expect_equal(sim$truth, rep(1:2, each = 20))
  expect_equal(sim$scenario, "bipolar")
  expect_equal(length(sim$profiles), 2)
})

test_that("sim_consensus returns correct structure", {
  sim <- sim_consensus(n = 30, p = 10, seed = 1)
  expect_equal(nrow(sim$data), 30)
  expect_equal(ncol(sim$data), 10)
  expect_equal(length(sim$truth), 30)
  expect_true(all(sim$truth == 1))
  expect_equal(sim$scenario, "consensus")
})

test_that("sim_triadic_impasse returns correct structure", {
  sim <- sim_triadic_impasse(n_per_faction = 20, items_per_block = 5, seed = 1)
  expect_equal(nrow(sim$data), 60)
  expect_equal(ncol(sim$data), 15)
  expect_equal(sim$truth, rep(1:3, each = 20))
  expect_equal(sim$scenario, "triadic_impasse")
  expect_equal(length(sim$profiles), 3)
})

test_that("sim_hub_and_spoke returns correct structure", {
  sim <- sim_hub_and_spoke(n_per_faction = 20, p = 10, seed = 1)
  expect_equal(nrow(sim$data), 60)
  expect_equal(ncol(sim$data), 10)
  expect_equal(sim$truth, rep(1:3, each = 20))
  expect_equal(sim$scenario, "hub_and_spoke")
  expect_equal(length(sim$profiles), 3)
})

test_that("sim_bipolar is reproducible with same seed", {
  s1 <- sim_bipolar(n_per_faction = 10, seed = 99)
  s2 <- sim_bipolar(n_per_faction = 10, seed = 99)
  expect_equal(s1$data, s2$data)
})

test_that("sim_bipolar varies with different seeds", {
  s1 <- sim_bipolar(n_per_faction = 10, seed = 1)
  s2 <- sim_bipolar(n_per_faction = 10, seed = 2)
  expect_false(identical(s1$data, s2$data))
})

test_that("signal and noise parameters affect data", {
  s_high <- sim_bipolar(n_per_faction = 20, signal = 5.0, noise_sd = 0.1, seed = 1)
  s_low  <- sim_bipolar(n_per_faction = 20, signal = 0.5, noise_sd = 2.0, seed = 1)
  # High signal should produce much higher variance across items
  expect_gt(var(as.vector(s_high$data)), var(as.vector(s_low$data)))
})
